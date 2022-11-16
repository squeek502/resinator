const std = @import("std");
const Allocator = std.mem.Allocator;
const UncheckedSliceWriter = @import("utils.zig").UncheckedSliceWriter;
const parseQuotedAsciiString = @import("literals.zig").parseQuotedAsciiString;

pub const ParseLineCommandsResult = struct {
    result: []u8,
    mappings: SourceMappings,
};

const CurrentMapping = struct {
    line_num: usize = 1,
    filename: std.ArrayListUnmanaged(u8) = .{},
    pending: bool = true,
    ignore_contents: bool = false,
};

/// Parses and removes #line commands as well as all source code that is within a file
/// with .c or .h extensions.
///
/// > RC treats files with the .c and .h extensions in a special manner. It
/// > assumes that a file with one of these extensions does not contain
/// > resources. If a file has the .c or .h file name extension, RC ignores all
/// > lines in the file except the preprocessor directives. Therefore, to
/// > include a file that contains resources in another resource script, give
/// > the file to be included an extension other than .c or .h.
/// from https://learn.microsoft.com/en-us/windows/win32/menurc/preprocessor-directives
///
/// Returns a slice of `buf` with the aforementioned stuff removed as well as a mapping
/// between the lines and their correpsonding lines in their original files.
///
/// `buf` must be at least as long as `source`
/// In-place transformation is supported (i.e. `source` and `buf` can be the same slice)
pub fn parseAndRemoveLineCommands(allocator: Allocator, source: []const u8, buf: []u8) !ParseLineCommandsResult {
    var parse_result = ParseLineCommandsResult{
        .result = undefined,
        .mappings = .{},
    };
    errdefer parse_result.mappings.deinit(allocator);

    var current_mapping: CurrentMapping = .{};
    defer current_mapping.filename.deinit(allocator);

    std.debug.assert(buf.len >= source.len);
    var result = UncheckedSliceWriter{ .slice = buf };
    const State = enum {
        line_start,
        preprocessor,
        non_preprocessor,
    };
    var state: State = .line_start;
    var index: usize = 0;
    var pending_start: ?usize = null;
    var preprocessor_start: usize = 0;
    var line_number: usize = 1;
    while (index < source.len) : (index += 1) {
        const c = source[index];
        switch (state) {
            .line_start => switch (c) {
                '#' => {
                    preprocessor_start = index;
                    state = .preprocessor;
                    if (pending_start == null) {
                        pending_start = index;
                    }
                },
                '\n' => {
                    try handleLineEnd(allocator, line_number, &parse_result.mappings, &current_mapping);
                    if (!current_mapping.ignore_contents) {
                        result.write(c);
                        line_number += 1;
                    }
                    pending_start = null;
                },
                ' ', '\t', '\x0b', '\x0c' => {
                    if (pending_start == null) {
                        pending_start = index;
                    }
                },
                else => {
                    if (pending_start != null) {
                        if (!current_mapping.ignore_contents) {
                            result.writeSlice(source[pending_start.? .. index + 1]);
                        }
                        pending_start = null;
                        continue;
                    }
                    if (!current_mapping.ignore_contents) {
                        result.write(c);
                    }
                },
            },
            .preprocessor => switch (c) {
                '\n' => {
                    // Now that we have the full line we can decide what to do with it
                    const preprocessor_str = source[preprocessor_start..index];
                    if (std.mem.startsWith(u8, preprocessor_str, "#line")) {
                        try handleLineCommand(allocator, preprocessor_str, &current_mapping);
                    } else {
                        try handleLineEnd(allocator, line_number, &parse_result.mappings, &current_mapping);
                        if (!current_mapping.ignore_contents) {
                            result.writeSlice(source[pending_start.? .. index + 1]);
                            line_number += 1;
                        }
                    }
                    state = .line_start;
                    pending_start = null;
                },
                else => {},
            },
            .non_preprocessor => switch (c) {
                '\n' => {
                    try handleLineEnd(allocator, line_number, &parse_result.mappings, &current_mapping);
                    if (!current_mapping.ignore_contents) {
                        result.write(c);
                        line_number += 1;
                    }
                    state = .line_start;
                    pending_start = null;
                },
                else => {
                    if (!current_mapping.ignore_contents) {
                        result.write(c);
                    }
                },
            },
        }
    } else {
        switch (state) {
            .line_start, .non_preprocessor => {},
            .preprocessor => {
                // Now that we have the full line we can decide what to do with it
                const preprocessor_str = source[preprocessor_start..index];
                if (std.mem.startsWith(u8, preprocessor_str, "#line")) {
                    try handleLineCommand(allocator, preprocessor_str, &current_mapping);
                } else {
                    try handleLineEnd(allocator, line_number, &parse_result.mappings, &current_mapping);
                    if (!current_mapping.ignore_contents) {
                        result.writeSlice(source[pending_start.?..index]);
                    }
                }
            },
        }
    }

    // TODO: This feels hacky and bad
    // if (current_mapping.pending) {
    //     try handleLineEnd(allocator, line_number, &parse_result.mappings, &current_mapping);
    // }

    parse_result.result = result.getWritten();

    // Remove whitespace from the end of the result. This avoids issues when the
    // preprocessor adds a newline to the end of the file, since then the
    // post-preprocessed source could have more lines than the corresponding input source and
    // the inserted line can't be mapped to any lines in the original file.
    // There's no way that whitespace at the end of a file can affect the parsing
    // of the RC script so this is okay to do unconditionally.
    // TODO: There might be a better way around this
    while (parse_result.result.len > 0 and std.ascii.isWhitespace(parse_result.result[parse_result.result.len - 1])) {
        parse_result.result.len -= 1;
    }

    return parse_result;
}

pub fn handleLineEnd(allocator: Allocator, post_processed_line_number: usize, mapping: *SourceMappings, current_mapping: *CurrentMapping) !void {
    const filename_offset = try mapping.files.put(allocator, current_mapping.filename.items);

    try mapping.set(allocator, post_processed_line_number, .{
        .start_line = current_mapping.line_num,
        .end_line = current_mapping.line_num,
        .filename_offset = filename_offset,
    });

    current_mapping.line_num += 1;
    current_mapping.pending = false;
}

pub fn handleLineCommand(allocator: Allocator, line_command: []const u8, current_mapping: *CurrentMapping) !void {
    var tokenizer = std.mem.tokenize(u8, line_command, " \r");
    _ = tokenizer.next(); // #line
    const linenum_str = tokenizer.next() orelse return;
    const linenum = std.fmt.parseUnsigned(usize, linenum_str, 10) catch return;

    // TODO handle edge-cases around string literals, garbage after the string literal, etc
    const filename_literal = tokenizer.rest();
    std.debug.assert(filename_literal[0] == '"' and filename_literal[filename_literal.len - 1] == '"');
    // TODO might need to use a more general C-style string parser
    const filename = try parseQuotedAsciiString(allocator, filename_literal, 0);
    defer allocator.free(filename);

    current_mapping.line_num = linenum;
    current_mapping.filename.clearRetainingCapacity();
    try current_mapping.filename.appendSlice(allocator, filename);
    current_mapping.pending = true;
    current_mapping.ignore_contents = std.ascii.endsWithIgnoreCase(filename, ".c") or std.ascii.endsWithIgnoreCase(filename, ".h");
}

pub fn parseAndRemoveLineCommandsAlloc(allocator: Allocator, source: []const u8) !ParseLineCommandsResult {
    var buf = try allocator.alloc(u8, source.len);
    var result = try parseAndRemoveLineCommands(allocator, source, buf);
    result.result = allocator.shrink(buf, result.result.len);
    return result;
}

pub const SourceMappings = struct {
    /// line number -> span where the index is (line number - 1)
    mapping: std.ArrayListUnmanaged(SourceSpan) = .{},
    files: StringTable = .{},

    pub const SourceSpan = struct {
        start_line: usize,
        end_line: usize,
        filename_offset: u32,
    };

    pub fn deinit(self: *SourceMappings, allocator: Allocator) void {
        self.files.deinit(allocator);
        self.mapping.deinit(allocator);
    }

    pub fn set(self: *SourceMappings, allocator: Allocator, line_num: usize, span: SourceSpan) !void {
        var ptr = try self.expandAndGet(allocator, line_num);
        ptr.* = span;
    }

    pub fn has(self: *SourceMappings, line_num: usize) bool {
        return self.mapping.items.len >= line_num;
    }

    /// Note: `line_num` is 1-indexed
    pub fn get(self: SourceMappings, line_num: usize) SourceSpan {
        return self.mapping.items[line_num - 1];
    }

    pub fn getPtr(self: SourceMappings, line_num: usize) *SourceSpan {
        return &self.mapping.items[line_num - 1];
    }

    /// Expands the number of lines in the mapping to include the requested
    /// line number (if necessary) and returns a pointer to the value at that
    /// line number.
    ///
    /// Note: `line_num` is 1-indexed
    pub fn expandAndGet(self: *SourceMappings, allocator: Allocator, line_num: usize) !*SourceSpan {
        try self.mapping.resize(allocator, line_num);
        return &self.mapping.items[line_num - 1];
    }
};

/// Same thing as StringTable in Zig's src/Wasm.zig
pub const StringTable = struct {
    data: std.ArrayListUnmanaged(u8) = .{},
    map: std.HashMapUnmanaged(u32, void, std.hash_map.StringIndexContext, std.hash_map.default_max_load_percentage) = .{},

    pub fn deinit(self: *StringTable, allocator: Allocator) void {
        self.data.deinit(allocator);
        self.map.deinit(allocator);
    }

    pub fn put(self: *StringTable, allocator: Allocator, value: []const u8) !u32 {
        const result = try self.map.getOrPutContextAdapted(
            allocator,
            value,
            std.hash_map.StringIndexAdapter{ .bytes = &self.data },
            .{ .bytes = &self.data },
        );
        if (result.found_existing) {
            return result.key_ptr.*;
        }

        try self.data.ensureUnusedCapacity(allocator, value.len + 1);
        const offset = @intCast(u32, self.data.items.len);

        self.data.appendSliceAssumeCapacity(value);
        self.data.appendAssumeCapacity(0);

        result.key_ptr.* = offset;

        return offset;
    }

    pub fn get(self: StringTable, offset: u32) []const u8 {
        std.debug.assert(offset < self.data.items.len);
        return std.mem.sliceTo(@ptrCast([*:0]const u8, self.data.items.ptr + offset), 0);
    }

    pub fn getOffset(self: *StringTable, value: []const u8) ?u32 {
        return self.map.getKeyAdapted(
            value,
            std.hash_map.StringIndexAdapter{ .bytes = &self.data },
        );
    }
};

const ExpectedSourceSpan = struct {
    start_line: usize,
    end_line: usize,
    filename: []const u8,
};

fn testParseAndRemoveLineCommands(
    expected: []const u8,
    comptime expected_spans: []const ExpectedSourceSpan,
    source: []const u8,
) !void {
    var results = try parseAndRemoveLineCommandsAlloc(std.testing.allocator, source);
    defer std.testing.allocator.free(results.result);
    defer results.mappings.deinit(std.testing.allocator);

    try std.testing.expectEqualStrings(expected, results.result);

    expectEqualMappings(expected_spans, results.mappings) catch |err| {
        std.debug.print("\nexpected mappings:\n", .{});
        for (expected_spans) |span, i| {
            const line_num = i + 1;
            std.debug.print("{}: {s}:{}-{}\n", .{ line_num, span.filename, span.start_line, span.end_line });
        }
        std.debug.print("\nactual mappings:\n", .{});
        for (results.mappings.mapping.items) |span, i| {
            const line_num = i + 1;
            const filename = results.mappings.files.get(span.filename_offset);
            std.debug.print("{}: {s}:{}-{}\n", .{ line_num, filename, span.start_line, span.end_line });
        }
        std.debug.print("\n", .{});
        return err;
    };
}

fn expectEqualMappings(expected_spans: []const ExpectedSourceSpan, mappings: SourceMappings) !void {
    try std.testing.expectEqual(expected_spans.len, mappings.mapping.items.len);
    for (expected_spans) |expected_span, i| {
        const line_num = i + 1;
        const span = mappings.get(line_num);
        const filename = mappings.files.get(span.filename_offset);
        try std.testing.expectEqual(expected_span.start_line, span.start_line);
        try std.testing.expectEqual(expected_span.end_line, span.end_line);
        try std.testing.expectEqualStrings(expected_span.filename, filename);
    }
}

test "basic" {
    try testParseAndRemoveLineCommands("", &[_]ExpectedSourceSpan{}, "#line 1 \"blah.rc\"");
}

test "only removes line commands" {
    try testParseAndRemoveLineCommands(
        \\#pragma code_page(65001)
    , &[_]ExpectedSourceSpan{
        .{ .start_line = 1, .end_line = 1, .filename = "blah.rc" },
    },
        \\#line 1 "blah.rc"
        \\#pragma code_page(65001)
    );
}

test "example" {
    try testParseAndRemoveLineCommands(
        \\
        \\included RCDATA {"hello"}
    , &[_]ExpectedSourceSpan{
        .{ .start_line = 1, .end_line = 1, .filename = "./included.rc" },
        .{ .start_line = 2, .end_line = 2, .filename = "./included.rc" },
    },
        \\#line 1 "rcdata.rc"
        \\#line 1 "<built-in>"
        \\#line 1 "<built-in>"
        \\#line 355 "<built-in>"
        \\#line 1 "<command line>"
        \\#line 1 "<built-in>"
        \\#line 1 "rcdata.rc"
        \\#line 1 "./header.h"
        \\
        \\
        \\2 RCDATA {"blah"}
        \\
        \\
        \\#line 1 "./included.rc"
        \\
        \\included RCDATA {"hello"}
        \\#line 7 "./header.h"
        \\#line 1 "rcdata.rc"
    );
}

test "in place" {
    var mut_source = "#line 1 \"blah.rc\"".*;
    var result = try parseAndRemoveLineCommands(std.testing.allocator, &mut_source, &mut_source);
    defer result.mappings.deinit(std.testing.allocator);
    try std.testing.expectEqualStrings("", result.result);
}
