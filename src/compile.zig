const std = @import("std");
const Allocator = std.mem.Allocator;
const Node = @import("ast.zig").Node;
const Lexer = @import("lex.zig").Lexer;
const Parser = @import("parse.zig").Parser;
const Resource = @import("rc.zig").Resource;
const Token = @import("lex.zig").Token;
const Number = @import("literals.zig").Number;
const parseNumberLiteral = @import("literals.zig").parseNumberLiteral;
const parseQuotedAsciiString = @import("literals.zig").parseQuotedAsciiString;
const parseQuotedWideStringAlloc = @import("literals.zig").parseQuotedWideStringAlloc;
const res = @import("res.zig");
const WORD = std.os.windows.WORD;
const DWORD = std.os.windows.DWORD;

pub fn compile(allocator: Allocator, source: []const u8, writer: anytype, cwd: std.fs.Dir) !void {
    var lexer = Lexer.init(source);
    var parser = Parser.init(&lexer);
    var tree = try parser.parse(allocator);
    defer tree.deinit();

    var arena_allocator = std.heap.ArenaAllocator.init(allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    var compiler = Compiler{
        .source = source,
        .arena = arena,
        .allocator = allocator,
        .cwd = cwd,
    };

    try compiler.writeRoot(tree.root(), writer);
}

pub const Compiler = struct {
    source: []const u8,
    arena: Allocator,
    allocator: Allocator,
    cwd: std.fs.Dir,

    pub const State = struct {};

    pub fn writeRoot(self: *Compiler, root: *Node.Root, writer: anytype) !void {
        try writeEmptyResource(writer);
        for (root.body) |node| {
            try self.writeNode(node, writer);
        }
    }

    pub fn writeNode(self: *Compiler, node: *Node, writer: anytype) !void {
        switch (node.id) {
            .root => unreachable, // writeRoot should be called directly instead
            .resource_external => try self.writeResourceExternal(@fieldParentPtr(Node.ResourceExternal, "base", node), writer),
            .resource_raw_data => try self.writeResourceRawData(@fieldParentPtr(Node.ResourceRawData, "base", node), writer),
            .literal => unreachable, // this is context dependent and should be handled by its parent
            .binary_expression => @panic("TODO"),
            .grouped_expression => @panic("TODO"),
            .invalid => @panic("TODO"),
        }
    }

    pub const NameOrOrdinal = union(enum) {
        name: [:0]const u16,
        ordinal: u16,

        pub fn deinit(self: NameOrOrdinal, allocator: Allocator) void {
            switch (self) {
                .name => |name| {
                    allocator.free(name);
                },
                .ordinal => {},
            }
        }

        /// Returns the full length of the amount of bytes that would be written by `write`
        /// (e.g. for an ordinal it will return the length including the 0xFFFF indicator)
        pub fn byteLen(self: NameOrOrdinal) u32 {
            switch (self) {
                .name => |name| {
                    // + 1 for 0-terminated, * 2 for bytes per u16
                    return @intCast(u32, (name.len + 1) * 2);
                },
                .ordinal => return 4,
            }
        }

        pub fn write(self: NameOrOrdinal, writer: anytype) !void {
            switch (self) {
                .name => |name| {
                    try writer.writeAll(std.mem.sliceAsBytes(name[0 .. name.len + 1]));
                },
                .ordinal => |ordinal| {
                    try writer.writeIntLittle(WORD, 0xffff);
                    try writer.writeIntLittle(WORD, ordinal);
                },
            }
        }

        pub fn fromString(allocator: Allocator, str: []const u8) !NameOrOrdinal {
            if (maybeOrdinalFromString(str)) |ordinal| {
                return ordinal;
            }
            return nameFromString(allocator, str);
        }

        pub fn nameFromString(allocator: Allocator, str: []const u8) !NameOrOrdinal {
            const as_utf16 = try std.unicode.utf8ToUtf16LeWithNull(allocator, str);
            return NameOrOrdinal{ .name = as_utf16 };
        }

        pub fn maybeOrdinalFromString(str: []const u8) ?NameOrOrdinal {
            // TODO: Needs to match the `rc` ordinal parsing
            const ordinal = std.fmt.parseUnsigned(WORD, str, 0) catch return null;
            return NameOrOrdinal{ .ordinal = ordinal };
        }
    };

    const Filename = struct {
        utf8: []const u8,
        needs_free: bool = false,

        pub fn deinit(self: Filename, allocator: Allocator) void {
            if (self.needs_free) {
                allocator.free(self.utf8);
            }
        }
    };

    pub fn calculateColumnOfToken(self: *Compiler, token: Token) usize {
        const line_start = line_start: {
            var index = token.start;
            while (true) {
                if (self.source[index] == '\n') break :line_start index + 1;
                if (index != 0) index -= 1 else break;
            }
            break :line_start 0;
        };

        var i: usize = line_start;
        var column: usize = 0;
        while (i < token.start) : (i += 1) {
            const c = self.source[i];
            switch (c) {
                '\t' => column += @import("literals.zig").columnsUntilTabStop(column),
                else => column += 1,
            }
        }
        return column;
    }

    pub fn evaluateFilenameExpression(self: *Compiler, expression_node: *Node) !Filename {
        switch (expression_node.id) {
            .literal => {
                const literal_node = expression_node.cast(.literal).?;
                switch (literal_node.token.id) {
                    .literal, .number => {
                        const literal_as_string = literal_node.token.slice(self.source);
                        return .{ .utf8 = literal_as_string };
                    },
                    .quoted_ascii_string => {
                        const slice = literal_node.token.slice(self.source);
                        const column = self.calculateColumnOfToken(literal_node.token);
                        const parsed = try parseQuotedAsciiString(self.allocator, slice, column);
                        return .{ .utf8 = parsed, .needs_free = true };
                    },
                    .quoted_wide_string => {
                        // TODO: No need to parse this to UTF-16 and then back to UTF-8
                        // if it's already UTF-8. Should have a function that parses wide
                        // strings directly to UTF-8.
                        const slice = literal_node.token.slice(self.source);
                        const column = self.calculateColumnOfToken(literal_node.token);
                        const parsed_string = try parseQuotedWideStringAlloc(self.allocator, slice, column);
                        defer self.allocator.free(parsed_string);
                        const parsed_as_utf8 = try std.unicode.utf16leToUtf8Alloc(self.allocator, parsed_string);
                        return .{ .utf8 = parsed_as_utf8, .needs_free = true };
                    },
                    else => unreachable, // no other token types should be in a filename literal node
                }
            },
            .binary_expression => {
                const binary_expression_node = expression_node.cast(.binary_expression).?;
                return self.evaluateFilenameExpression(binary_expression_node.right);
            },
            .grouped_expression => {
                const grouped_expression_node = expression_node.cast(.grouped_expression).?;
                return self.evaluateFilenameExpression(grouped_expression_node.expression);
            },
            else => unreachable,
        }
    }

    pub fn writeResourceExternal(self: *Compiler, node: *Node.ResourceExternal, writer: anytype) !void {
        const filename = try self.evaluateFilenameExpression(node.filename);
        defer filename.deinit(self.allocator);

        // TODO: emit error on file not found
        const contents: []const u8 = self.cwd.readFileAlloc(self.allocator, filename.utf8, std.math.maxInt(u32)) catch "";
        defer self.allocator.free(contents);

        try self.writeResource(writer, node.id, node.type, contents);
    }

    pub const DataType = enum {
        number,
        ascii_string,
        wide_string,
    };

    pub const Data = union(DataType) {
        number: Number,
        ascii_string: []const u8,
        wide_string: []const u16,

        pub fn deinit(self: Data, allocator: Allocator) void {
            switch (self) {
                .wide_string => |wide_string| {
                    allocator.free(wide_string);
                },
                .ascii_string => |ascii_string| {
                    allocator.free(ascii_string);
                },
                else => {},
            }
        }

        pub fn write(self: Data, writer: anytype) !void {
            switch (self) {
                .number => |number| switch (number.is_long) {
                    false => try writer.writeIntLittle(WORD, number.asWord()),
                    true => try writer.writeIntLittle(DWORD, number.value),
                },
                .ascii_string => |ascii_string| {
                    try writer.writeAll(ascii_string);
                },
                .wide_string => |wide_string| {
                    try writer.writeAll(std.mem.sliceAsBytes(wide_string));
                },
            }
        }

        pub fn evaluateOperator(operator_char: u8, lhs: Data, rhs: Data) Data {
            std.debug.assert(lhs == .number);
            std.debug.assert(rhs == .number);
            const result = switch (operator_char) {
                '-' => lhs.number.value -% rhs.number.value,
                '+' => lhs.number.value +% rhs.number.value,
                '|' => lhs.number.value | rhs.number.value,
                '&' => lhs.number.value & rhs.number.value,
                else => unreachable, // invalid operator, this would be a lexer/parser bug
            };
            return .{ .number = .{
                .value = result,
                .is_long = lhs.number.is_long or rhs.number.is_long,
            } };
        }
    };

    pub fn evaluateDataExpression(self: *Compiler, expression_node: *Node) !Data {
        switch (expression_node.id) {
            .literal => {
                const literal_node = expression_node.cast(.literal).?;
                switch (literal_node.token.id) {
                    .number => {
                        const number = parseNumberLiteral(literal_node.token.slice(self.source));
                        return .{ .number = number };
                    },
                    .quoted_ascii_string => {
                        const slice = literal_node.token.slice(self.source);
                        const column = self.calculateColumnOfToken(literal_node.token);
                        const parsed = try parseQuotedAsciiString(self.allocator, slice, column);
                        errdefer self.allocator.free(parsed);
                        return .{ .ascii_string = parsed };
                    },
                    .quoted_wide_string => {
                        const slice = literal_node.token.slice(self.source);
                        const column = self.calculateColumnOfToken(literal_node.token);
                        const parsed_string = try parseQuotedWideStringAlloc(self.allocator, slice, column);
                        errdefer self.allocator.free(parsed_string);
                        return .{ .wide_string = parsed_string };
                    },
                    else => unreachable, // no other token types should be in a data literal node
                }
            },
            .binary_expression => {
                const binary_expression_node = expression_node.cast(.binary_expression).?;
                const lhs = try self.evaluateDataExpression(binary_expression_node.left);
                defer lhs.deinit(self.allocator);
                const rhs = try self.evaluateDataExpression(binary_expression_node.right);
                defer rhs.deinit(self.allocator);
                const operator_char = binary_expression_node.operator.slice(self.source)[0];
                return Data.evaluateOperator(operator_char, lhs, rhs);
            },
            else => {
                std.debug.print("{}\n", .{expression_node.id});
                @panic("TODO: evaluateDataExpression");
            },
        }
    }

    pub fn writeResourceRawData(self: *Compiler, node: *Node.ResourceRawData, writer: anytype) !void {
        var data_buffer = std.ArrayList(u8).init(self.allocator);
        defer data_buffer.deinit();
        const data_writer = data_buffer.writer();

        for (node.raw_data) |expression| {
            const data = try self.evaluateDataExpression(expression);
            defer data.deinit(self.allocator);
            try data.write(data_writer);
        }

        try self.writeResource(writer, node.id, node.type, data_buffer.items);
    }

    pub fn writeResource(self: *Compiler, writer: anytype, id_token: Token, type_token: Token, data: []const u8) !void {
        const default_language = 0x409;
        const default_memory_flags = 0x30;

        const type_value = type: {
            const resource_type = Resource.fromString(type_token.slice(self.source));
            if (resource_type != .user_defined) {
                if (res.RT.fromResource(resource_type)) |rt_constant| {
                    break :type NameOrOrdinal{ .ordinal = @enumToInt(rt_constant) };
                } else {
                    @panic("TODO: unhandled resource -> RT constant conversion");
                }
            } else {
                break :type try NameOrOrdinal.fromString(self.allocator, type_token.slice(self.source));
            }
        };
        defer type_value.deinit(self.allocator);

        // TODO: The type can change how the resource is written to the file
        //       (i.e. if it's an ICON then the contents should be parsed as a .ico, among
        //       other things)

        const name_value = try NameOrOrdinal.fromString(self.allocator, id_token.slice(self.source));
        defer name_value.deinit(self.allocator);

        const byte_length_up_to_name: u32 = 8 + name_value.byteLen() + type_value.byteLen();
        const padding_after_name = std.mem.alignForward(byte_length_up_to_name, 4) - byte_length_up_to_name;
        const header_size: u32 = byte_length_up_to_name + @intCast(u32, padding_after_name) + 16;

        try writer.writeIntLittle(DWORD, @intCast(u32, data.len)); // DataSize
        try writer.writeIntLittle(DWORD, header_size); // HeaderSize
        try type_value.write(writer); // TYPE
        try name_value.write(writer); // NAME
        try writer.writeByteNTimes(0, padding_after_name);

        try writer.writeIntLittle(DWORD, 0); // DataVersion
        try writer.writeIntLittle(WORD, default_memory_flags); // MemoryFlags
        try writer.writeIntLittle(WORD, default_language); // LanguageId
        try writer.writeIntLittle(DWORD, 0); // Version
        try writer.writeIntLittle(DWORD, 0); // Characteristics

        try writer.writeAll(data);

        const padding_after_data = std.mem.alignForward(data.len, 4) - data.len;
        try writer.writeByteNTimes(0, padding_after_data);
    }

    pub fn writeEmptyResource(writer: anytype) !void {
        try writer.writeIntLittle(DWORD, 0); // DataSize
        try writer.writeIntLittle(DWORD, 32); // HeaderSize
        try writer.writeIntLittle(WORD, 0xffff);
        try writer.writeIntLittle(WORD, 0x0000); // TYPE
        try writer.writeIntLittle(WORD, 0xffff);
        try writer.writeIntLittle(WORD, 0x0000); // NAME
        try writer.writeIntLittle(DWORD, 0); // DataVersion
        try writer.writeIntLittle(WORD, 0); // MemoryFlags
        try writer.writeIntLittle(WORD, 0); // LanguageId
        try writer.writeIntLittle(DWORD, 0); // Version
        try writer.writeIntLittle(DWORD, 0); // Characteristics
    }
};

fn testCompile(source: []const u8, cwd: std.fs.Dir) !void {
    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();

    try compile(std.testing.allocator, source, buffer.writer(), cwd);

    const expected_res = try getExpectedFromWindowsRC(std.testing.allocator, source);
    defer std.testing.allocator.free(expected_res);

    try std.testing.expectEqualSlices(u8, expected_res, buffer.items);
}

fn testCompileWithOutput(source: []const u8, expected_output: []const u8, cwd: std.fs.Dir) !void {
    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();

    try compile(std.testing.allocator, source, buffer.writer(), cwd);

    std.testing.expectEqualSlices(u8, expected_output, buffer.items) catch |e| {
        std.debug.print("got:\n{}\n", .{std.zig.fmtEscapes(buffer.items)});
        std.debug.print("expected:\n{}\n", .{std.zig.fmtEscapes(expected_output)});
        return e;
    };
}

pub fn getExpectedFromWindowsRC(allocator: Allocator, source: []const u8) ![]const u8 {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile("test.rc", source);

    var result = try std.ChildProcess.exec(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            // TODO: Don't hardcode this
            "C:\\Program Files (x86)\\Windows Kits\\10\\bin\\10.0.19041.0\\x86\\rc.exe",
            "test.rc",
        },
        .cwd = ("zig-cache/tmp/" ++ tmp.sub_path),
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    switch (result.term) {
        .Exited => |code| {
            if (code != 0) {
                std.debug.print("exit code: {}\n", .{result.term});
                std.debug.print("stdout: {s}\n", .{result.stdout});
                std.debug.print("stderr: {s}\n", .{result.stderr});
                return error.ExitCodeFailure;
            }
        },
        .Signal, .Stopped, .Unknown => {
            return error.ProcessTerminated;
        },
    }

    return tmp.dir.readFileAlloc(allocator, "test.res", std.math.maxInt(usize));
}

test "empty rc" {
    try testCompileWithOutput(
        "",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
}

test "basic rcdata" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile("file.bin", "hello world");

    try testCompileWithOutput(
        "1 RCDATA file.bin",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00hello world\x00",
        tmp_dir.dir,
    );
    try testCompileWithOutput(
        "1 RCDATA \"file.bin\"",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00hello world\x00",
        tmp_dir.dir,
    );
    try testCompileWithOutput(
        "1 RCDATA L\"file.bin\"",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00hello world\x00",
        tmp_dir.dir,
    );
}

test "basic rcdata with empty raw data" {
    try testCompileWithOutput(
        "1 RCDATA {}",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
}

test "basic rcdata with raw data" {
    try testCompileWithOutput(
        "1 RCDATA { 1, \"2\", L\"3\" }",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x01\x0023\x00\x00\x00\x00",
        std.fs.cwd(),
    );
}

test "basic but with tricky type" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile("file.bin", "hello world");

    try testCompileWithOutput(
        "1 \"RCDATA\" file.bin",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x000\x00\x00\x00\"\x00R\x00C\x00D\x00A\x00T\x00A\x00\"\x00\x00\x00\xff\xff\x01\x00\x00\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00hello world\x00",
        tmp_dir.dir,
    );
}

test "raw data with number expression" {
    try testCompileWithOutput(
        "1 RCDATA { 1+1 }",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00",
        std.fs.cwd(),
    );
    // overflow is wrapping
    try testCompileWithOutput(
        "1 RCDATA { 65535+1 }",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
    // binary operators promote to the largest size of their operands
    try testCompileWithOutput(
        "1 RCDATA { 65535 + 1L }",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00",
        std.fs.cwd(),
    );
}

test "filenames as numeric expressions" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile("-1", "hello world");
    try testCompileWithOutput(
        "1 RCDATA -1",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00hello world\x00",
        tmp_dir.dir,
    );
    try tmp_dir.dir.deleteFile("-1");

    try tmp_dir.dir.writeFile("~1", "hello world");
    try testCompileWithOutput(
        "1 RCDATA ~1",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00hello world\x00",
        tmp_dir.dir,
    );
    try tmp_dir.dir.deleteFile("~1");

    try tmp_dir.dir.writeFile("1", "hello world");
    try testCompileWithOutput(
        "1 RCDATA 1+1",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00hello world\x00",
        tmp_dir.dir,
    );
    try tmp_dir.dir.deleteFile("1");

    try tmp_dir.dir.writeFile("-1", "hello world");
    try testCompileWithOutput(
        "1 RCDATA 1+-1",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00hello world\x00",
        tmp_dir.dir,
    );
    try tmp_dir.dir.deleteFile("-1");

    try tmp_dir.dir.writeFile("-1", "hello world");
    try testCompileWithOutput(
        "1 RCDATA (1+-1)",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00hello world\x00",
        tmp_dir.dir,
    );
    try tmp_dir.dir.deleteFile("-1");
}
