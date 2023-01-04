const std = @import("std");
const Token = @import("lex.zig").Token;
const SourceMappings = @import("source_mapping.zig").SourceMappings;
const utils = @import("utils.zig");
const rc = @import("rc.zig");
const res = @import("res.zig");

pub const Diagnostics = struct {
    errors: std.ArrayListUnmanaged(ErrorDetails) = .{},
    /// Append-only, cannot handle removing strings.
    /// Expects to own all strings within the list.
    strings: std.ArrayListUnmanaged([]const u8) = .{},
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Diagnostics {
        return .{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Diagnostics) void {
        self.errors.deinit(self.allocator);
        for (self.strings.items) |str| {
            self.allocator.free(str);
        }
        self.strings.deinit(self.allocator);
    }

    pub fn append(self: *Diagnostics, error_details: ErrorDetails) !void {
        try self.errors.append(self.allocator, error_details);
    }

    /// Returns the index of the added string
    pub fn putString(self: *Diagnostics, str: []const u8) !usize {
        const dupe = try self.allocator.dupe(u8, str);
        const index = self.strings.items.len;
        try self.strings.append(self.allocator, dupe);
        return index;
    }

    pub fn renderToStdErr(self: *Diagnostics, cwd: std.fs.Dir, source: []const u8, source_mappings: ?SourceMappings) void {
        // Set the codepage to UTF-8 unconditionally to ensure that everything renders okay
        // TODO: Reset codepage afterwards?
        if (@import("builtin").os.tag == .windows) {
            _ = std.os.windows.kernel32.SetConsoleOutputCP(65001);
        }

        // TODO: take this as a param probably
        const colors = utils.Colors.detect();
        std.debug.getStderrMutex().lock();
        defer std.debug.getStderrMutex().unlock();
        const stderr = std.io.getStdErr().writer();
        for (self.errors.items) |err_details| {
            renderErrorMessage(self.allocator, stderr, colors, cwd, err_details, source, self.strings.items, source_mappings) catch return;
        }
    }
};

/// Contains enough context to append errors/warnings/notes etc
pub const DiagnosticsContext = struct {
    diagnostics: *Diagnostics,
    token: Token,
};

pub const ErrorDetails = struct {
    err: Error,
    token: Token,
    // TODO: I don't really like this; notes at least should probably be handled differently
    type: enum { err, warning, note } = .err,
    print_source_line: bool = true,
    extra: union {
        none: void,
        expected: Token.Id,
        number: u32,
        expected_types: ExpectedTypes,
        // TODO: Add 'nameForErrorDisplay' to Resource for better error messages
        resource: rc.Resource,
        string_and_language: StringAndLanguage,
        file_open_error: FileOpenError,
        icon_dir: IconDirContext,
    } = .{ .none = {} },

    comptime {
        // all fields in the extra union should be 32 bits or less
        for (std.meta.fields(std.meta.fieldInfo(ErrorDetails, .extra).type)) |field| {
            std.debug.assert(@bitSizeOf(field.type) <= 32);
        }
    }

    pub const StringAndLanguage = packed struct(u32) {
        id: u16,
        language: res.Language,
    };

    pub const FileOpenError = packed struct(u32) {
        err: FileOpenErrorEnum,
        filename_string_index: FilenameStringIndex,

        pub const FilenameStringIndex = std.meta.Int(.unsigned, 32 - @bitSizeOf(FileOpenErrorEnum));
        pub const FileOpenErrorEnum = std.meta.FieldEnum(std.fs.File.OpenError);

        pub fn enumFromError(err: std.fs.File.OpenError) FileOpenErrorEnum {
            return switch (err) {
                inline else => |e| @field(ErrorDetails.FileOpenError.FileOpenErrorEnum, @errorName(e)),
            };
        }
    };

    pub const IconDirContext = packed struct(u32) {
        icon_type: enum(u1) { cursor, icon },
        icon_format: enum(u2) { unknown, riff, png },
        index: u16,
        _: u13 = undefined,
    };

    pub const ExpectedTypes = packed struct(u32) {
        number: bool = false,
        number_expression: bool = false,
        string_literal: bool = false,
        accelerator_type_or_option: bool = false,
        control_class: bool = false,
        _: u27 = undefined,

        pub const strings = std.ComptimeStringMap([]const u8, .{
            .{ "number", "number" },
            .{ "number_expression", "number expression" },
            .{ "string_literal", "quoted string literal" },
            .{ "accelerator_type_or_option", "accelerator type or option [ASCII, VIRTKEY, etc]" },
            .{ "control_class", "control class [BUTTON, EDIT, etc]" },
        });

        pub fn writeCommaSeparated(self: ExpectedTypes, writer: anytype) !void {
            const struct_info = @typeInfo(ExpectedTypes).Struct;
            const num_real_fields = struct_info.fields.len - 1;
            const num_padding_bits = @bitSizeOf(ExpectedTypes) - num_real_fields;
            const mask = std.math.maxInt(struct_info.backing_integer.?) >> num_padding_bits;
            const relevant_bits_only = @bitCast(struct_info.backing_integer.?, self) & mask;
            const num_set_bits = @popCount(relevant_bits_only);

            var i: usize = 0;
            inline for (struct_info.fields) |field_info| {
                if (field_info.type != bool) continue;
                if (i == num_set_bits) return;
                if (@field(self, field_info.name)) {
                    try writer.writeAll(strings.get(field_info.name).?);
                    i += 1;
                    if (num_set_bits > 2 and i != num_set_bits) {
                        try writer.writeAll(", ");
                    } else if (i != num_set_bits) {
                        try writer.writeByte(' ');
                    }
                    if (num_set_bits > 1 and i == num_set_bits - 1) {
                        try writer.writeAll("or ");
                    }
                }
            }
        }
    };

    pub const Error = enum {
        // Lexer
        unfinished_string_literal,
        string_literal_too_long,
        illegal_byte,
        illegal_byte_outside_string_literals,
        illegal_byte_order_mark,
        illegal_private_use_character,
        found_c_style_escaped_quote,
        code_page_pragma_missing_left_paren,
        code_page_pragma_missing_right_paren,
        code_page_pragma_invalid_code_page,
        code_page_pragma_unsupported_code_page,

        // Parser
        unfinished_raw_data_block,
        unfinished_string_table_block,
        /// `expected` is populated.
        expected_token,
        /// `expected_types` is populated
        expected_something_else,
        /// `resource` is populated
        resource_type_cant_use_raw_data,
        /// `resource` is populated
        id_must_be_ordinal,
        /// `resource` is populated
        name_or_id_not_allowed,
        string_resource_as_numeric_type,
        ascii_character_not_equivalent_to_virtual_key_code,
        empty_menu_not_allowed,
        rc_would_miscompile_version_value_padding,

        // Compiler
        /// `string_and_language` is populated
        string_already_defined,
        font_id_already_defined,
        /// `file_open_error` is populated
        file_open_error,
        invalid_accelerator_key,
        accelerator_type_required,
        rc_would_miscompile_control_padding,
        rc_would_miscompile_control_class_ordinal,
        /// `icon_dir` is populated
        rc_would_error_on_icon_dir,
        /// `icon_dir` is populated
        format_not_supported_in_icon_dir,
        /// `resource` is populated and contains the expected type
        icon_dir_and_resource_type_mismatch,

        // Literals
        /// `number` is populated
        rc_would_miscompile_codepoint_byte_swap,
        /// `number` is populated
        rc_would_miscompile_codepoint_skip,
    };

    pub fn render(self: ErrorDetails, writer: anytype, source: []const u8, strings: []const []const u8) !void {
        switch (self.err) {
            .unfinished_string_literal => {
                return writer.print("unfinished string literal at '{s}', expected closing '\"'", .{self.token.nameForErrorDisplay(source)});
            },
            .string_literal_too_long => {
                return writer.writeAll("string literal too long (max is 4097 characters)");
            },
            .illegal_byte => {
                const byte = self.token.slice(source)[0];
                return writer.print("character '\\x{X:0>2}' is not allowed", .{byte});
            },
            .illegal_byte_outside_string_literals => {
                const byte = self.token.slice(source)[0];
                return writer.print("character '\\x{X:0>2}' is not allowed outside of string literals", .{byte});
            },
            .illegal_byte_order_mark => {
                return writer.writeAll("byte order mark <U+FEFF> is not allowed");
            },
            .illegal_private_use_character => {
                return writer.writeAll("private use character <U+E000> is not allowed");
            },
            .found_c_style_escaped_quote => {
                return writer.writeAll("escaping quotes with \\\" is not allowed (use \"\" instead)");
            },
            .code_page_pragma_missing_left_paren => {
                return writer.writeAll("expected left parenthesis after 'code_page' in code_page #pragma");
            },
            .code_page_pragma_missing_right_paren => {
                return writer.writeAll("expected right parenthesis after '<number>' in code_page #pragma");
            },
            .code_page_pragma_invalid_code_page => {
                return writer.writeAll("invalid or unknown code page in code_page #pragma");
            },
            .code_page_pragma_unsupported_code_page => {
                // TODO add info about which code page is unsupported
                return writer.writeAll("unsupported code page in code_page #pragma");
            },
            .unfinished_raw_data_block => {
                return writer.print("unfinished raw data block at '{s}', expected closing '}}' or 'END'", .{self.token.nameForErrorDisplay(source)});
            },
            .unfinished_string_table_block => {
                return writer.print("unfinished STRINGTABLE block at '{s}', expected closing '}}' or 'END'", .{self.token.nameForErrorDisplay(source)});
            },
            .expected_token => {
                return writer.print("expected '{s}', got '{s}'", .{ self.extra.expected.nameForErrorDisplay(), self.token.nameForErrorDisplay(source) });
            },
            .expected_something_else => {
                try writer.writeAll("expected ");
                try self.extra.expected_types.writeCommaSeparated(writer);
                return writer.print("; got '{s}'", .{self.token.nameForErrorDisplay(source)});
            },
            .resource_type_cant_use_raw_data => switch (self.type) {
                .err, .warning => try writer.print("expected '<filename>', found '{s}' (resource type '{s}' can't use raw data)", .{ self.token.nameForErrorDisplay(source), @tagName(self.extra.resource) }),
                .note => try writer.print("if '{s}' is intended to be a filename, it must be specified as a quoted string literal", .{self.token.nameForErrorDisplay(source)}),
            },
            .id_must_be_ordinal => {
                try writer.print("id of resource type '{s}' must be an ordinal (u16), got '{s}'", .{ @tagName(self.extra.resource), self.token.nameForErrorDisplay(source) });
            },
            .name_or_id_not_allowed => {
                try writer.print("name or id is not allowed for resource type '{s}'", .{@tagName(self.extra.resource)});
            },
            .string_resource_as_numeric_type => switch (self.type) {
                .err, .warning => try writer.writeAll("the number 6 (RT_STRING) cannot be used as a resource type"),
                .note => try writer.writeAll("using RT_STRING directly likely results in an invalid .res file, use a STRINGTABLE instead"),
            },
            .ascii_character_not_equivalent_to_virtual_key_code => {
                // TODO: Better wording? This is what the Win32 RC compiler emits.
                //       This occurs when VIRTKEY and a control code is specified ("^c", etc)
                try writer.writeAll("ASCII character not equivalent to virtual key code");
            },
            .empty_menu_not_allowed => {
                try writer.print("empty menu of type '{s}' not allowed", .{self.token.nameForErrorDisplay(source)});
            },
            .rc_would_miscompile_version_value_padding => switch (self.type) {
                .err, .warning => return writer.print("the padding before this quoted string value would be miscompiled by the Win32 RC compiler", .{}),
                .note => return writer.print("to avoid the potential miscompilation, consider adding a comma between the VALUE's key and the quoted string", .{}),
            },
            .string_already_defined => switch (self.type) {
                // TODO: better printing of language, using constant names from WinNT.h
                .err, .warning => return writer.print("string with id {d} (0x{X}) already defined for language {d},{d}", .{ self.extra.string_and_language.id, self.extra.string_and_language.id, self.extra.string_and_language.language.primary_language_id, self.extra.string_and_language.language.sublanguage_id }),
                .note => return writer.print("previous definition of string with id {d} (0x{X}) here", .{ self.extra.string_and_language.id, self.extra.string_and_language.id }),
            },
            .font_id_already_defined => switch (self.type) {
                .err => return writer.print("font with id {d} already defined", .{self.extra.number}),
                .warning => return writer.print("skipped duplicate font with id {d}", .{self.extra.number}),
                .note => return writer.print("previous definition of font with id {d} here", .{self.extra.number}),
            },
            .file_open_error => {
                try writer.print("unable to open file '{s}': {s}", .{ strings[self.extra.file_open_error.filename_string_index], @tagName(self.extra.file_open_error.err) });
            },
            .invalid_accelerator_key => {
                try writer.print("invalid accelerator key: {s}", .{self.token.nameForErrorDisplay(source)});
            },
            .accelerator_type_required => {
                try writer.print("accelerator type [ASCII or VIRTKEY] required when key is an integer", .{});
            },
            .rc_would_miscompile_control_padding => switch (self.type) {
                .err, .warning => return writer.print("the padding before this control would be miscompiled by the Win32 RC compiler (it would insert 2 extra bytes of padding)", .{}),
                .note => return writer.print("to avoid the potential miscompilation, consider removing any 'extra data' blocks from the controls in this dialog", .{}),
            },
            .rc_would_miscompile_control_class_ordinal => switch (self.type) {
                .err, .warning => return writer.print("the control class of this CONTROL would be miscompiled by the Win32 RC compiler", .{}),
                .note => return writer.print("to avoid the potential miscompilation, consider specifying the control class using a string (BUTTON, EDIT, etc) instead of a number", .{}),
            },
            .rc_would_error_on_icon_dir => switch (self.type) {
                .err, .warning => return writer.print("the resource at index {} of this {s} has the format '{s}'; this would be an error in the Win32 RC compiler", .{ self.extra.icon_dir.index, @tagName(self.extra.icon_dir.icon_type), @tagName(self.extra.icon_dir.icon_format) }),
                .note => {
                    // The only note supported is one specific to exactly this combination
                    if (!(self.extra.icon_dir.icon_type == .icon and self.extra.icon_dir.icon_format == .riff)) unreachable;
                    try writer.print("animated RIFF icons within resource groups may not be well supported, consider using an animated icon file (.ani) instead", .{});
                },
            },
            .format_not_supported_in_icon_dir => {
                try writer.print("resource with format '{s}' (at index {}) is not allowed in {s} resource groups", .{ @tagName(self.extra.icon_dir.icon_format), self.extra.icon_dir.index, @tagName(self.extra.icon_dir.icon_type) });
            },
            .icon_dir_and_resource_type_mismatch => {
                const unexpected_type: rc.Resource = if (self.extra.resource == .icon) .cursor else .icon;
                // TODO: Better wording
                try writer.print("resource type '{s}' does not match type '{s}' specified in the file", .{ @tagName(self.extra.resource), @tagName(unexpected_type) });
            },
            .rc_would_miscompile_codepoint_byte_swap => switch (self.type) {
                .err, .warning => return writer.print("codepoint U+{X} within a string literal would be miscompiled by the Win32 RC compiler (the bytes of the UTF-16 code unit would be swapped)", .{self.extra.number}),
                .note => return writer.print("to avoid the potential miscompilation, an integer escape sequence in a wide string literal could be used instead: L\"\\x{X}\"", .{self.extra.number}),
            },
            .rc_would_miscompile_codepoint_skip => switch (self.type) {
                .err, .warning => return writer.print("codepoint U+{X} within a string literal would be miscompiled by the Win32 RC compiler (the codepoint would be missing from the compiled resource)", .{self.extra.number}),
                .note => return writer.print("to avoid the potential miscompilation, an integer escape sequence in a wide string literal could be used instead: L\"\\x{X}\"", .{self.extra.number}),
            },
        }
    }
};

pub fn renderErrorMessage(allocator: std.mem.Allocator, writer: anytype, colors: utils.Colors, cwd: std.fs.Dir, err_details: ErrorDetails, source: []const u8, strings: []const []const u8, source_mappings: ?SourceMappings) !void {
    const source_line_start = err_details.token.getLineStart(source);
    const column = err_details.token.calculateColumn(source, 1, source_line_start);

    // var counting_writer_container = std.io.countingWriter(writer);
    // const counting_writer = counting_writer_container.writer();

    const corresponding_span: ?SourceMappings.SourceSpan = if (source_mappings) |mappings| mappings.get(err_details.token.line_number) else null;
    const corresponding_file: ?[]const u8 = if (source_mappings) |mappings| mappings.files.get(corresponding_span.?.filename_offset) else null;

    const err_line = if (corresponding_span) |span| span.start_line else err_details.token.line_number;

    colors.set(writer, .bold);
    if (corresponding_file) |file| {
        try writer.writeAll(file);
    } else {
        colors.set(writer, .dim);
        try writer.writeAll("<after preprocessor>");
        colors.set(writer, .reset);
        colors.set(writer, .bold);
    }
    try writer.print(":{d}:{d}: ", .{ err_line, column });
    switch (err_details.type) {
        .err => {
            colors.set(writer, .red);
            try writer.writeAll("error: ");
        },
        .warning => {
            colors.set(writer, .yellow);
            try writer.writeAll("warning: ");
        },
        .note => {
            colors.set(writer, .cyan);
            try writer.writeAll("note: ");
        },
    }
    colors.set(writer, .reset);
    colors.set(writer, .bold);
    try err_details.render(writer, source, strings);
    try writer.writeByte('\n');
    colors.set(writer, .reset);

    if (!err_details.print_source_line) {
        try writer.writeByte('\n');
        return;
    }

    const token_offset = err_details.token.start - source_line_start;
    const source_line = err_details.token.getLine(source, source_line_start);

    // Need this to determine if the 'line originated from' note is worth printing
    var source_line_for_display_buf = try std.ArrayList(u8).initCapacity(allocator, source_line.len);
    defer source_line_for_display_buf.deinit();
    try writeSourceSlice(source_line_for_display_buf.writer(), source_line);

    if (err_details.err == .string_literal_too_long) {
        const before_slice = source_line[0..@min(source_line.len, token_offset + 16)];
        try writeSourceSlice(writer, before_slice);
        colors.set(writer, .dim);
        try writer.writeAll("<...truncated...>");
        colors.set(writer, .reset);
    } else {
        try writer.writeAll(source_line_for_display_buf.items);
    }
    try writer.writeByte('\n');

    colors.set(writer, .green);
    try writer.writeByteNTimes(' ', token_offset);
    try writer.writeByte('^');
    const token_len = err_details.token.end - err_details.token.start;
    if (token_len > 1) {
        try writer.writeByteNTimes('~', token_len - 1);
    }
    try writer.writeByte('\n');
    colors.set(writer, .reset);

    if (source_mappings) |_| {
        var corresponding_lines = try CorrespondingLines.init(allocator, cwd, err_details, source_line_for_display_buf.items, corresponding_span.?, corresponding_file.?);
        defer corresponding_lines.deinit(allocator);

        if (!corresponding_lines.worth_printing_note) return;

        colors.set(writer, .bold);
        if (corresponding_file) |file| {
            try writer.writeAll(file);
        } else {
            colors.set(writer, .dim);
            try writer.writeAll("<after preprocessor>");
            colors.set(writer, .reset);
            colors.set(writer, .bold);
        }
        try writer.print(":{d}:{d}: ", .{ err_line, column });
        colors.set(writer, .cyan);
        try writer.writeAll("note: ");
        colors.set(writer, .reset);
        colors.set(writer, .bold);
        try writer.writeAll("this line originated from line");
        if (corresponding_span.?.start_line != corresponding_span.?.end_line) {
            try writer.print("s {}-{}", .{ corresponding_span.?.start_line, corresponding_span.?.end_line });
        } else {
            try writer.print(" {}", .{corresponding_span.?.start_line});
        }
        try writer.print(" of file '{s}'\n", .{corresponding_file.?});
        colors.set(writer, .reset);

        if (!corresponding_lines.worth_printing_lines) return;

        if (corresponding_lines.lines_is_error_message) {
            colors.set(writer, .red);
            try writer.writeAll(" | ");
            colors.set(writer, .dim);
            try writer.writeAll(corresponding_lines.lines.items);
            colors.set(writer, .reset);
            try writer.writeAll("\n\n");
            return;
        }

        try writer.writeAll(corresponding_lines.lines.items);
        try writer.writeAll("\n\n");
    }
}

const CorrespondingLines = struct {
    worth_printing_note: bool = true,
    worth_printing_lines: bool = true,
    lines: std.ArrayListUnmanaged(u8) = .{},
    lines_is_error_message: bool = false,

    pub fn init(allocator: std.mem.Allocator, cwd: std.fs.Dir, err_details: ErrorDetails, lines_for_comparison: []const u8, corresponding_span: SourceMappings.SourceSpan, corresponding_file: []const u8) !CorrespondingLines {
        var corresponding_lines = CorrespondingLines{};

        // We don't do line comparison for this error, so don't print the note if the line
        // number is different
        if (err_details.err == .string_literal_too_long and err_details.token.line_number == corresponding_span.start_line) {
            corresponding_lines.worth_printing_note = false;
            return corresponding_lines;
        }

        // Don't print the originating line for this error, we know it's really long
        if (err_details.err == .string_literal_too_long) {
            corresponding_lines.worth_printing_lines = false;
            return corresponding_lines;
        }

        var writer = corresponding_lines.lines.writer(allocator);
        if (cwd.openFile(corresponding_file, .{})) |file| {
            defer file.close();
            var buffered_reader = std.io.bufferedReader(file.reader());
            writeLinesFromStream(writer, buffered_reader.reader(), corresponding_span.start_line, corresponding_span.end_line) catch |err| switch (err) {
                error.LinesNotFound => {
                    corresponding_lines.lines.clearRetainingCapacity();
                    try writer.print("unable to print line(s) from file: {s}\n", .{@errorName(err)});
                    corresponding_lines.lines_is_error_message = true;
                    return corresponding_lines;
                },
                else => |e| return e,
            };
        } else |err| {
            corresponding_lines.lines.clearRetainingCapacity();
            try writer.print("unable to print line(s) from file: {s}\n", .{@errorName(err)});
            corresponding_lines.lines_is_error_message = true;
            return corresponding_lines;
        }

        // If the lines are the same as they were before preprocessing, skip printing the note entirely
        if (std.mem.eql(u8, lines_for_comparison, corresponding_lines.lines.items)) {
            corresponding_lines.worth_printing_note = false;
        }
        return corresponding_lines;
    }

    pub fn deinit(self: *CorrespondingLines, allocator: std.mem.Allocator) void {
        self.lines.deinit(allocator);
    }
};

fn writeSourceSlice(writer: anytype, slice: []const u8) !void {
    for (slice) |c| try writeSourceByte(writer, c);
}

inline fn writeSourceByte(writer: anytype, byte: u8) !void {
    switch (byte) {
        '\x00'...'\x08', '\x0E'...'\x1F', '\x7F' => try writer.writeAll("�"),
        // \r is seemingly ignored by the RC compiler so skipping it when printing source lines
        // could help avoid confusing output (e.g. RC\rDATA if printed verbatim would show up
        // in the console as DATA but the compiler reads it as RCDATA)
        //
        // NOTE: This is irrelevant when using the clang preprocessor, because unpaired \r
        //       characters get converted to \n, but may become relevant if another
        //       preprocessor is used instead.
        '\r' => {},
        '\t', '\x0B', '\x0C' => try writer.writeByte(' '),
        else => try writer.writeByte(byte),
    }
}

pub fn writeLinesFromStream(writer: anytype, input: anytype, start_line: usize, end_line: usize) !void {
    var line_num: usize = 1;
    while (try readByteOrEof(input)) |byte| {
        switch (byte) {
            '\n' => {
                if (line_num == end_line) return;
                if (line_num >= start_line) try writeSourceByte(writer, byte);
                line_num += 1;
            },
            else => {
                if (line_num >= start_line) try writeSourceByte(writer, byte);
            },
        }
    }
    if (line_num != end_line) {
        return error.LinesNotFound;
    }
}

pub fn readByteOrEof(reader: anytype) !?u8 {
    return reader.readByte() catch |err| switch (err) {
        error.EndOfStream => return null,
        else => |e| return e,
    };
}
