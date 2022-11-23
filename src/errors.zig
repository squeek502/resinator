const std = @import("std");
const Token = @import("lex.zig").Token;
const SourceMappings = @import("source_mapping.zig").SourceMappings;

pub const Diagnostics = struct {
    errors: std.ArrayListUnmanaged(ErrorDetails) = .{},
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Diagnostics {
        return .{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Diagnostics) void {
        self.errors.deinit(self.allocator);
    }

    pub fn append(self: *Diagnostics, error_details: ErrorDetails) !void {
        try self.errors.append(self.allocator, error_details);
    }

    pub fn renderToStdErr(self: *Diagnostics, cwd: std.fs.Dir, source: []const u8, source_mappings: ?SourceMappings) void {
        // TODO: take this as a param probably
        const ttyconf = std.debug.detectTTYConfig();
        std.debug.getStderrMutex().lock();
        defer std.debug.getStderrMutex().unlock();
        const stderr = std.io.getStdErr().writer();
        for (self.errors.items) |err_details| {
            renderErrorMessage(stderr, ttyconf, cwd, err_details, source, source_mappings) catch return;
        }
    }
};

pub const ErrorDetails = struct {
    err: Error,
    token: Token,
    extra: union {
        none: void,
        expected: Token.Id,
    } = .{ .none = {} },

    pub const Error = enum {
        // Lexer
        unfinished_string_literal,

        // Parser
        unfinished_raw_data_block,
        unfinished_string_table_block,
        /// `expected` is populated.
        expected_token,

        // Compiler
        string_resource_as_numeric_type,
    };

    pub fn render(self: ErrorDetails, writer: anytype, source: []const u8) !void {
        switch (self.err) {
            .unfinished_string_literal => {
                return writer.print("unfinished string literal at '{s}', expected closing '\"'", .{self.token.nameForErrorDisplay(source)});
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
            .string_resource_as_numeric_type => {
                // TODO: Add note about why this is the case (i.e. it always (?) leads to an invalid .res)
                return writer.writeAll("the number 6 (RT_STRING) can not be used as a resource type");
            },
        }
    }
};

pub fn renderErrorMessage(writer: anytype, ttyconf: std.debug.TTY.Config, cwd: std.fs.Dir, err_details: ErrorDetails, source: []const u8, source_mappings: ?SourceMappings) !void {
    const source_line_start = err_details.token.getLineStart(source);
    const column = err_details.token.calculateColumn(source, 1, source_line_start);

    // var counting_writer_container = std.io.countingWriter(writer);
    // const counting_writer = counting_writer_container.writer();

    ttyconf.setColor(writer, .Bold);
    ttyconf.setColor(writer, .Dim);
    try writer.writeAll("<after preprocessor>");
    ttyconf.setColor(writer, .Reset);
    ttyconf.setColor(writer, .Bold);
    try writer.print(":{d}:{d}: ", .{ err_details.token.line_number, column });
    ttyconf.setColor(writer, .Red);
    try writer.writeAll("error: ");
    ttyconf.setColor(writer, .Reset);
    ttyconf.setColor(writer, .Bold);
    try err_details.render(writer, source);
    try writer.writeAll("\n");
    ttyconf.setColor(writer, .Reset);

    const source_line = err_details.token.getLine(source, source_line_start);
    for (source_line) |c| switch (c) {
        '\t' => try writer.writeByte(' '),
        else => try writer.writeByte(c),
    };
    try writer.writeByte('\n');

    ttyconf.setColor(writer, .Green);
    const token_offset = err_details.token.start - source_line_start;
    try writer.writeByteNTimes(' ', token_offset);
    try writer.writeByte('^');
    try writer.writeByte('\n');
    ttyconf.setColor(writer, .Reset);

    if (source_mappings) |mappings| {
        const corresponding_span = mappings.get(err_details.token.line_number);
        const corresponding_file = mappings.files.get(corresponding_span.filename_offset);

        ttyconf.setColor(writer, .Bold);
        try writer.print("{s}:{d}:{d}: ", .{ corresponding_file, corresponding_span.start_line, 1 });
        ttyconf.setColor(writer, .Cyan);
        try writer.writeAll("note: ");
        ttyconf.setColor(writer, .Reset);
        ttyconf.setColor(writer, .Bold);
        try writer.writeAll("this line originated from line");
        if (corresponding_span.start_line != corresponding_span.end_line) {
            try writer.print("s {}-{}", .{ corresponding_span.start_line, corresponding_span.end_line });
        } else {
            try writer.print(" {}", .{corresponding_span.start_line});
        }
        try writer.print(" of file '{s}'\n", .{corresponding_file});
        ttyconf.setColor(writer, .Reset);

        if (cwd.openFile(corresponding_file, .{})) |file| {
            var buffered_reader = std.io.bufferedReader(file.reader());
            writeLinesFromStream(writer, buffered_reader.reader(), corresponding_span.start_line, corresponding_span.end_line) catch |err| switch (err) {
                error.LinesNotFound => {
                    ttyconf.setColor(writer, .Red);
                    try writer.writeAll(" | ");
                    ttyconf.setColor(writer, .Dim);
                    try writer.print("unable to print line(s) from file: {s}\n", .{@errorName(err)});
                    ttyconf.setColor(writer, .Reset);
                },
                else => |e| return e,
            };
            try writer.writeByte('\n');
        } else |err| {
            ttyconf.setColor(writer, .Red);
            try writer.writeAll(" | ");
            ttyconf.setColor(writer, .Dim);
            try writer.print("unable to print line(s) from file: {s}\n", .{@errorName(err)});
            ttyconf.setColor(writer, .Reset);
        }
    }
}

pub fn writeLinesFromStream(writer: anytype, input: anytype, start_line: usize, end_line: usize) !void {
    var line_num: usize = 1;
    while (try readByteOrEof(input)) |byte| {
        switch (byte) {
            '\n' => {
                if (line_num >= start_line) try writer.writeByte(byte);
                if (line_num == end_line) return;
                line_num += 1;
            },
            else => {
                if (line_num >= start_line) try writer.writeByte(byte);
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
