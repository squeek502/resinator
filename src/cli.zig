const std = @import("std");
const CodePage = @import("code_pages.zig").CodePage;
const lang = @import("lang.zig");
const utils = @import("utils.zig");
const Allocator = std.mem.Allocator;

pub const Diagnostics = struct {
    errors: std.ArrayListUnmanaged(ErrorDetails) = .{},
    allocator: Allocator,

    pub const ErrorDetails = struct {
        arg_index: usize,
        arg_span: ArgSpan = .{},
        msg: std.ArrayListUnmanaged(u8) = .{},
        type: Type = .err,
        print_args: bool = true,

        pub const Type = enum { err, warning, note };
        pub const ArgSpan = struct {
            index_span: isize = 1,
            offset: usize = 0,
        };
    };

    pub fn init(allocator: Allocator) Diagnostics {
        return .{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Diagnostics) void {
        for (self.errors.items) |*details| {
            details.msg.deinit(self.allocator);
        }
        self.errors.deinit(self.allocator);
    }

    pub fn append(self: *Diagnostics, error_details: ErrorDetails) !void {
        try self.errors.append(self.allocator, error_details);
    }

    pub fn renderToStdErr(self: *Diagnostics, args: []const []const u8) void {
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
            renderErrorMessage(stderr, colors, err_details, args) catch return;
        }
    }
};

pub const Options = struct {
    allocator: Allocator,
    input_filename: []const u8 = &[_]u8{},
    output_filename: []const u8 = &[_]u8{},
    extra_include_paths: std.ArrayListUnmanaged([]const u8) = .{},
    ignore_include_env_var: bool = false,
    preprocess: bool = true,
    default_language_id: ?u16 = null,
    default_code_page: ?CodePage = null,
    verbose: bool = false,

    pub fn deinit(self: *Options) void {
        for (self.extra_include_paths.items) |extra_include_path| {
            self.allocator.free(extra_include_path);
        }
        self.extra_include_paths.deinit(self.allocator);
        self.allocator.free(self.input_filename);
        self.allocator.free(self.output_filename);
    }
};

pub const Arg = struct {
    prefix: enum { long, short, slash },
    name: []const u8,
    full: []const u8,

    pub fn fromString(str: []const u8) ?@This() {
        if (std.mem.startsWith(u8, str, "--")) {
            return .{ .prefix = .long, .name = str[2..], .full = str };
        } else if (std.mem.startsWith(u8, str, "-")) {
            return .{ .prefix = .short, .name = str[1..], .full = str };
        } else if (std.mem.startsWith(u8, str, "/")) {
            return .{ .prefix = .slash, .name = str[1..], .full = str };
        }
        return null;
    }

    pub fn option(self: Arg, option_len: usize) []const u8 {
        return switch (self.prefix) {
            .long => self.full[0 .. option_len + 2],
            .short, .slash => self.full[0 .. option_len + 1],
        };
    }

    pub const Value = struct {
        slice: []const u8,
        index_increment: u2 = 1,

        pub fn argSpan(self: Value, arg: Arg) Diagnostics.ErrorDetails.ArgSpan {
            switch (self.index_increment) {
                1 => return .{ .offset = @ptrToInt(self.slice.ptr) - @ptrToInt(arg.full.ptr) },
                2 => return .{ .index_span = -1 },
                else => unreachable,
            }
        }

        pub fn index(self: Value, arg_index: usize) usize {
            if (self.index_increment == 2) return arg_index + 1;
            return arg_index;
        }
    };

    pub fn value(self: Arg, option_len: usize, index: usize, args: []const []const u8) error{MissingValue}!Value {
        const rest = args[index][self.option(option_len).len..];
        if (rest.len > 0) return .{ .slice = rest };
        if (index + 1 >= args.len) return error.MissingValue;
        return .{ .slice = args[index + 1], .index_increment = 2 };
    }

    pub const Context = struct {
        index: usize,
        arg: Arg,
        value: Value,
    };
};

pub const ParseError = error{ParseError} || Allocator.Error;

pub fn parse(allocator: Allocator, args: []const []const u8, diagnostics: *Diagnostics) ParseError!Options {
    var options = Options{ .allocator = allocator };
    errdefer options.deinit();

    var output_filename: ?[]const u8 = null;
    var output_filename_context: Arg.Context = undefined;

    var arg_i: usize = 1; // start at 1 to skip past the exe name
    while (arg_i < args.len) {
        const arg = Arg.fromString(args[arg_i]) orelse break;
        // -- on its own ends arg parsing
        if (arg.name.len == 0 and arg.prefix == .long) {
            arg_i += 1;
            break;
        }

        if (std.ascii.startsWithIgnoreCase(arg.name, "i")) {
            const value = arg.value(1, arg_i, args) catch {
                var err_details = Diagnostics.ErrorDetails{ .arg_index = arg_i + 1, .arg_span = .{ .index_span = -1 } };
                var msg_writer = err_details.msg.writer(allocator);
                try msg_writer.print("missing include path after {s} option", .{arg.option(1)});
                try diagnostics.append(err_details);
                return error.ParseError;
            };
            const path = value.slice;
            const duped = try allocator.dupe(u8, path);
            try options.extra_include_paths.append(options.allocator, duped);
            arg_i += value.index_increment;
        } else if (std.ascii.startsWithIgnoreCase(arg.name, "fo")) {
            const value = arg.value(2, arg_i, args) catch {
                var err_details = Diagnostics.ErrorDetails{ .arg_index = arg_i + 1, .arg_span = .{ .index_span = -1 } };
                var msg_writer = err_details.msg.writer(allocator);
                try msg_writer.print("missing output path after {s} option", .{arg.option(2)});
                try diagnostics.append(err_details);
                return error.ParseError;
            };
            output_filename_context = .{ .index = arg_i, .arg = arg, .value = value };
            output_filename = value.slice;
            arg_i += value.index_increment;
        }
        // Note: This must come before the "l" case to avoid becoming a dead branch
        else if (std.ascii.startsWithIgnoreCase(arg.name, "ln")) {
            const value = arg.value(2, arg_i, args) catch {
                var err_details = Diagnostics.ErrorDetails{ .arg_index = arg_i + 1, .arg_span = .{ .index_span = -1 } };
                var msg_writer = err_details.msg.writer(allocator);
                try msg_writer.print("missing language tag after {s} option", .{arg.option(2)});
                try diagnostics.append(err_details);
                return error.ParseError;
            };
            const tag = value.slice;
            options.default_language_id = lang.tagToInt(tag) catch {
                var err_details = Diagnostics.ErrorDetails{ .arg_index = value.index(arg_i), .arg_span = value.argSpan(arg) };
                var msg_writer = err_details.msg.writer(allocator);
                try msg_writer.print("invalid language tag: {s}", .{tag});
                try diagnostics.append(err_details);
                return error.ParseError;
            };
            if (options.default_language_id.? == lang.LOCALE_CUSTOM_UNSPECIFIED) {
                var err_details = Diagnostics.ErrorDetails{ .type = .warning, .arg_index = value.index(arg_i), .arg_span = value.argSpan(arg) };
                var msg_writer = err_details.msg.writer(allocator);
                try msg_writer.print("language tag '{s}' does not have an assigned ID so it will be resolved to LOCALE_CUSTOM_UNSPECIFIED (id=0x{x})", .{ tag, lang.LOCALE_CUSTOM_UNSPECIFIED });
                try diagnostics.append(err_details);
            }
            arg_i += value.index_increment;
        } else if (std.ascii.startsWithIgnoreCase(arg.name, "l")) {
            const value = arg.value(1, arg_i, args) catch {
                var err_details = Diagnostics.ErrorDetails{ .arg_index = arg_i + 1, .arg_span = .{ .index_span = -1 } };
                var msg_writer = err_details.msg.writer(allocator);
                try msg_writer.print("missing language ID after {s} option", .{arg.option(1)});
                try diagnostics.append(err_details);
                return error.ParseError;
            };
            const num_str = value.slice;
            options.default_language_id = lang.parseInt(num_str) catch {
                var err_details = Diagnostics.ErrorDetails{ .arg_index = value.index(arg_i), .arg_span = value.argSpan(arg) };
                var msg_writer = err_details.msg.writer(allocator);
                try msg_writer.print("invalid language ID: {s}", .{num_str});
                try diagnostics.append(err_details);
                return error.ParseError;
            };
            arg_i += value.index_increment;
        } else if (std.ascii.startsWithIgnoreCase(arg.name, "c")) {
            const value = arg.value(1, arg_i, args) catch {
                var err_details = Diagnostics.ErrorDetails{ .arg_index = arg_i + 1, .arg_span = .{ .index_span = -1 } };
                var msg_writer = err_details.msg.writer(allocator);
                try msg_writer.print("missing code page ID after {s} option", .{arg.option(1)});
                try diagnostics.append(err_details);
                return error.ParseError;
            };
            const num_str = value.slice;
            const code_page_id = std.fmt.parseUnsigned(u16, num_str, 10) catch {
                var err_details = Diagnostics.ErrorDetails{ .arg_index = value.index(arg_i), .arg_span = value.argSpan(arg) };
                var msg_writer = err_details.msg.writer(allocator);
                try msg_writer.print("invalid code page ID: {s}", .{num_str});
                try diagnostics.append(err_details);
                return error.ParseError;
            };
            options.default_code_page = CodePage.getByIdentifierEnsureSupported(code_page_id) catch |err| switch (err) {
                error.InvalidCodePage => {
                    var err_details = Diagnostics.ErrorDetails{ .arg_index = value.index(arg_i), .arg_span = value.argSpan(arg) };
                    var msg_writer = err_details.msg.writer(allocator);
                    try msg_writer.print("invalid or unknown code page ID: {}", .{code_page_id});
                    try diagnostics.append(err_details);
                    return error.ParseError;
                },
                error.UnsupportedCodePage => {
                    var err_details = Diagnostics.ErrorDetails{ .arg_index = value.index(arg_i), .arg_span = value.argSpan(arg) };
                    var msg_writer = err_details.msg.writer(allocator);
                    try msg_writer.print("unsupported code page: {s} (id={})", .{
                        @tagName(CodePage.getByIdentifier(code_page_id) catch unreachable),
                        code_page_id,
                    });
                    try diagnostics.append(err_details);
                    return error.ParseError;
                },
            };
            arg_i += value.index_increment;
        } else if (std.ascii.eqlIgnoreCase("v", arg.name)) {
            options.verbose = true;
            arg_i += 1;
        } else if (std.ascii.eqlIgnoreCase("x", arg.name)) {
            options.ignore_include_env_var = true;
            arg_i += 1;
        } else if (std.ascii.eqlIgnoreCase("no-preprocess", arg.name)) {
            options.preprocess = false;
            arg_i += 1;
        } else {
            break;
        }
    }

    var positionals = args[arg_i..];

    if (positionals.len < 1) {
        var err_details = Diagnostics.ErrorDetails{ .print_args = false, .arg_index = arg_i };
        var msg_writer = err_details.msg.writer(allocator);
        try msg_writer.writeAll("missing input filename");
        try diagnostics.append(err_details);
        return error.ParseError;
    }
    options.input_filename = try allocator.dupe(u8, positionals[0]);

    if (positionals.len > 1) {
        if (output_filename != null) {
            var err_details = Diagnostics.ErrorDetails{ .arg_index = arg_i + 1 };
            var msg_writer = err_details.msg.writer(allocator);
            try msg_writer.writeAll("output filename already specified");
            try diagnostics.append(err_details);
            var note_details = Diagnostics.ErrorDetails{
                .type = .note,
                .arg_index = output_filename_context.value.index(output_filename_context.index),
                .arg_span = output_filename_context.value.argSpan(output_filename_context.arg),
            };
            var note_writer = note_details.msg.writer(allocator);
            try note_writer.writeAll("output filename previously specified here");
            try diagnostics.append(note_details);
            return error.ParseError;
        }
        output_filename = positionals[1];
    }
    if (output_filename == null) {
        var output_filename_buf: [std.fs.MAX_NAME_BYTES]u8 = undefined;
        var filename_fbs = std.io.fixedBufferStream(&output_filename_buf);
        var filename_writer = filename_fbs.writer();
        // TODO: It's technically possible for this to overflow the buffer
        //       since input_filename is not guaranteed to be within MAX_NAME_BYTES
        filename_writer.writeAll(std.fs.path.stem(options.input_filename)) catch unreachable;
        filename_writer.writeAll(".res") catch unreachable;
        output_filename = output_filename_buf[0..filename_writer.context.pos];
    }

    options.output_filename = try allocator.dupe(u8, output_filename.?);

    return options;
}

pub fn renderErrorMessage(writer: anytype, colors: utils.Colors, err_details: Diagnostics.ErrorDetails, args: []const []const u8) !void {
    colors.set(writer, .dim);
    try writer.writeAll("<cli>");
    colors.set(writer, .reset);
    colors.set(writer, .bold);
    try writer.writeAll(": ");
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
    try writer.writeAll(err_details.msg.items);
    try writer.writeByte('\n');
    colors.set(writer, .reset);

    if (!err_details.print_args) {
        try writer.writeByte('\n');
        return;
    }

    colors.set(writer, .dim);
    const prefix = " ... ";
    try writer.writeAll(prefix);
    colors.set(writer, .reset);

    const positive_span = err_details.arg_span.index_span >= 0;
    const span_start = if (positive_span) err_details.arg_index else @intCast(usize, @as(i65, err_details.arg_index) + err_details.arg_span.index_span);
    const span_end = if (positive_span) err_details.arg_index + @intCast(usize, err_details.arg_span.index_span) else err_details.arg_index + 1;
    const span_slice = args[span_start..@min(args.len, span_end)];

    for (span_slice, 0..) |arg, i| {
        try writer.writeAll(arg);
        if (i < span_slice.len - 1) try writer.writeByte(' ');
    }
    if (span_end < args.len) {
        colors.set(writer, .dim);
        try writer.writeAll(prefix);
        colors.set(writer, .reset);
    }
    try writer.writeByte('\n');

    colors.set(writer, .green);
    try writer.writeByteNTimes(' ', prefix.len);
    if (err_details.arg_span.index_span < 0) {
        var num_squiggles_before: usize = 0;
        for (span_slice[0..std.math.absCast(err_details.arg_span.index_span)]) |arg_before| {
            num_squiggles_before += arg_before.len + 1;
        }
        try writer.writeByteNTimes('~', num_squiggles_before);
    }
    if (err_details.arg_span.offset > 0) {
        try writer.writeByteNTimes('~', err_details.arg_span.offset);
    }
    try writer.writeByte('^');
    // We sometimes want to point to a missing arg, in which case arg_index may be
    // outside of the range of args
    if (err_details.arg_index < args.len) {
        const pointed_to_arg = args[err_details.arg_index];
        if (pointed_to_arg.len - err_details.arg_span.offset > 1) {
            var num_squiggles = pointed_to_arg.len - err_details.arg_span.offset - 1;
            try writer.writeByteNTimes('~', num_squiggles);
        }
    }
    try writer.writeByte('\n');
    colors.set(writer, .reset);
}
