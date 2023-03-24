const std = @import("std");
const CodePage = @import("code_pages.zig").CodePage;
const lang = @import("lang.zig");

pub const Options = struct {
    allocator: std.mem.Allocator,
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

    pub fn fromString(str: []const u8) ?@This() {
        if (std.mem.startsWith(u8, str, "--")) {
            return .{ .prefix = .long, .name = str[2..] };
        } else if (std.mem.startsWith(u8, str, "-")) {
            return .{ .prefix = .short, .name = str[1..] };
        } else if (std.mem.startsWith(u8, str, "/")) {
            return .{ .prefix = .slash, .name = str[1..] };
        }
        return null;
    }
};

pub fn parse(allocator: std.mem.Allocator, args: []const []const u8) !Options {
    var options = Options{ .allocator = allocator };
    errdefer options.deinit();

    var output_filename: ?[]const u8 = null;

    var arg_i: usize = 1; // start at 1 to skip past the exe name
    while (arg_i < args.len) {
        const arg = Arg.fromString(args[arg_i]) orelse break;
        // -- on its own ends arg parsing
        if (arg.name.len == 0 and arg.prefix == .long) {
            arg_i += 1;
            break;
        }

        if (std.ascii.startsWithIgnoreCase(arg.name, "i")) {
            const rest = arg.name[1..];
            if (rest.len == 0 and arg_i + 1 >= args.len) {
                std.debug.print("Missing include path after {s} option\n", .{args[arg_i]});
                std.os.exit(1);
            }
            const path = switch (rest.len) {
                0 => args[arg_i + 1],
                else => rest,
            };
            const duped = try allocator.dupe(u8, path);
            try options.extra_include_paths.append(options.allocator, duped);
            arg_i += if (rest.len == 0) 2 else 1;
        } else if (std.ascii.startsWithIgnoreCase(arg.name, "fo")) {
            const rest = arg.name[2..];
            if (rest.len == 0 and arg_i + 1 >= args.len) {
                std.debug.print("Missing output path after {s} option\n", .{args[arg_i]});
                std.os.exit(1);
            }
            output_filename = switch (rest.len) {
                0 => args[arg_i + 1],
                else => rest,
            };
            arg_i += if (rest.len == 0) 2 else 1;
        }
        // Note: This must come before the "l" case to avoid becoming a dead branch
        else if (std.ascii.startsWithIgnoreCase(arg.name, "ln")) {
            const rest = arg.name[2..];
            if (rest.len == 0 and arg_i + 1 >= args.len) {
                std.debug.print("Missing language tag after {s} option\n", .{args[arg_i]});
                std.os.exit(1);
            }
            const tag = switch (rest.len) {
                0 => args[arg_i + 1],
                else => rest,
            };
            options.default_language_id = lang.tagToInt(tag) catch {
                std.debug.print("Invalid language tag: {s}\n", .{tag});
                std.os.exit(1);
            };
            if (options.default_language_id.? == lang.LOCALE_CUSTOM_UNSPECIFIED) {
                // TODO: Better formatting
                std.debug.print("Warning: language tag '{s}' does not have an assigned ID so it has been converted to LOCALE_CUSTOM_UNSPECIFIED (id=0x{x})\n", .{ tag, lang.LOCALE_CUSTOM_UNSPECIFIED });
            }
            arg_i += if (rest.len == 0) 2 else 1;
        } else if (std.ascii.startsWithIgnoreCase(arg.name, "l")) {
            const rest = arg.name[1..];
            if (rest.len == 0 and arg_i + 1 >= args.len) {
                std.debug.print("Missing language ID after {s} option\n", .{args[arg_i]});
                std.os.exit(1);
            }
            const num_str = switch (rest.len) {
                0 => args[arg_i + 1],
                else => rest,
            };
            options.default_language_id = lang.parseInt(num_str) catch {
                std.debug.print("Invalid language ID: {s}\n", .{num_str});
                std.os.exit(1);
            };
            arg_i += if (rest.len == 0) 2 else 1;
        } else if (std.ascii.startsWithIgnoreCase(arg.name, "c")) {
            const rest = arg.name[1..];
            if (rest.len == 0 and arg_i + 1 >= args.len) {
                std.debug.print("Missing code page ID after {s} option\n", .{args[arg_i]});
                std.os.exit(1);
            }
            const num_str = switch (rest.len) {
                0 => args[arg_i + 1],
                else => rest,
            };
            const code_page_id = std.fmt.parseUnsigned(u16, num_str, 10) catch {
                std.debug.print("Invalid code page ID: {s}\n", .{num_str});
                std.os.exit(1);
            };
            options.default_code_page = CodePage.getByIdentifierEnsureSupported(code_page_id) catch |err| switch (err) {
                error.InvalidCodePage => {
                    std.debug.print("Invalid or unknown code page ID: {}\n", .{code_page_id});
                    std.os.exit(1);
                },
                error.UnsupportedCodePage => {
                    std.debug.print("Unsupported code page: {s} (id={})\n", .{
                        @tagName(CodePage.getByIdentifier(code_page_id) catch unreachable),
                        code_page_id,
                    });
                    std.os.exit(1);
                },
            };
            arg_i += if (rest.len == 0) 2 else 1;
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
        std.debug.print("Missing input filename\n", .{});
        std.os.exit(1);
    }
    options.input_filename = try allocator.dupe(u8, positionals[0]);

    if (positionals.len > 1) {
        if (output_filename != null) {
            std.debug.print("Output filename already specified with the /fo option\n", .{});
            std.os.exit(1);
        }
        output_filename = positionals[1];
    }
    if (output_filename == null) {
        var output_filename_buf: [std.fs.MAX_NAME_BYTES]u8 = undefined;
        var filename_fbs = std.io.fixedBufferStream(&output_filename_buf);
        var filename_writer = filename_fbs.writer();
        try filename_writer.writeAll(std.fs.path.stem(options.input_filename));
        try filename_writer.writeAll(".res");
        output_filename = output_filename_buf[0..filename_writer.context.pos];
    }

    options.output_filename = try allocator.dupe(u8, output_filename.?);

    return options;
}
