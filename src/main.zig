const std = @import("std");
const removeComments = @import("comments.zig").removeComments;
const ParseLineCommandsResult = @import("source_mapping.zig").ParseLineCommandsResult;
const parseAndRemoveLineCommands = @import("source_mapping.zig").parseAndRemoveLineCommands;
const SourceMappings = @import("source_mapping.zig").SourceMappings;
const compile = @import("compile.zig").compile;
const Diagnostics = @import("errors.zig").Diagnostics;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .stack_trace_frames = 8 }){};
    defer std.debug.assert(gpa.deinit() == false);
    const allocator = gpa.allocator();

    var args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var output_filename: ?[]const u8 = null;
    var output_filename_buf: [std.fs.MAX_NAME_BYTES]u8 = undefined;
    var extra_include_paths = std.ArrayList([]const u8).init(allocator);
    defer extra_include_paths.deinit();
    var ignore_include_env_var = false;
    var preprocess = true;

    const Arg = struct {
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
            try extra_include_paths.append(switch (rest.len) {
                0 => args[arg_i + 1],
                else => rest,
            });
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
        } else if (std.ascii.eqlIgnoreCase("x", arg.name)) {
            ignore_include_env_var = true;
            arg_i += 1;
        } else if (std.ascii.eqlIgnoreCase("no-preprocess", arg.name)) {
            preprocess = false;
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
    const input_filename = positionals[0];

    if (positionals.len > 1) {
        if (output_filename != null) {
            std.debug.print("Output filename already specified with the /fo option\n", .{});
            std.os.exit(1);
        }
        output_filename = positionals[1];
    }
    if (output_filename == null) {
        var filename_fbs = std.io.fixedBufferStream(&output_filename_buf);
        var filename_writer = filename_fbs.writer();
        try filename_writer.writeAll(std.fs.path.stem(input_filename));
        try filename_writer.writeAll(".res");
        output_filename = output_filename_buf[0..filename_writer.context.pos];
    }

    // TODO: I'm getting lost with where the SourceMappings memory lives and result location stuff.
    //       There's got to be a better way!
    //       Plan: - Make parseAndRemoveLineCommands take a SourceMappings ptr to insert into
    //             - Create a SourceMappings on the stack here and populate it when
    //               the preprocessor is turned off too, since we still want to know
    //               the line mappings when multiline comments are removed from the input.
    var input: Input = undefined;
    if (preprocess) {
        var argv = std.ArrayList([]const u8).init(allocator);
        defer argv.deinit();

        try argv.appendSlice(&[_][]const u8{
            "clang",
            "-E", // preprocessor only
            "--comments",
            "-fuse-line-directives", // #line <num> instead of # <num>
            // TODO: could use --trace-includes to give info about what's included from where
            "-xc", // output c
            // TODO: Turn this off, check the warnings, and convert the spaces back to NUL
            "-Werror=null-character", // error on null characters instead of converting them to spaces
            // TODO: could remove -Werror=null-character and instead parse warnings looking for 'warning: null character ignored'
            //       since the only real problem is when clang doesn't preserve null characters
            //"-Werror=invalid-pp-token", // will error on unfinished string literals
            // TODO: could use -Werror instead
            // https://learn.microsoft.com/en-us/windows/win32/menurc/predefined-macros
            "-DRC_INVOKED",
        });
        for (extra_include_paths.items) |extra_include_path| {
            try argv.append("--include-directory");
            try argv.append(extra_include_path);
        }
        try argv.append(input_filename);

        var result = try std.ChildProcess.exec(.{
            .allocator = allocator,
            .argv = argv.items,
            .max_output_bytes = std.math.maxInt(u32),
        });
        errdefer allocator.free(result.stdout);
        defer allocator.free(result.stderr);

        switch (result.term) {
            .Exited => |code| {
                if (code != 0) {
                    // TODO: Better formatting
                    std.debug.print("Preprocessor errors:\n{s}\n", .{result.stderr});
                    return error.ExitCodeFailure;
                }
            },
            .Signal, .Stopped, .Unknown => {
                return error.ClangProcessTerminated;
            },
        }

        input.buf = result.stdout;
        input.result = .{ .preprocessed = try parseAndRemoveLineCommands(allocator, input.buf, input.buf) };
        errdefer input.result.preprocessed.mappings.deinit(allocator);

        // Set the root file
        if (input.result.preprocessed.mappings.files.getOffset(input_filename)) |root_filename_offset| {
            input.result.preprocessed.mappings.root_filename_offset = root_filename_offset;
        } else {
            // This *should* be impossible, as the clang preprocessor inserts whatever filename
            // you give it into the #line directives (e.g. `.\./rCDaTA.rC` for a file called
            // `rcdata.rc` will get a `#line 1 ".\\./rCDaTA.rC"` directive), but this may still
            // happen if there is a mismatch in how the line directive strings are parsed versus
            // how they are escaped/written by the preprocessor.
            std.debug.print("input filename not found in source mappings: {s}\n", .{input_filename});
            @panic("Internal error (this is a bug)");
        }
    } else {
        input.buf = try std.fs.cwd().readFileAlloc(allocator, input_filename, std.math.maxInt(usize));
        errdefer allocator.free(input.buf);

        input.result = .{ .raw = input.buf };
    }
    defer input.deinit(allocator);

    var final_input = removeComments(input.resultSlice(), input.resultSlice(), input.mappingsPtr());

    var output_file = try std.fs.cwd().createFile(output_filename.?, .{});
    defer output_file.close();

    var diagnostics = Diagnostics.init(allocator);
    defer diagnostics.deinit();

    // std.debug.print("after preprocessor:\n------------------\n{s}\n------------------\n", .{final_input});
    // if (input.mappingsPtr()) |mappings| {
    //     std.debug.print("\nmappings:\n", .{});
    //     for (mappings.mapping.items, 0..) |span, i| {
    //         const line_num = i + 1;
    //         const filename = mappings.files.get(span.filename_offset);
    //         std.debug.print("{}: {s}:{}-{}\n", .{ line_num, filename, span.start_line, span.end_line });
    //     }
    // }
    // std.debug.print("\n", .{});

    compile(allocator, final_input, output_file.writer(), .{
        .cwd = std.fs.cwd(),
        .diagnostics = &diagnostics,
        .source_mappings = input.mappingsPtr(),
        .ignore_include_env_var = ignore_include_env_var,
        .extra_include_paths = extra_include_paths.items,
    }) catch |err| switch (err) {
        error.ParseError, error.CompileError => {
            diagnostics.renderToStdErr(std.fs.cwd(), final_input, input.mappings());
            std.os.exit(1);
        },
        else => |e| return e,
    };

    // print any warnings/notes
    diagnostics.renderToStdErr(std.fs.cwd(), final_input, input.mappings());
}

const Input = struct {
    buf: []u8,
    result: union(enum) {
        raw: []u8,
        preprocessed: ParseLineCommandsResult,
    },

    pub fn resultSlice(self: *Input) []u8 {
        return switch (self.result) {
            .raw => |s| s,
            .preprocessed => |r| r.result,
        };
    }

    pub fn mappingsPtr(self: *Input) ?*SourceMappings {
        return switch (self.result) {
            .raw => null,
            .preprocessed => |*r| return &r.mappings,
        };
    }

    pub fn mappings(self: *Input) ?SourceMappings {
        return switch (self.result) {
            .raw => null,
            .preprocessed => |r| return r.mappings,
        };
    }

    pub fn deinit(self: *Input, allocator: std.mem.Allocator) void {
        allocator.free(self.buf);
        switch (self.result) {
            .preprocessed => |*preprocessed| {
                preprocessed.mappings.deinit(allocator);
            },
            .raw => {},
        }
    }
};
