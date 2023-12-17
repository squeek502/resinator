const std = @import("std");
const removeComments = @import("comments.zig").removeComments;
const parseAndRemoveLineCommands = @import("source_mapping.zig").parseAndRemoveLineCommands;
const compile = @import("compile.zig").compile;
const Diagnostics = @import("errors.zig").Diagnostics;
const cli = @import("cli.zig");
const parse = @import("parse.zig");
const lex = @import("lex.zig");
const preprocess = @import("preprocess.zig");
const renderErrorMessage = @import("utils.zig").renderErrorMessage;
const aro = @import("aro");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .stack_trace_frames = 8 }){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    // Set the codepage to UTF-8 unconditionally to ensure that everything renders okay
    // TODO: Reset codepage afterwards?
    if (@import("builtin").os.tag == .windows) {
        _ = std.os.windows.kernel32.SetConsoleOutputCP(65001);
    }
    const stderr = std.io.getStdErr();
    const stderr_config = std.io.tty.detectConfig(stderr);

    var options = options: {
        const args = try std.process.argsAlloc(allocator);
        defer std.process.argsFree(allocator, args);

        var cli_diagnostics = cli.Diagnostics.init(allocator);
        defer cli_diagnostics.deinit();
        var options = cli.parse(allocator, args, &cli_diagnostics) catch |err| switch (err) {
            error.ParseError => {
                cli_diagnostics.renderToStdErr(args, stderr_config);
                std.os.exit(1);
            },
            else => |e| return e,
        };
        try options.maybeAppendRC(std.fs.cwd());

        // print any warnings/notes
        cli_diagnostics.renderToStdErr(args, stderr_config);
        // If there was something printed, then add an extra newline separator
        // so that there is a clear separation between the cli diagnostics and whatever
        // gets printed after
        if (cli_diagnostics.errors.items.len > 0) {
            std.debug.print("\n", .{});
        }
        break :options options;
    };
    defer options.deinit();

    if (options.print_help_and_exit) {
        try cli.writeUsage(stderr.writer(), "resinator");
        return;
    }

    const stdout_writer = std.io.getStdOut().writer();
    if (options.verbose) {
        try options.dumpVerbose(stdout_writer);
        try stdout_writer.writeByte('\n');
    }

    const full_input = full_input: {
        if (options.preprocess != .no) {
            // TODO: replace include detection with `windows_sdk.zig`-alike
            if (options.auto_includes != .none and !(try Preprocessor.zigSupportsLibcIncludesOption(allocator))) {
                try renderErrorMessage(stderr.writer(), stderr_config, .err, "the version of zig being used for preprocessing cannot autodetect include paths", .{});
                try renderErrorMessage(stderr.writer(), stderr_config, .note, "zig must support the -includes option of the 'zig libc' subcommand to autodetect include paths", .{});
                try renderErrorMessage(stderr.writer(), stderr_config, .note, "support for the -includes option was added in zig version 0.12.0-dev.378+4f952c7e0", .{});
                std.os.exit(1);
            }

            const include_args = Preprocessor.getIncludeArgs(allocator, options.auto_includes) catch |err| switch (err) {
                error.OutOfMemory => |e| return e,
                else => {
                    try renderErrorMessage(stderr.writer(), stderr_config, .err, "unable to autodetect include paths: {s}", .{@errorName(err)});
                    std.os.exit(1);
                },
            };
            defer include_args.deinit(allocator);

            var comp = aro.Compilation.init(allocator);
            defer comp.deinit();

            var preprocessed_buf = std.ArrayList(u8).init(allocator);
            errdefer preprocessed_buf.deinit();

            var argv = std.ArrayList([]const u8).init(comp.gpa);
            defer argv.deinit();

            var args_arena = std.heap.ArenaAllocator.init(allocator);
            defer args_arena.deinit();

            try argv.append("arocc"); // dummy command name
            try preprocess.appendAroArgs(args_arena.allocator(), &argv, options, include_args.include_paths);
            try argv.append(options.input_filename);

            if (options.verbose) {
                try stdout_writer.writeAll("Preprocessor: arocc (built-in)\n");
                for (argv.items[0 .. argv.items.len - 1]) |arg| {
                    try stdout_writer.print("{s} ", .{arg});
                }
                try stdout_writer.print("{s}\n\n", .{argv.items[argv.items.len - 1]});
            }

            preprocess.preprocess(&comp, preprocessed_buf.writer(), argv.items) catch |err| switch (err) {
                error.ArgError => {
                    // extra newline to separate this line from the aro errors
                    try renderErrorMessage(stderr.writer(), stderr_config, .err, "failed during preprocessor argument parsing (this is always a bug):\n", .{});
                    aro.Diagnostics.render(&comp, stderr_config);
                    std.os.exit(1);
                },
                error.GeneratedSourceError => {
                    // extra newline to separate this line from the aro errors
                    try renderErrorMessage(stderr.writer(), stderr_config, .err, "failed during preprocessor setup (this is always a bug):\n", .{});
                    aro.Diagnostics.render(&comp, stderr_config);
                    std.os.exit(1);
                },
                error.PreprocessError => {
                    // extra newline to separate this line from the aro errors
                    try renderErrorMessage(stderr.writer(), stderr_config, .err, "failed during preprocessing:\n", .{});
                    aro.Diagnostics.render(&comp, stderr_config);
                    std.os.exit(1);
                },
                error.StreamTooLong => {
                    try renderErrorMessage(stderr.writer(), stderr_config, .err, "failed during preprocessing: maximum file size exceeded", .{});
                    std.os.exit(1);
                },
                error.OutOfMemory => |e| return e,
            };

            break :full_input try preprocessed_buf.toOwnedSlice();
        } else {
            break :full_input std.fs.cwd().readFileAlloc(allocator, options.input_filename, std.math.maxInt(usize)) catch |err| {
                try renderErrorMessage(stderr.writer(), stderr_config, .err, "unable to read input file path '{s}': {s}", .{ options.input_filename, @errorName(err) });
                std.os.exit(1);
            };
        }
    };
    defer allocator.free(full_input);

    if (options.preprocess == .only) {
        try std.fs.cwd().writeFile(options.output_filename, full_input);
        return;
    }

    // Note: We still want to run this when no-preprocess is set because:
    //   1. We want to print accurate line numbers after removing multiline comments
    //   2. We want to be able to handle an already-preprocessed input with #line commands in it
    var mapping_results = try parseAndRemoveLineCommands(allocator, full_input, full_input, .{ .initial_filename = options.input_filename });
    defer mapping_results.mappings.deinit(allocator);

    // TODO: Need to test to make sure that the parsing of the #line directives match
    //       the initial_filename given to parseAndRemoveLineCommands.
    // They *should* match, as the clang preprocessor inserts whatever filename
    // you give it into the #line directives (e.g. `.\./rCDaTA.rC` for a file called
    // `rcdata.rc` will get a `#line 1 ".\\./rCDaTA.rC"` directive), but there still
    // may be a mismatch in how the line directive strings are parsed versus
    // how they are escaped/written by the preprocessor.

    const final_input = removeComments(mapping_results.result, mapping_results.result, &mapping_results.mappings) catch |err| switch (err) {
        error.InvalidSourceMappingCollapse => {
            try renderErrorMessage(stderr.writer(), stderr_config, .err, "failed during comment removal; this is a known bug", .{});
            std.os.exit(1);
        },
        else => |e| return e,
    };

    var output_file = std.fs.cwd().createFile(options.output_filename, .{}) catch |err| {
        try renderErrorMessage(stderr.writer(), stderr_config, .err, "unable to create output file '{s}': {s}", .{ options.output_filename, @errorName(err) });
        std.os.exit(1);
    };
    var output_file_closed = false;
    defer if (!output_file_closed) output_file.close();

    var diagnostics = Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var dependencies_list = std.ArrayList([]const u8).init(allocator);
    defer {
        for (dependencies_list.items) |item| {
            allocator.free(item);
        }
        dependencies_list.deinit();
    }

    if (options.debug) {
        std.debug.print("after preprocessor:\n------------------\n{s}\n------------------\n", .{final_input});
        std.debug.print("\nmappings:\n", .{});
        var it = mapping_results.mappings.sources.inorderIterator();
        while (it.next()) |node| {
            const source = node.key;
            const filename = mapping_results.mappings.files.get(source.filename_offset);
            std.debug.print("{}: {s} : {}-{}\n", .{ source.start_line, filename, source.corresponding_start_line, source.corresponding_start_line + source.span });
        }
        std.debug.print("end line #: {}\n", .{mapping_results.mappings.end_line});
        std.debug.print("\n", .{});

        // Separately parse and dump the AST
        ast: {
            var parse_diagnostics = Diagnostics.init(allocator);
            defer parse_diagnostics.deinit();
            var lexer = lex.Lexer.init(final_input, .{});
            var parser = parse.Parser.init(&lexer, .{});
            var tree = parser.parse(allocator, &parse_diagnostics) catch {
                std.debug.print("Failed to parse\n", .{});
                break :ast;
            };
            defer tree.deinit();

            try tree.dump(stderr.writer());
            std.debug.print("\n", .{});
        }
    }

    var output_buffered_stream = std.io.bufferedWriter(output_file.writer());

    compile(allocator, final_input, output_buffered_stream.writer(), .{
        .cwd = std.fs.cwd(),
        .diagnostics = &diagnostics,
        .source_mappings = &mapping_results.mappings,
        .dependencies_list = if (options.debug) &dependencies_list else null,
        .ignore_include_env_var = options.ignore_include_env_var,
        .extra_include_paths = options.extra_include_paths.items,
        .default_language_id = options.default_language_id,
        .default_code_page = options.default_code_page orelse .windows1252,
        .verbose = options.verbose,
        .null_terminate_string_table_strings = options.null_terminate_string_table_strings,
        .max_string_literal_codepoints = options.max_string_literal_codepoints,
        .silent_duplicate_control_ids = options.silent_duplicate_control_ids,
        .warn_instead_of_error_on_invalid_code_page = options.warn_instead_of_error_on_invalid_code_page,
    }) catch |err| switch (err) {
        error.ParseError, error.CompileError => {
            diagnostics.renderToStdErr(std.fs.cwd(), final_input, stderr_config, mapping_results.mappings);
            // Delete the output file on error
            output_file.close();
            output_file_closed = true;
            // Failing to delete is not really a big deal, so swallow any errors
            std.fs.cwd().deleteFile(options.output_filename) catch {};
            std.os.exit(1);
        },
        else => |e| return e,
    };

    try output_buffered_stream.flush();

    if (options.debug) {
        std.debug.print("dependencies list:\n", .{});
        for (dependencies_list.items) |path| {
            std.debug.print(" {s}\n", .{path});
        }
        std.debug.print("\n", .{});
    }

    // print any warnings/notes
    diagnostics.renderToStdErr(std.fs.cwd(), final_input, stderr_config, mapping_results.mappings);
}

const Preprocessor = struct {
    var zig_supports_libc_includes_option: ?bool = null;

    pub const IncludeArgs = struct {
        nostdinc: bool = false,
        include_paths: []const []const u8 = &.{},
        // Passing an explicit target will get clang to do autodetection of include dirs
        // as long as -nostdinc is not passed as well.
        target: ?[]const u8 = "x86_64-unknown-windows",

        pub fn deinit(self: IncludeArgs, allocator: std.mem.Allocator) void {
            for (self.include_paths) |include_path| {
                allocator.free(include_path);
            }
            allocator.free(self.include_paths);
        }
    };

    pub fn getIncludeArgs(allocator: std.mem.Allocator, auto_includes: cli.Options.AutoIncludes) !IncludeArgs {
        if (auto_includes == .none) return .{ .nostdinc = true };
        if (!(try zigSupportsLibcIncludesOption(allocator))) {
            return .{};
        }

        var cur_includes = auto_includes;
        while (true) {
            switch (cur_includes) {
                .any, .msvc => {
                    return .{
                        .nostdinc = true,
                        .include_paths = getIncludeDirsFromZig(allocator, "native-windows-msvc") catch |err| {
                            if (cur_includes == .any) {
                                // fall back to mingw
                                cur_includes = .gnu;
                                continue;
                            }
                            return err;
                        },
                        .target = "x86_64-unknown-windows-msvc",
                    };
                },
                .gnu => {
                    return .{
                        .nostdinc = true,
                        .include_paths = try getIncludeDirsFromZig(allocator, "native-windows-gnu"),
                        .target = "x86_64-unknown-windows-gnu",
                    };
                },
                .none => unreachable,
            }
        }
    }

    fn zigSupportsLibcIncludesOption(allocator: std.mem.Allocator) !bool {
        if (Preprocessor.zig_supports_libc_includes_option == null) {
            const result = std.ChildProcess.run(.{
                .allocator = allocator,
                .argv = &.{ "zig", "libc", "-includes" },
                .max_output_bytes = std.math.maxInt(u16),
            }) catch |err| switch (err) {
                error.OutOfMemory => |e| return e,
                else => return false,
            };
            defer allocator.free(result.stdout);
            defer allocator.free(result.stderr);

            switch (result.term) {
                .Exited => |code| if (code == 0) return true,
                .Signal, .Stopped, .Unknown => return false,
            }

            Preprocessor.zig_supports_libc_includes_option = std.mem.indexOf(u8, result.stderr, "error: unrecognized parameter") == null;
        }
        return Preprocessor.zig_supports_libc_includes_option.?;
    }

    fn getIncludeDirsFromZig(allocator: std.mem.Allocator, target: []const u8) ![]const []const u8 {
        const result = try std.ChildProcess.run(.{
            .allocator = allocator,
            .argv = &.{ "zig", "libc", "-includes", "-target", target },
            .max_output_bytes = std.math.maxInt(u16),
        });
        defer allocator.free(result.stdout);
        defer allocator.free(result.stderr);

        switch (result.term) {
            .Exited => |code| if (code != 0) return error.NonZeroExitCode,
            .Signal, .Stopped, .Unknown => return error.UnexpectedStop,
        }

        var line_it = std.mem.splitScalar(u8, result.stdout, '\n');
        var includes = std.ArrayList([]const u8).init(allocator);
        errdefer {
            for (includes.items) |include| {
                allocator.free(include);
            }
            includes.deinit();
        }
        while (line_it.next()) |line| {
            if (line.len > 0) {
                const duped_line = try allocator.dupe(u8, line);
                errdefer allocator.free(duped_line);
                try includes.append(duped_line);
            }
        }
        return includes.toOwnedSlice();
    }
};
