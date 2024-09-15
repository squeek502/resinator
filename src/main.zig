const std = @import("std");
const builtin = @import("builtin");
const removeComments = @import("comments.zig").removeComments;
const parseAndRemoveLineCommands = @import("source_mapping.zig").parseAndRemoveLineCommands;
const compile = @import("compile.zig").compile;
const Diagnostics = @import("errors.zig").Diagnostics;
const cli = @import("cli.zig");
const parse = @import("parse.zig");
const lex = @import("lex.zig");
const preprocess = @import("preprocess.zig");
const renderErrorMessage = @import("utils.zig").renderErrorMessage;
const auto_includes = @import("auto_includes.zig");
const aro = @import("aro");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .stack_trace_frames = 8 }){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    var arena_state = std.heap.ArenaAllocator.init(allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    // Set the codepage to UTF-8 unconditionally to ensure that everything renders okay
    // TODO: Reset codepage afterwards?
    if (builtin.os.tag == .windows) {
        _ = std.os.windows.kernel32.SetConsoleOutputCP(65001);
    }
    const stderr = std.io.getStdErr();
    const stderr_config = std.io.tty.detectConfig(stderr);

    var options = options: {
        const all_args = try std.process.argsAlloc(allocator);
        defer std.process.argsFree(allocator, all_args);
        const args = all_args[1..]; // skip past the executable name

        var cli_diagnostics = cli.Diagnostics.init(allocator);
        defer cli_diagnostics.deinit();
        var options = cli.parse(allocator, args, &cli_diagnostics) catch |err| switch (err) {
            error.ParseError => {
                cli_diagnostics.renderToStdErr(args, stderr_config);
                std.process.exit(1);
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

    var dependencies_list = std.ArrayList([]const u8).init(allocator);
    defer {
        for (dependencies_list.items) |item| {
            allocator.free(item);
        }
        dependencies_list.deinit();
    }
    const maybe_dependencies_list: ?*std.ArrayList([]const u8) = if (options.depfile_path != null) &dependencies_list else null;

    const include_paths = getIncludePaths(arena, options.auto_includes, options.mingw_includes_dir) catch |err| switch (err) {
        error.OutOfMemory => |e| return e,
        else => {
            switch (err) {
                error.MsvcIncludesNotFound => {
                    try renderErrorMessage(stderr.writer(), stderr_config, .err, "MSVC include paths could not be automatically detected", .{});
                },
                error.CannotResolveCachePath => {
                    try renderErrorMessage(stderr.writer(), stderr_config, .err, "could not resolve global cache path (for MinGW auto includes)", .{});
                },
                // All other errors are related to MinGW includes
                else => {
                    try renderErrorMessage(stderr.writer(), stderr_config, .err, "failed to find / extract cached MinGW includes: {s}", .{@errorName(err)});
                },
            }
            try renderErrorMessage(stderr.writer(), stderr_config, .note, "to disable auto includes, use the option /:auto-includes none", .{});
            std.process.exit(1);
        },
    };

    const full_input = full_input: {
        if (options.preprocess != .no) {
            var preprocessed_buf = std.ArrayList(u8).init(allocator);
            errdefer preprocessed_buf.deinit();

            // We're going to throw away everything except the final preprocessed output anyway,
            // so we can use a scoped arena for everything else.
            var aro_arena_state = std.heap.ArenaAllocator.init(allocator);
            defer aro_arena_state.deinit();
            const aro_arena = aro_arena_state.allocator();

            var comp = aro.Compilation.init(aro_arena, std.fs.cwd());
            defer comp.deinit();

            var argv = std.ArrayList([]const u8).init(comp.gpa);
            defer argv.deinit();

            try argv.append("arocc"); // dummy command name
            try preprocess.appendAroArgs(aro_arena, &argv, options, include_paths);
            try argv.append(options.input_filename);

            if (options.verbose) {
                try stdout_writer.writeAll("Preprocessor: arocc (built-in)\n");
                for (argv.items[0 .. argv.items.len - 1]) |arg| {
                    try stdout_writer.print("{s} ", .{arg});
                }
                try stdout_writer.print("{s}\n\n", .{argv.items[argv.items.len - 1]});
            }

            preprocess.preprocess(&comp, preprocessed_buf.writer(), argv.items, maybe_dependencies_list) catch |err| switch (err) {
                error.GeneratedSourceError => {
                    // extra newline to separate this line from the aro errors
                    try renderErrorMessage(stderr.writer(), stderr_config, .err, "failed during preprocessor setup (this is always a bug):\n", .{});
                    aro.Diagnostics.render(&comp, stderr_config);
                    std.process.exit(1);
                },
                // ArgError can occur if e.g. the .rc file is not found
                error.ArgError, error.PreprocessError => {
                    // extra newline to separate this line from the aro errors
                    try renderErrorMessage(stderr.writer(), stderr_config, .err, "failed during preprocessing:\n", .{});
                    aro.Diagnostics.render(&comp, stderr_config);
                    std.process.exit(1);
                },
                error.StreamTooLong => {
                    try renderErrorMessage(stderr.writer(), stderr_config, .err, "failed during preprocessing: maximum file size exceeded", .{});
                    std.process.exit(1);
                },
                error.OutOfMemory => |e| return e,
            };

            break :full_input try preprocessed_buf.toOwnedSlice();
        } else {
            break :full_input std.fs.cwd().readFileAlloc(allocator, options.input_filename, std.math.maxInt(usize)) catch |err| {
                try renderErrorMessage(stderr.writer(), stderr_config, .err, "unable to read input file path '{s}': {s}", .{ options.input_filename, @errorName(err) });
                std.process.exit(1);
            };
        }
    };
    defer allocator.free(full_input);

    if (options.preprocess == .only) {
        try std.fs.cwd().writeFile(.{ .sub_path = options.output_filename, .data = full_input });
        return;
    }

    // Note: We still want to run this when no-preprocess is set because:
    //   1. We want to print accurate line numbers after removing multiline comments
    //   2. We want to be able to handle an already-preprocessed input with #line commands in it
    var mapping_results = parseAndRemoveLineCommands(allocator, full_input, full_input, .{ .initial_filename = options.input_filename }) catch |err| switch (err) {
        error.InvalidLineCommand => {
            // TODO: Better error message
            try renderErrorMessage(stderr.writer(), stderr_config, .err, "invalid line command in the preprocessed source", .{});
            std.process.exit(1);
        },
        error.LineNumberOverflow => {
            // TODO: Better error message
            try renderErrorMessage(stderr.writer(), stderr_config, .err, "line number count exceeded maximum of {}", .{std.math.maxInt(usize)});
            std.process.exit(1);
        },
        error.OutOfMemory => |e| return e,
    };
    defer mapping_results.mappings.deinit(allocator);

    const final_input = try removeComments(mapping_results.result, mapping_results.result, &mapping_results.mappings);

    var output_file = std.fs.cwd().createFile(options.output_filename, .{}) catch |err| {
        try renderErrorMessage(stderr.writer(), stderr_config, .err, "unable to create output file '{s}': {s}", .{ options.output_filename, @errorName(err) });
        std.process.exit(1);
    };
    var output_file_closed = false;
    defer if (!output_file_closed) output_file.close();

    var diagnostics = Diagnostics.init(allocator);
    defer diagnostics.deinit();

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
        .dependencies_list = maybe_dependencies_list,
        .ignore_include_env_var = options.ignore_include_env_var,
        .extra_include_paths = options.extra_include_paths.items,
        .system_include_paths = include_paths,
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
            std.process.exit(1);
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

    // write the depfile
    if (options.depfile_path) |depfile_path| {
        var depfile = std.fs.cwd().createFile(depfile_path, .{}) catch |err| {
            try renderErrorMessage(stderr.writer(), stderr_config, .err, "unable to create depfile '{s}': {s}", .{ depfile_path, @errorName(err) });
            std.process.exit(1);
        };
        defer depfile.close();

        const depfile_writer = depfile.writer();
        var depfile_buffered_writer = std.io.bufferedWriter(depfile_writer);
        switch (options.depfile_fmt) {
            .json => {
                var write_stream = std.json.writeStream(depfile_buffered_writer.writer(), .{ .whitespace = .indent_2 });
                defer write_stream.deinit();

                try write_stream.beginArray();
                for (dependencies_list.items) |dep_path| {
                    try write_stream.write(dep_path);
                }
                try write_stream.endArray();
            },
        }
        try depfile_buffered_writer.flush();
    }
}

fn getIncludePaths(allocator: std.mem.Allocator, auto_includes_option: cli.Options.AutoIncludes, maybe_mingw_includes_dir: ?[]const u8) ![]const []const u8 {
    var includes = auto_includes_option;
    if (builtin.target.os.tag != .windows) {
        switch (includes) {
            // MSVC can't be found when the host isn't Windows, so short-circuit.
            .msvc => return error.MsvcIncludesNotFound,
            // Skip straight to gnu since we won't be able to detect MSVC on non-Windows hosts.
            .any => includes = .gnu,
            .none, .gnu => {},
        }
    }

    while (true) {
        switch (includes) {
            .none => return &[_][]const u8{},
            .any, .msvc => {
                // MSVC is only detectable on Windows targets. This unreachable is to signify
                // that .any and .msvc should be dealt with on non-Windows targets before this point,
                // since getting MSVC include paths uses Windows-only APIs.
                if (builtin.target.os.tag != .windows) unreachable;
                return auto_includes.getMsvcIncludePaths(allocator) catch |err| switch (err) {
                    error.OutOfMemory => |e| return e,
                    error.MsvcIncludesNotFound => {
                        if (includes == .any) {
                            // fall back to MinGW
                            includes = .gnu;
                            continue;
                        }
                        return err;
                    },
                };
            },
            .gnu => {
                const include_path = include_path: {
                    if (maybe_mingw_includes_dir) |mingw_includes_dir| {
                        break :include_path try allocator.dupe(u8, mingw_includes_dir);
                    } else {
                        const root_node = std.Progress.start(.{
                            .root_name = "auto includes",
                        });
                        break :include_path try auto_includes.extractMingwIncludes(allocator, root_node);
                    }
                };
                errdefer allocator.free(include_path);

                var include_paths = try allocator.alloc([]const u8, 1);
                include_paths[0] = include_path;
                return include_paths;
            },
        }
    }
}
