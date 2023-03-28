const std = @import("std");
const removeComments = @import("comments.zig").removeComments;
const parseAndRemoveLineCommands = @import("source_mapping.zig").parseAndRemoveLineCommands;
const compile = @import("compile.zig").compile;
const Diagnostics = @import("errors.zig").Diagnostics;
const cli = @import("cli.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .stack_trace_frames = 8 }){};
    defer std.debug.assert(gpa.deinit() == false);
    const allocator = gpa.allocator();

    var options = options: {
        var args = try std.process.argsAlloc(allocator);
        defer std.process.argsFree(allocator, args);

        var cli_diagnostics = cli.Diagnostics.init(allocator);
        defer cli_diagnostics.deinit();
        var options = cli.parse(allocator, args, &cli_diagnostics) catch |err| switch (err) {
            error.ParseError => {
                cli_diagnostics.renderToStdErr(args);
                std.os.exit(1);
            },
            else => |e| return e,
        };

        // print any warnings/notes
        cli_diagnostics.renderToStdErr(args);
        // If there was something printed, then add an extra newline separator
        // so that there is a clear separation between the cli diagnostics and whatever
        // gets printed after
        if (cli_diagnostics.errors.items.len > 0) {
            std.debug.print("\n", .{});
        }
        break :options options;
    };
    defer options.deinit();

    const stdout_writer = std.io.getStdOut().writer();
    if (options.verbose) {
        try options.dumpVerbose(stdout_writer);
        try stdout_writer.writeByte('\n');
    }

    var full_input = full_input: {
        if (options.preprocess) {
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
            for (options.extra_include_paths.items) |extra_include_path| {
                try argv.append("--include-directory");
                try argv.append(extra_include_path);
            }
            var symbol_it = options.symbols.iterator();
            while (symbol_it.next()) |entry| {
                switch (entry.value_ptr.*) {
                    .define => try argv.append("-D"),
                    .undefine => try argv.append("-U"),
                }
                try argv.append(entry.key_ptr.*);
            }
            try argv.append(options.input_filename);

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

            break :full_input result.stdout;
        } else {
            break :full_input try std.fs.cwd().readFileAlloc(allocator, options.input_filename, std.math.maxInt(usize));
        }
    };
    defer allocator.free(full_input);

    // Note: We still want to run this when no-preprocess is set because:
    //   1. We want to print accurate line numbers after removing multiline comments
    //   2. We want to be able to handle an already-preprocessed input with #line commands in it
    var mapping_results = try parseAndRemoveLineCommands(allocator, full_input, full_input, .{ .initial_filename = options.input_filename });
    defer mapping_results.mappings.deinit(allocator);

    // Set the root file
    if (mapping_results.mappings.files.getOffset(options.input_filename)) |root_filename_offset| {
        mapping_results.mappings.root_filename_offset = root_filename_offset;
    } else {
        // This *should* be impossible, as the clang preprocessor inserts whatever filename
        // you give it into the #line directives (e.g. `.\./rCDaTA.rC` for a file called
        // `rcdata.rc` will get a `#line 1 ".\\./rCDaTA.rC"` directive), but this may still
        // happen if there is a mismatch in how the line directive strings are parsed versus
        // how they are escaped/written by the preprocessor.
        std.debug.print("input filename not found in source mappings: {s}\n", .{options.input_filename});
        @panic("Internal error (this is a bug)");
    }

    var final_input = removeComments(mapping_results.result, mapping_results.result, &mapping_results.mappings);

    var output_file = try std.fs.cwd().createFile(options.output_filename, .{});
    defer output_file.close();

    var diagnostics = Diagnostics.init(allocator);
    defer diagnostics.deinit();

    // std.debug.print("after preprocessor:\n------------------\n{s}\n------------------\n", .{final_input});
    // std.debug.print("\nmappings:\n", .{});
    // for (mapping_results.mappings.mapping.items, 0..) |span, i| {
    //     const line_num = i + 1;
    //     const filename = mapping_results.mappings.files.get(span.filename_offset);
    //     std.debug.print("{}: {s}:{}-{}\n", .{ line_num, filename, span.start_line, span.end_line });
    // }
    // std.debug.print("\n", .{});

    compile(allocator, final_input, output_file.writer(), .{
        .cwd = std.fs.cwd(),
        .diagnostics = &diagnostics,
        .source_mappings = &mapping_results.mappings,
        .ignore_include_env_var = options.ignore_include_env_var,
        .extra_include_paths = options.extra_include_paths.items,
        .default_language_id = options.default_language_id,
        .default_code_page = options.default_code_page orelse .windows1252,
        .verbose = options.verbose,
        .null_terminate_string_table_strings = options.null_terminate_string_table_strings,
        .max_string_literal_codepoints = options.max_string_literal_codepoints,
        .silent_duplicate_control_ids = options.silent_duplicate_control_ids,
    }) catch |err| switch (err) {
        error.ParseError, error.CompileError => {
            diagnostics.renderToStdErr(std.fs.cwd(), final_input, mapping_results.mappings);
            std.os.exit(1);
        },
        else => |e| return e,
    };

    // print any warnings/notes
    diagnostics.renderToStdErr(std.fs.cwd(), final_input, mapping_results.mappings);
}
