const std = @import("std");
const removeComments = @import("comments.zig").removeComments;
const parseAndRemoveLineCommands = @import("source_mapping.zig").parseAndRemoveLineCommands;
const compile = @import("compile.zig").compile;
const Diagnostics = @import("errors.zig").Diagnostics;
const cli = @import("cli.zig");
const parse = @import("parse.zig");
const lex = @import("lex.zig");

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
        try options.maybeAppendRC(std.fs.cwd());

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

    if (options.print_help_and_exit) {
        std.debug.print("{s}", .{cli.usage_string});
        return;
    }

    const stdout_writer = std.io.getStdOut().writer();
    if (options.verbose) {
        try options.dumpVerbose(stdout_writer);
        try stdout_writer.writeByte('\n');
    }

    var full_input = full_input: {
        if (options.preprocess != .no) {
            var argv = std.ArrayList([]const u8).init(allocator);
            defer argv.deinit();
            var temp_strings = std.ArrayList([]const u8).init(allocator);
            defer {
                for (temp_strings.items) |temp_string| {
                    allocator.free(temp_string);
                }
                temp_strings.deinit();
            }

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
                    .define => |value| {
                        try argv.append("-D");
                        const define_arg = arg: {
                            const arg = try std.fmt.allocPrint(allocator, "{s}={s}", .{ entry.key_ptr.*, value });
                            errdefer allocator.free(arg);
                            try temp_strings.append(arg);
                            break :arg arg;
                        };
                        try argv.append(define_arg);
                    },
                    .undefine => {
                        try argv.append("-U");
                        try argv.append(entry.key_ptr.*);
                    },
                }
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

    var final_input = removeComments(mapping_results.result, mapping_results.result, &mapping_results.mappings);

    var output_file = try std.fs.cwd().createFile(options.output_filename, .{});
    defer output_file.close();

    var diagnostics = Diagnostics.init(allocator);
    defer diagnostics.deinit();

    if (options.debug) {
        std.debug.print("after preprocessor:\n------------------\n{s}\n------------------\n", .{final_input});
        std.debug.print("\nmappings:\n", .{});
        for (mapping_results.mappings.mapping.items, 0..) |span, i| {
            const line_num = i + 1;
            const filename = mapping_results.mappings.files.get(span.filename_offset);
            std.debug.print("{}: {s}:{}-{}\n", .{ line_num, filename, span.start_line, span.end_line });
        }
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

            try tree.dump(std.io.getStdErr().writer());
            std.debug.print("\n", .{});
        }
    }

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
        .warn_instead_of_error_on_invalid_code_page = options.warn_instead_of_error_on_invalid_code_page,
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
