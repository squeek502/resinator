const std = @import("std");
const removeComments = @import("comments.zig").removeComments;
const parseAndRemoveLineCommands = @import("source_mapping.zig").parseAndRemoveLineCommands;
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

    var arg_i: usize = 1; // start at 1 to skip past the exe name
    while (arg_i < args.len) {
        // TODO: Is this actually case-insensitive?
        if (std.ascii.eqlIgnoreCase("/I", args[arg_i])) {
            if (arg_i + 1 >= args.len) {
                std.debug.print("Missing include path after {s} option\n", .{args[arg_i]});
                std.os.exit(1);
            }
            try extra_include_paths.append(args[arg_i + 1]);
            arg_i += 2;
        } else if (std.ascii.eqlIgnoreCase("/fo", args[arg_i])) {
            if (arg_i + 1 >= args.len) {
                std.debug.print("Missing output path after {s} option\n", .{args[arg_i]});
                std.os.exit(1);
            }
            output_filename = args[arg_i + 1];
            arg_i += 2;
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
    }

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
    defer allocator.free(result.stdout);
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

    var mapping_results = try parseAndRemoveLineCommands(allocator, result.stdout, result.stdout);
    defer mapping_results.mappings.deinit(allocator);

    var preprocessed_input = removeComments(mapping_results.result, mapping_results.result, &mapping_results.mappings);

    var output_file = try std.fs.cwd().createFile(output_filename.?, .{});
    defer output_file.close();

    var diagnostics = Diagnostics.init(allocator);
    defer diagnostics.deinit();

    // std.debug.print("after preprocessor:\n------------------\n{s}\n------------------\n\nmappings:\n", .{preprocessed_input});
    // for (mapping_results.mappings.mapping.items) |span, i| {
    //     const line_num = i + 1;
    //     const filename = mapping_results.mappings.files.get(span.filename_offset);
    //     std.debug.print("{}: {s}:{}-{}\n", .{ line_num, filename, span.start_line, span.end_line });
    // }
    // std.debug.print("\n", .{});

    compile(allocator, preprocessed_input, output_file.writer(), .{
        .cwd = std.fs.cwd(),
        .diagnostics = &diagnostics,
        .source_mappings = &mapping_results.mappings,
    }) catch |err| switch (err) {
        error.ParseError, error.CompileError => {
            diagnostics.renderToStdErr(std.fs.cwd(), preprocessed_input, mapping_results.mappings);
            std.os.exit(1);
        },
        else => |e| return e,
    };

    // print any warnings/notes
    diagnostics.renderToStdErr(std.fs.cwd(), preprocessed_input, mapping_results.mappings);
}
