const std = @import("std");
const removeComments = @import("comments.zig").removeComments;
const parseAndRemoveLineCommands = @import("source_mapping.zig").parseAndRemoveLineCommands;
const compile = @import("compile.zig").compile;
const Diagnostics = @import("errors.zig").Diagnostics;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == false);
    const allocator = gpa.allocator();

    var args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 1) {
        std.debug.print("Missing input filename\n", .{});
        std.os.exit(1);
    }
    if (args.len < 2) {
        std.debug.print("Missing output filename\n", .{});
        std.os.exit(1);
    }

    const input_filename = args[1];

    var result = try std.ChildProcess.exec(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "clang",
            "-E", // preprocessor only
            "--comments",
            "-fuse-line-directives", // #line <num> instead of # <num>
            "-xc", // output c
            // TODO: could use --trace-includes to give info about what's included from where
            //"-Werror=invalid-pp-token", // will error on unfinished string literals
            // TODO: could use -Werror instead
            input_filename,
        },
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

    const output_filename = args[2];
    var output_file = try std.fs.cwd().createFile(output_filename, .{});
    defer output_file.close();

    var diagnostics = Diagnostics.init(allocator);
    defer diagnostics.deinit();

    std.debug.print("after preprocessor:\n------------------\n{s}\n------------------\n\nmappings:\n", .{preprocessed_input});
    for (mapping_results.mappings.mapping.items) |span, i| {
        const line_num = i + 1;
        const filename = mapping_results.mappings.files.get(span.filename_offset);
        std.debug.print("{}: {s}:{}-{}\n", .{ line_num, filename, span.start_line, span.end_line });
    }
    std.debug.print("\n", .{});

    compile(allocator, preprocessed_input, output_file.writer(), std.fs.cwd(), &diagnostics) catch |err| switch (err) {
        error.ParseError, error.CompileError => {
            diagnostics.renderToStdErr(std.fs.cwd(), preprocessed_input, mapping_results.mappings);
            std.os.exit(1);
        },
        else => |e| return e,
    };
}
