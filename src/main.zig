const std = @import("std");
const removeComments = @import("comments.zig").removeComments;
const compile = @import("compile.zig").compile;

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
            "--no-line-commands",
            "-xc", // output c
            input_filename,
        },
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    switch (result.term) {
        .Exited => |code| {
            if (code != 0) {
                std.debug.print("exit code: {}\n", .{result.term});
                std.debug.print("stdout: {s}\n", .{result.stdout});
                std.debug.print("stderr: {s}\n", .{result.stderr});
                return error.ExitCodeFailure;
            }
        },
        .Signal, .Stopped, .Unknown => {
            return error.ClangProcessTerminated;
        },
    }

    var preprocessed_input = removeComments(result.stdout, result.stdout);

    const output_filename = args[2];
    var output_file = try std.fs.cwd().createFile(output_filename, .{});
    defer output_file.close();

    try compile(allocator, preprocessed_input, output_file.writer(), std.fs.cwd());
}
