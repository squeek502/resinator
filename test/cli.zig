const std = @import("std");
const build_options = @import("build_options");
const exe_path = build_options.cli_exe_path;

test "including windows.h" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile("test.rc",
        \\#include "windows.h"
    );

    try testExpectNoError(tmp.dir, &.{ exe_path, "test.rc" });
    try testExpectNoError(tmp.dir, &.{ exe_path, "/:auto-includes", "gnu", "test.rc" });
}

fn testExpectNoError(dir: std.fs.Dir, argv: []const []const u8) !void {
    const allocator = std.testing.allocator;

    const dir_path = try dir.realpathAlloc(allocator, ".");
    defer allocator.free(dir_path);

    const result = try std.ChildProcess.run(.{
        .allocator = allocator,
        .argv = argv,
        .max_output_bytes = std.math.maxInt(u32),
        .cwd = dir_path,
        .cwd_dir = dir,
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    if (result.term != .Exited or result.term.Exited != 0) {
        std.debug.print("command failed with unexpected errors:\n---\nstdout:\n---\n{s}\n---\nstderr:\n---\n{s}\n", .{ result.stdout, result.stderr });
        return error.UnexpectedErrors;
    }
}
