const std = @import("std");
const build_options = @import("build_options");
const exe_path = build_options.cli_exe_path;

test "including windows.h" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(.{
        .sub_path = "test.rc",
        .data =
        \\#include "windows.h"
        ,
    });

    try testExpectNoError(tmp.dir, &.{ exe_path, "test.rc" });
    try testExpectNoError(tmp.dir, &.{ exe_path, "/:auto-includes", "gnu", "test.rc" });
}

test "predefined macros" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(.{
        .sub_path = "test.rc",
        .data =
        \\1 RCDATA { RC_INVOKED, _WIN32 }
        ,
    });

    try testExpectNoError(tmp.dir, &.{ exe_path, "test.rc" });
    // case sensitive, so this doesn't undef RC_INVOKED
    try testExpectNoError(tmp.dir, &.{ exe_path, "/u", "rc_invoked", "test.rc" });
    // undefing predefined macros works
    try testExpectError(tmp.dir, &.{ exe_path, "/u", "RC_INVOKED", "test.rc" });
    try testExpectError(tmp.dir, &.{ exe_path, "/u", "_WIN32", "test.rc" });
}

fn testExpectNoError(dir: std.fs.Dir, argv: []const []const u8) !void {
    const allocator = std.testing.allocator;

    const dir_path = try dir.realpathAlloc(allocator, ".");
    defer allocator.free(dir_path);

    const result = try std.process.Child.run(.{
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

fn testExpectError(dir: std.fs.Dir, argv: []const []const u8) !void {
    const allocator = std.testing.allocator;

    const dir_path = try dir.realpathAlloc(allocator, ".");
    defer allocator.free(dir_path);

    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = argv,
        .max_output_bytes = std.math.maxInt(u32),
        .cwd = dir_path,
        .cwd_dir = dir,
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    if (result.term != .Exited or result.term.Exited == 0) {
        std.debug.print("command unexpectedly succeeded:\n---\nstdout:\n---\n{s}\n---\nstderr:\n---\n{s}\n", .{ result.stdout, result.stderr });
        return error.UnexpectedSuccess;
    }
}
