const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .stack_trace_frames = 8 }){};
    defer if (gpa.deinit()) @panic("found leaks");
    const allocator = gpa.allocator();

    var iterable_cwd = try std.fs.cwd().openIterableDir(".", .{});
    defer iterable_cwd.close();
    var walker = try iterable_cwd.walk(allocator);
    defer walker.deinit();
    while (try walker.next()) |entry| {
        if (entry.kind != .File) continue;
        const ext = std.fs.path.extension(entry.basename);
        if (!std.ascii.eqlIgnoreCase(ext, ".rc")) continue;

        const dir_path = std.fs.path.dirname(entry.path).?;

        std.debug.print("{s}\n", .{entry.path});

        try expectSameResOutput(
            allocator,
            entry.basename,
            entry.dir,
            dir_path,
        );
    }
}

pub fn expectSameResOutput(allocator: Allocator, filename: []const u8, cwd: std.fs.Dir, cwd_path: []const u8) !void {
    const expected_res: ?[]const u8 = getExpectedFromWindowsRCWithDir(allocator, filename, cwd, cwd_path) catch |err| switch (err) {
        error.ExitCodeFailure, error.ProcessTerminated => null,
        else => |e| return e,
    };
    defer if (expected_res != null) allocator.free(expected_res.?);

    const actual_res: ?[]const u8 = getActualFromResinatorWithDir(allocator, filename, cwd, cwd_path) catch |err| switch (err) {
        error.ExitCodeFailure, error.ProcessTerminated => null,
        else => |e| return e,
    };
    defer if (actual_res != null) allocator.free(actual_res.?);

    if (actual_res == null and expected_res == null) return;

    if (actual_res == null and expected_res != null) {
        return error.DidNotExpectErrorButGotOne;
    }

    if (expected_res == null and actual_res != null) {
        return error.ExpectedErrorButDidntGetOne;
    }

    std.testing.expectEqualSlices(u8, expected_res.?, actual_res.?) catch |err| {
        return err;
    };
}

pub fn getActualFromResinatorWithDir(allocator: Allocator, filename: []const u8, cwd: std.fs.Dir, cwd_path: []const u8) ![]const u8 {
    const out_filename = try std.mem.concat(allocator, u8, &.{ filename, ".resinator.res" });
    defer allocator.free(out_filename);

    var result = try std.ChildProcess.exec(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            // Note: This relies on `resinator.exe` being in the PATH
            "resinator.exe",
            filename,
            out_filename,
        },
        .cwd = cwd_path,
        .max_output_bytes = std.math.maxInt(u32),
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
            return error.ProcessTerminated;
        },
    }

    return cwd.readFileAlloc(allocator, out_filename, std.math.maxInt(usize));
}

pub fn getExpectedFromWindowsRCWithDir(allocator: Allocator, filename: []const u8, cwd: std.fs.Dir, cwd_path: []const u8) ![]const u8 {
    const out_filename = try std.mem.concat(allocator, u8, &.{ filename, ".expected.res" });
    defer allocator.free(out_filename);

    var result = try std.ChildProcess.exec(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            // Note: This relies on `rc.exe` being in the PATH
            "rc.exe",
            "/fo",
            out_filename,
            filename,
        },
        .cwd = cwd_path,
        .max_output_bytes = std.math.maxInt(u32),
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
            return error.ProcessTerminated;
        },
    }

    return cwd.readFileAlloc(allocator, out_filename, std.math.maxInt(usize));
}
