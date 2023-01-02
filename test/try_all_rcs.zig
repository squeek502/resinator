//! This is primarily intended to be run on a checked out copy of
//! https://github.com/microsoft/Windows-classic-samples
//!
//! It's a little janky and needs to be recompiled for use with
//! windres/llvm-rc (see the `for_windres`/`for_llvm_rc` constants below)

const std = @import("std");
const Allocator = std.mem.Allocator;

const for_windres = false;
const for_llvm_rc = false;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .stack_trace_frames = 8 }){};
    defer if (gpa.deinit()) @panic("found leaks");
    const allocator = gpa.allocator();

    var args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const command = if (args.len > 1) args[1] else "resinator.exe";

    var iterable_cwd = try std.fs.cwd().openIterableDir(".", .{});
    defer iterable_cwd.close();
    var walker = try iterable_cwd.walk(allocator);
    defer walker.deinit();
    var num_successes: usize = 0;
    var num_errors: usize = 0;
    var num_discrepancies_unexpected_err: usize = 0;
    var num_discrepancies_missing_err: usize = 0;
    var num_discrepancies_diff_res: usize = 0;
    while (try walker.next()) |entry| {
        if (entry.kind != .File) continue;
        const ext = std.fs.path.extension(entry.basename);
        if (!std.ascii.eqlIgnoreCase(ext, ".rc")) continue;

        // File that compiles differently due to preprocessor differences
        if (std.mem.eql(u8, "NonDefaultDropMenuVerb.rc", entry.basename)) continue;

        if (for_windres) {
            if (std.mem.eql(u8, "AmbientLightAware.rc", entry.basename)) continue;
        }

        const dir_path = std.fs.path.dirname(entry.path) orelse "";

        std.debug.print("{s}\n", .{entry.path});

        const did_compile = expectSameResOutput(
            allocator,
            command,
            entry.basename,
            entry.dir,
            dir_path,
        ) catch |err| {
            switch (err) {
                error.DidNotExpectErrorButGotOne => num_discrepancies_unexpected_err += 1,
                error.ExpectedErrorButDidntGetOne => num_discrepancies_missing_err += 1,
                error.TestExpectedEqual => num_discrepancies_diff_res += 1,
                else => |e| return e,
            }
            continue;
        };
        if (did_compile) num_successes += 1 else num_errors += 1;
    }

    const total_discrepancies = num_discrepancies_unexpected_err + num_discrepancies_missing_err + num_discrepancies_diff_res;
    if (total_discrepancies > 0) {
        std.debug.print("\nFound {} discrepancies out of {} .rc files\n", .{ total_discrepancies, total_discrepancies + num_successes + num_errors });
        std.debug.print(".rc files with different .res outputs: {}\n.rc files with unexpected compile errors: {}\n.rc files with missing compile errors: {}\n", .{
            num_discrepancies_diff_res,
            num_discrepancies_unexpected_err,
            num_discrepancies_missing_err,
        });
    }
    std.debug.print("\nSuccessfully processed {} .rc files with no discrepancies\n", .{num_successes + num_errors});
    std.debug.print(".rc files correctly compiled: {},\n.rc files with expected compile errors: {}\n", .{ num_successes, num_errors });
}

pub fn expectSameResOutput(allocator: Allocator, command: []const u8, filename: []const u8, cwd: std.fs.Dir, cwd_path: []const u8) !bool {
    const expected_res: ?[]const u8 = getExpectedFromWindowsRCWithDir(allocator, filename, cwd, cwd_path) catch |err| switch (err) {
        error.ExitCodeFailure, error.ProcessTerminated => null,
        else => |e| return e,
    };
    defer if (expected_res != null) allocator.free(expected_res.?);

    const actual_res: ?[]const u8 = getActualFromResinatorWithDir(allocator, command, filename, cwd, cwd_path) catch |err| switch (err) {
        error.ExitCodeFailure => null,
        else => |e| return e,
    };
    defer if (actual_res != null) allocator.free(actual_res.?);

    if (actual_res == null and expected_res == null) return false;

    if (actual_res == null and expected_res != null) {
        return error.DidNotExpectErrorButGotOne;
    }

    if (expected_res == null and actual_res != null) {
        return error.ExpectedErrorButDidntGetOne;
    }

    std.testing.expectEqualSlices(u8, expected_res.?, actual_res.?) catch |err| {
        return err;
    };

    return true;
}

pub fn getActualFromResinatorWithDir(allocator: Allocator, command: []const u8, filename: []const u8, cwd: std.fs.Dir, cwd_path: []const u8) ![]const u8 {
    const out_filename = try std.mem.concat(allocator, u8, &.{ filename, ".actual.res" });
    defer allocator.free(out_filename);

    var result = try std.ChildProcess.exec(.{
        .allocator = allocator,
        .argv = if (for_llvm_rc) &[_][]const u8{
            command,
            "/fo",
            out_filename,
            filename,
        } else &[_][]const u8{
            command,
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
                if (code == 1) {
                    return error.ExitCodeFailure;
                } else {
                    return error.ProbablePanic;
                }
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
