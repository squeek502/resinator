const std = @import("std");
const resinator = @import("resinator");
const utils = @import("utils.zig");

const reference_rc = @embedFile("data/reference.rc");
const expected_res = @embedFile("data/reference.res");

fn setupTmpDir() !std.testing.TmpDir {
    var tmp = std.testing.tmpDir(.{});
    errdefer tmp.cleanup();

    // Note: `dirname(@src().file) + "/files"` in theory would work here but it seems to be
    //       relative to the cwd at invoke-time whereas std.fs.cwd() at runtime has the cwd
    //       of the build.zig (or test runner?), so they don't always match up.
    //
    //       For now, we assume that `fs.cwd()` will always be the directory of the build.zig
    //
    // TODO: Figure out a better way to consistently find the ./test/files directory.
    const files_dir_path = "test/data/files";

    var files_dir = try std.fs.cwd().openIterableDir(files_dir_path, .{});
    defer files_dir.close();

    var files_it = files_dir.iterate();
    while (try files_it.next()) |entry| {
        try files_dir.dir.copyFile(entry.name, tmp.dir, entry.name, .{});
    }

    return tmp;
}

test "reference.rc" {
    var tmp = try setupTmpDir();
    //defer tmp.cleanup();

    const tmp_path = try tmp.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(tmp_path);

    var result = try utils.getResinatorResult(std.testing.allocator, reference_rc, .{
        .cwd = tmp.dir,
        .cwd_path = tmp_path,
        .run_preprocessor = true,
    });
    defer result.deinit(std.testing.allocator);

    if (result.res == null) {
        result.diagnostics.renderToStdErrDetectTTY(tmp.dir, result.processed_rc.?, null);
        return error.NoResOutput;
    }

    try std.testing.expectEqualSlices(u8, expected_res, result.res.?);
}
