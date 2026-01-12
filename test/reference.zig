const std = @import("std");
const utils = @import("utils.zig");

const reference_rc = @embedFile("data/reference.rc");
const expected_res = @embedFile("data/reference.res");

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    const arena = init.arena.allocator();

    const args = try init.minimal.args.toSlice(arena);
    const working_dir_path = args[1];
    var working_dir = try std.Io.Dir.cwd().openDir(io, working_dir_path, .{});
    defer working_dir.close(io);

    var result = try utils.getResinatorResult(arena, io, reference_rc, .{
        .cwd_path = args[1],
        .cwd = working_dir,
    });
    defer result.deinit(arena);

    if (result.res == null) {
        try result.diagnostics.renderToStderr(io, working_dir, result.processed_rc.?, null);
        return error.NoResOutput;
    }

    try std.testing.expectEqualSlices(u8, expected_res, result.res.?);
}
