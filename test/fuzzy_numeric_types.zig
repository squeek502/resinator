const std = @import("std");
const utils = @import("test_utils");

test "raw data" {
    const io = std.testing.io;
    const allocator = std.testing.allocator;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(tmp_path);

    var source_buffer: std.ArrayList(u8) = .empty;
    defer source_buffer.deinit(allocator);

    var num: u8 = 0;
    while (num < 100) : (num += 1) {
        // RT_STRING as a number is a special case since it will always lead to invalid .res files
        if (num == 6) return;

        source_buffer.shrinkRetainingCapacity(0);

        try source_buffer.print(allocator, "1 {d} {{ \"hello\" }}", .{num});

        const source = source_buffer.items;

        try utils.expectSameResOutput(allocator, io, source, .{
            .cwd = tmp.dir,
            .cwd_path = tmp_path,
        });
    }
}
