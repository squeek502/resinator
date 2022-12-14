const std = @import("std");
const utils = @import("utils.zig");

test "raw data" {
    const allocator = std.testing.allocator;
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var source_buffer = std.ArrayList(u8).init(allocator);
    defer source_buffer.deinit();

    var num: u8 = 0;
    while (num < 100) : (num += 1) {
        // RT_STRING as a number is a special case since it will always lead to invalid .res files
        if (num == 6) return;

        source_buffer.shrinkRetainingCapacity(0);

        const source_writer = source_buffer.writer();
        try source_writer.print("1 {d} {{ \"hello\" }}", .{num});

        const source = source_buffer.items;

        try utils.expectSameResOutput(allocator, source, &buffer);
    }
}
