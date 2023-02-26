const std = @import("std");
const utils = @import("utils.zig");

const common_resource_attributes: []const []const u8 = &.{
    "PRELOAD",  "LOADONCALL",  "FIXED",
    "MOVEABLE", "DISCARDABLE", "PURE",
    "IMPURE",   "SHARED",      "NONSHARED",
};

// TODO: For each resource type as well
test "RCDATA common resource attribute permutations" {
    const allocator = std.testing.allocator;
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);

    var source_buffer = std.ArrayList(u8).init(allocator);
    defer source_buffer.deinit();

    var permutations_iterator = utils.AllKPermutationsIterator(common_resource_attributes.len).init();
    var perm_i: usize = 0;
    const num_permutations = utils.numAllKPermutations(common_resource_attributes.len);
    while (permutations_iterator.next()) |perm| {
        perm_i += 1;

        const is_batch_i = perm_i % 10000 == 0 or perm_i == num_permutations;

        const source_writer = source_buffer.writer();
        try source_writer.writeAll("1 RCDATA ");
        for (perm) |i| {
            const attribute = common_resource_attributes[i];
            try source_writer.writeAll(attribute);
            try source_writer.writeByte(' ');
        }
        try source_writer.writeAll("{}\n");

        // With 9 common resource attributes, the total number of K-permutations is 986,410, so by
        // batching large amounts of permutations together we hugely reduce the amount of time it takes this
        // test to run, since the bottleneck is the creation of each `.rc` and `.res` file.
        if (is_batch_i) {
            std.debug.print("{}\n", .{perm_i});
            const source = source_buffer.items;

            try utils.expectSameResOutput(allocator, source, &buffer, tmp.dir, tmp_path);

            source_buffer.shrinkRetainingCapacity(0);
        }
    }
}

test "ICON common resource attribute permutations" {
    // This takes a long time so it's disabled by default
    if (true) return error.SkipZigTest;

    const allocator = std.testing.allocator;
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);

    try tmp.dir.writeFile("test.ico", "\x00\x00\x01\x00\x01\x00\x01\x01\x00\x00\x01\x00 \x000\x00\x00\x00\x16\x00\x00\x00(\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x01\x00 \x00\x00\x00\x00\x00\x04\x00\x00\x00\x12\x0b\x00\x00\x12\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\xff\xff\x00\x00\x00\x00");

    var source_buffer = std.ArrayList(u8).init(allocator);
    defer source_buffer.deinit();

    var permutations_iterator = utils.AllKPermutationsIterator(common_resource_attributes.len).init();
    var perm_i: usize = 0;
    const num_permutations = utils.numAllKPermutations(common_resource_attributes.len);
    while (permutations_iterator.next()) |perm| {
        perm_i += 1;

        const is_batch_i = perm_i % 10000 == 0 or perm_i == num_permutations;

        // We only care about permutations with PRELOAD in them
        var has_preload: bool = false;
        for (perm) |i| {
            if (i == 0) {
                has_preload = true;
                break;
            }
        }
        if (!is_batch_i and !has_preload) continue;

        const source_writer = source_buffer.writer();
        try source_writer.writeAll("1 ICON ");
        for (perm) |i| {
            const attribute = common_resource_attributes[i];
            try source_writer.writeAll(attribute);
            try source_writer.writeByte(' ');
        }
        try source_writer.writeAll("test.ico\n");

        // With 9 common resource attributes, the total number of K-permutations is 986,410, so by
        // batching large amounts of permutations together we hugely reduce the amount of time it takes this
        // test to run, since the bottleneck is the creation of each `.rc` and `.res` file.
        if (is_batch_i) {
            std.debug.print("{}\n", .{perm_i});
            const source = source_buffer.items;

            try utils.expectSameResOutput(allocator, source, &buffer, tmp.dir, tmp_path);

            source_buffer.shrinkRetainingCapacity(0);
        }
    }
}
