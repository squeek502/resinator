const std = @import("std");
const utils = @import("utils.zig");

const common_resource_attributes: []const []const u8 = &.{
    "PRELOAD",  "LOADONCALL",  "FIXED",
    "MOVEABLE", "DISCARDABLE", "PURE",
    "IMPURE",   "SHARED",      "NONSHARED",
};

// TODO: For each resource type as well
test "common resource attribute permutations" {
    // Use a single tmp dir to avoid creating and cleaning up a dir for each RC invocation
    // Unfortunately there doesn't seem to be a way to avoid hitting the filesystem,
    // the Windows RC compiler doesn't seem to like named pipes for either input or output
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const allocator = std.testing.allocator;
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var source_buffer = std.ArrayList(u8).init(allocator);
    defer source_buffer.deinit();

    var permutations_iterator = utils.AllKPermutationsIterator(common_resource_attributes.len).init();
    var perm_i: usize = 0;
    const num_permutations = utils.numAllKPermutations(common_resource_attributes.len);
    while (permutations_iterator.next()) |perm| {
        perm_i += 1;

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
        if (perm_i % 10000 == 0 or perm_i == num_permutations) {
            std.debug.print("{}\n", .{perm_i});
            const source = source_buffer.items;

            try utils.expectSameResOutput(allocator, source, &buffer);

            source_buffer.shrinkRetainingCapacity(0);
        }
    }
}
