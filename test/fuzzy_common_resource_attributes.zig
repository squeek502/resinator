const std = @import("std");
const utils = @import("utils.zig");
const resinator = @import("resinator");

const common_resource_attributes: []const []const u8 = &.{
    "PRELOAD",  "LOADONCALL",  "FIXED",
    "MOVEABLE", "DISCARDABLE", "PURE",
    "IMPURE",   "SHARED",      "NONSHARED",
};

// TODO: For each resource type as well
test "common resource attribute permutations" {
    // Use a single tmp dir t avoid creating and cleaning up a dir for each RC invocation
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

            const expected_res = resinator.compile.getExpectedFromWindowsRCWithDir(allocator, source, tmp.dir, "zig-cache/tmp/" ++ tmp.sub_path) catch {
                std.debug.print("\n^^^^^^^^^^^^\nFound input that is rejected by the Windows RC compiler:\n\n{s}\n\n--------------------------------\n\n", .{std.fmt.fmtSliceEscapeLower(source)});
                return error.UnexpectedRCError;
            };
            defer allocator.free(expected_res);

            var diagnostics = resinator.errors.Diagnostics.init(allocator);
            defer diagnostics.deinit();

            buffer.shrinkRetainingCapacity(0);
            resinator.compile.compile(allocator, source, buffer.writer(), std.fs.cwd(), &diagnostics) catch |err| switch (err) {
                error.ParseError, error.CompileError => {
                    diagnostics.renderToStdErr(std.fs.cwd(), source, null);
                    return err;
                },
                else => return err,
            };

            std.testing.expectEqualSlices(u8, expected_res, buffer.items) catch |err| {
                std.debug.print("\nSource:\n{s}\n\n--------------------------------\n\n", .{std.fmt.fmtSliceEscapeLower(source)});
                std.debug.print("expected:\n{}\nactual:\n{}\n\n", .{ std.zig.fmtEscapes(expected_res), std.zig.fmtEscapes(buffer.items) });
                return err;
            };

            source_buffer.shrinkRetainingCapacity(0);
        }
    }
}
