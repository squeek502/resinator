const std = @import("std");
const utils = @import("utils.zig");
const fuzzy_options = @import("fuzzy_options");
const iterations = fuzzy_options.max_iterations;

test {
    const allocator = std.testing.allocator;
    var random = std.Random.DefaultPrng.init(0);
    const rand = random.random();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);

    var source_buffer = std.ArrayList(u8).init(allocator);
    defer source_buffer.deinit();

    var i: u64 = 0;
    while (iterations == 0 or i < iterations) : (i += 1) {
        source_buffer.shrinkRetainingCapacity(0);

        const lhs = try utils.randomNumberLiteral(allocator, rand, true);
        defer allocator.free(lhs);
        const operator = utils.randomOperator(rand);
        const rhs = try utils.randomNumberLiteral(allocator, rand, true);
        defer allocator.free(rhs);

        var source_writer = source_buffer.writer();
        try source_writer.print("1 RCDATA {{ {s} {c} {s} }}", .{ lhs, operator, rhs });

        const source = source_buffer.items;

        try utils.expectSameResOutput(allocator, source, .{
            .cwd = tmp.dir,
            .cwd_path = tmp_path,
        });
    }
}
