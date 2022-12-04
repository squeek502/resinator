const std = @import("std");
const utils = @import("utils.zig");
const fuzzy_options = @import("fuzzy_options");
const iterations = fuzzy_options.max_iterations;

test {
    const allocator = std.testing.allocator;
    var random = std.rand.DefaultPrng.init(0);
    var rand = random.random();

    var source_buffer = std.ArrayList(u8).init(allocator);
    defer source_buffer.deinit();

    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var i: u64 = 0;
    while (iterations == 0 or i < iterations) : (i += 1) {
        source_buffer.shrinkRetainingCapacity(0);
        const literal = try utils.randomNumberLiteral(allocator, rand, true);
        defer allocator.free(literal);
        var source_writer = source_buffer.writer();
        try source_writer.print("1 RCDATA {{ {s} }}", .{literal});

        const source = source_buffer.items;

        try utils.expectSameResOutput(allocator, source, &buffer);
    }
}
