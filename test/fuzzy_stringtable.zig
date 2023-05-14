const std = @import("std");
const utils = @import("utils.zig");
const fuzzy_options = @import("fuzzy_options");
const iterations = fuzzy_options.max_iterations;

test "fuzz" {
    const allocator = std.testing.allocator;
    var random = std.rand.DefaultPrng.init(0);
    var rand = random.random();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);

    var source_buffer = std.ArrayList(u8).init(allocator);
    defer source_buffer.deinit();

    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var i: u64 = 0;
    while (iterations == 0 or i < iterations) : (i += 1) {
        source_buffer.shrinkRetainingCapacity(0);
        const literal = try utils.randomAsciiStringLiteral(allocator, rand);
        defer allocator.free(literal);
        var source_writer = source_buffer.writer();
        try source_writer.print("STRINGTABLE {{ 1, {s} }}\n", .{literal});
        try source_writer.print("#pragma code_page(65001)\n", .{});
        try source_writer.print("STRINGTABLE {{ 2, {s} }}\n", .{literal});

        const source = source_buffer.items;

        // write out the source file to disk for debugging
        try std.fs.cwd().writeFile("zig-cache/tmp/fuzzy_stringtable_strings.rc", source);

        try utils.expectSameResOutput(allocator, source, &buffer, tmp.dir, tmp_path);
    }
}
