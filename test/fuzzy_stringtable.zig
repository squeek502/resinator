const std = @import("std");
const utils = @import("test_utils");
const fuzzy_options = @import("fuzzy_options");
const iterations = fuzzy_options.max_iterations;

test "fuzz" {
    const io = std.testing.io;
    const allocator = std.testing.allocator;
    var random = std.Random.DefaultPrng.init(0);
    const rand = random.random();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(tmp_path);

    var source_buffer: std.ArrayList(u8) = .empty;
    defer source_buffer.deinit(allocator);

    var i: u64 = 0;
    while (iterations == 0 or i < iterations) : (i += 1) {
        source_buffer.shrinkRetainingCapacity(0);
        const literal = try utils.randomStringLiteral(.ascii, allocator, rand, 256);
        defer allocator.free(literal);
        try source_buffer.print(allocator, "STRINGTABLE {{ 1, {s} }}\n", .{literal});
        try source_buffer.print(allocator, "#pragma code_page(65001)\n", .{});
        try source_buffer.print(allocator, "STRINGTABLE {{ 2, {s} }}\n", .{literal});

        const source = source_buffer.items;

        // write out the source file to disk for debugging
        if (fuzzy_options.fuzzy_debug)
            try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = ".zig-cache/tmp/fuzzy_stringtable_strings.rc", .data = source });

        try utils.expectSameResOutput(allocator, io, source, .{
            .cwd = tmp.dir,
            .cwd_path = tmp_path,
        });
    }
}
