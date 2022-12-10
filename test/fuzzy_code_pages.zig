const std = @import("std");
const utils = @import("utils.zig");
const fuzzy_options = @import("fuzzy_options");
const iterations = fuzzy_options.max_iterations;

test "fuzz" {
    // Use a single tmp dir to avoid creating and cleaning up a dir for each RC invocation
    // Unfortunately there doesn't seem to be a way to avoid hitting the filesystem,
    // the Windows RC compiler doesn't seem to like named pipes for either input or output
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

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
        const bytes = try utils.randomAlphanumExtendedBytes(allocator, rand);
        defer allocator.free(bytes);
        var source_writer = source_buffer.writer();
        try source_writer.print(
            \\#pragma code_page(1252)
            \\{s}
            \\#pragma code_page(65001)
            \\{s}
            \\{{
            \\#pragma code_page(1252)
            \\"{s}"
            \\L"{s}"
            \\#pragma code_page(65001)
            \\"{s}"
            \\L"{s}"
            \\}}
            \\
        , .{
            bytes,
            bytes,
            bytes,
            bytes,
            bytes,
            bytes,
        });

        const source = source_buffer.items;

        // write out the source file to disk for debugging
        try std.fs.cwd().writeFile("zig-cache/tmp/fuzzy_code_pages.rc", source);

        try utils.expectSameResOutput(allocator, source, &buffer);
    }
}
