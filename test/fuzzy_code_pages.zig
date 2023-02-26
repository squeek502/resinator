const std = @import("std");
const utils = @import("utils.zig");
const fuzzy_options = @import("fuzzy_options");
const iterations = fuzzy_options.max_iterations;

test "windows-1252 mappings" {
    // This takes a long time, so skip it by default; comment out the following line
    // to run the test.
    if (true) return error.SkipZigTest;

    const allocator = std.testing.allocator;
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);

    var source_buffer = std.ArrayList(u8).init(allocator);
    defer source_buffer.deinit();
    try source_buffer.appendSlice("#pragma code_page(65001)\n");

    const max_codepoint = 0x10FFFF;
    var codepoint: u21 = 128;
    var i: usize = 0;
    while (codepoint <= max_codepoint) : (codepoint += 1) {
        if (!std.unicode.utf8ValidCodepoint(codepoint)) continue;
        switch (codepoint) {
            // disallowed private use character
            '\u{E000}' => continue,
            // disallowed BOM
            '\u{FEFF}' => continue,
            else => {},
        }
        i += 1;

        const source_writer = source_buffer.writer();
        try source_writer.print("0x{X} RCDATA {{\"", .{codepoint});
        const codepoint_sequence_length = std.unicode.utf8CodepointSequenceLength(codepoint) catch unreachable;
        const start_index = source_buffer.items.len;
        try source_buffer.resize(source_buffer.items.len + codepoint_sequence_length);
        _ = std.unicode.utf8Encode(codepoint, source_buffer.items[start_index..]) catch unreachable;
        try source_writer.writeAll("\"}\n");

        // 0x10FFFF is a lot of codepoints, so breaking them into large batches helps
        // reduce the amount of time this test takes.
        if (i % 10000 == 0 or i == max_codepoint) {
            std.debug.print("{}\n", .{i});
            const source = source_buffer.items;

            try utils.expectSameResOutput(allocator, source, &buffer, tmp.dir, tmp_path);

            source_buffer.shrinkRetainingCapacity(0);
            try source_buffer.appendSlice("#pragma code_page(65001)\n");
        }
    }
}

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
        const bytes = try utils.randomAlphanumExtendedBytes(allocator, rand);
        defer allocator.free(bytes);
        var source_writer = source_buffer.writer();
        // There seems to be a very strange bug where the order of the pragma's
        // matter, if 1252 is first then the ascii string literal is parsed strangely
        // when the code page is 65001.
        // TODO: What is happening?
        try source_writer.print(
            \\#pragma code_page(65001)
            \\{s}
            \\{s}
            \\{{
            \\"{s}"
            \\L"{s}"
            \\}}
            \\
            \\#pragma code_page(1252)
            \\{s}
            \\{s}
            \\{{
            \\"{s}"
            \\L"{s}"
            \\}}
        , .{
            bytes,
            bytes,
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

        try utils.expectSameResOutput(allocator, source, &buffer, tmp.dir, tmp_path);
    }
}
