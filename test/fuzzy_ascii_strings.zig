const std = @import("std");
const utils = @import("utils.zig");
const fuzzy_options = @import("fuzzy_options");
const iterations = fuzzy_options.max_iterations;

test "single chars" {
    const allocator = std.testing.allocator;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);

    var source_buf = "1 RCDATA { \"?\" }".*;
    var source: []u8 = &source_buf;
    const byte_index = std.mem.indexOfScalar(u8, source, '?').?;
    var byte: u8 = 1;
    while (true) : (byte += 1) {
        // \" is invalid within string literals (but accepted by the Windows RC compiler), skip it
        if (byte == '\\') continue;

        source[byte_index] = byte;

        try utils.expectSameResOutput(allocator, source, .{
            .cwd = tmp.dir,
            .cwd_path = tmp_path,
            .run_preprocessor = false,
        });

        if (byte == 255) break;
    }
}

test "single char escapes" {
    const allocator = std.testing.allocator;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);

    var source_buf = "1 RCDATA { \"\\?\" }".*;
    var source: []u8 = &source_buf;
    const escaped_byte_index = std.mem.indexOfScalar(u8, source, '?').?;
    var escaped_byte: u8 = 0;
    while (true) : (escaped_byte += 1) {
        // 'Substitute' leads to a false positive here, so skip it
        if (escaped_byte == '\x1A') continue;
        // \" is invalid within string literals (but accepted by the Windows RC compiler), skip it
        if (escaped_byte == '"') continue;

        source[escaped_byte_index] = escaped_byte;

        try utils.expectSameResOutput(allocator, source, .{
            .cwd = tmp.dir,
            .cwd_path = tmp_path,
            .run_preprocessor = false,
        });

        if (escaped_byte == 255) break;
    }
}

test "fuzz" {
    const allocator = std.testing.allocator;
    var random = std.rand.DefaultPrng.init(0);
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
        const literal = try utils.randomAsciiStringLiteral(allocator, rand);
        defer allocator.free(literal);
        var source_writer = source_buffer.writer();
        try source_writer.print("1 RCDATA {{ {s} }}", .{literal});

        const source = source_buffer.items;

        // write out the source file to disk for debugging
        try std.fs.cwd().writeFile("zig-cache/tmp/fuzzy_ascii_strings.rc", source);

        try utils.expectSameResOutput(allocator, source, .{
            .cwd = tmp.dir,
            .cwd_path = tmp_path,
            .run_preprocessor = false,
        });
    }
}
