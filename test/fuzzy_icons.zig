const std = @import("std");
const utils = @import("test_utils");
const fuzzy_options = @import("fuzzy_options");
const iterations = fuzzy_options.max_iterations;

test "ICON fuzz" {
    const allocator = std.testing.allocator;
    var random = std.Random.DefaultPrng.init(0);
    var rand = random.random();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);

    var icon_buffer: std.Io.Writer.Allocating = .init(allocator);
    defer icon_buffer.deinit();

    const source = "1 ICON test.ico";

    var i: u64 = 0;
    while (iterations == 0 or i < iterations) : (i += 1) {
        icon_buffer.shrinkRetainingCapacity(0);
        const icon_writer = &icon_buffer.writer;
        // reserved bytes, very occasionally make it random
        try icon_writer.writeInt(u16, if (rand.int(u8) == 0) rand.int(u16) else 0, .little);
        // icon is 1, cursor is 2, anything else is invalid
        var res_type = if (rand.int(u8) == 0) rand.int(u16) else 1;
        // we don't want to test with a cursor value here, though, since
        // that is a compile error for us but not for the Win32 RC compiler
        if (res_type == 2) res_type = 0;
        try icon_writer.writeInt(u16, res_type, .little);
        // Unfortunately, we can't really make this random (or even > 1) because
        // the Win32 RC compiler has a bug that can lead to infinite `.res` filesize
        // (see also below comment about reported data size)
        try icon_writer.writeInt(u16, if (rand.int(u8) == 0) @as(u16, 0) else 1, .little);

        // we'll write one well-formed-ish resdir
        try icon_writer.writeInt(u8, rand.int(u8), .little); // width
        try icon_writer.writeInt(u8, rand.int(u8), .little); // height
        try icon_writer.writeInt(u8, rand.int(u8), .little); // num_colors
        // reserved, should be zero but occasionally make it random
        try icon_writer.writeInt(u8, if (rand.int(u8) == 0) rand.int(u8) else 0, .little);
        try icon_writer.writeInt(u16, rand.int(u16), .little); // color_planes/hotspot_x
        try icon_writer.writeInt(u16, rand.int(u16), .little); // bits_per_pixel/hotspot_y

        const make_png = rand.boolean();
        const random_bytes_len = rand.uintLessThanBiased(u32, 1000);
        var real_data_size = random_bytes_len;
        if (make_png) real_data_size += 16;

        // Add some jitter to the reported data size, but don't make it fully random to
        // dodge a Win32 RC bug that can cause infinite `.res` filesizes when the
        // reported data size is large enough that it gets interpreted as negative.
        const reported_data_size = if (rand.boolean())
            @as(u32, @intCast(@max(0, @as(i33, @intCast(real_data_size)) - rand.int(i8))))
        else
            real_data_size;
        try icon_writer.writeInt(u32, reported_data_size, .little);
        // Offset to the first icon, 0x16 is correct if there's 1 icon
        try icon_writer.writeInt(u32, if (rand.int(u8) == 0) rand.int(u32) else 0x16, .little);

        // half the time write enough of a PNG that the RC compiler treats it as a PNG
        if (make_png) {
            try icon_writer.writeAll("\x89PNG\r\n\x1a\n");
            try icon_writer.writeInt(u32, rand.int(u32), .big); // IHDR chunk size
            try icon_writer.writeAll("IHDR");
        }

        // and now a bunch of random bytes
        try icon_buffer.ensureUnusedCapacity(random_bytes_len);

        const slice_to_fill = try icon_writer.writableSlice(random_bytes_len);
        rand.bytes(slice_to_fill);

        try tmp.dir.writeFile(.{ .sub_path = "test.ico", .data = icon_buffer.written() });

        // also write it to the top-level tmp dir for debugging
        if (fuzzy_options.fuzzy_debug)
            try std.fs.cwd().writeFile(.{ .sub_path = ".zig-cache/tmp/fuzzy_icons.ico", .data = icon_buffer.written() });

        try utils.expectSameResOutput(allocator, source, .{
            .cwd = tmp.dir,
            .cwd_path = tmp_path,
        });
    }
}
