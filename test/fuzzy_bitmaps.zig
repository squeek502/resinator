const std = @import("std");
const utils = @import("test_utils");
const fuzzy_options = @import("fuzzy_options");
const iterations = fuzzy_options.max_iterations;
const resinator = @import("resinator");

// Note: This does not test against the Win32 compiler since it can easily run into
//       infinite or pseudo-infinite loops on malformed bitmaps.
test "BITMAP fuzz" {
    // Use a single tmp dir to avoid creating and cleaning up a dir for each RC invocation
    // Unfortunately there doesn't seem to be a way to avoid hitting the filesystem,
    // the Windows RC compiler doesn't seem to like named pipes for either input or output
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const allocator = std.testing.allocator;
    var random = std.Random.DefaultPrng.init(0);
    var rand = random.random();

    var image_buffer = std.ArrayList(u8).init(allocator);
    defer image_buffer.deinit();

    const source = "1 BITMAP test.bin";

    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var i: u64 = 0;
    while (iterations == 0 or i < iterations) : (i += 1) {
        image_buffer.shrinkRetainingCapacity(0);
        const image_writer = image_buffer.writer();

        const random_bytes_len = rand.uintLessThanBiased(u32, 1000);

        const bmp_file_header_len = 14;
        const dib_header_len = 40;
        const real_data_size = random_bytes_len + bmp_file_header_len + dib_header_len;

        const reported_data_size = if (rand.boolean())
            // essentially `real_data_size -| rand.int(i8)`
            @as(u32, @intCast(@max(0, @as(i33, @intCast(real_data_size)) - rand.int(i8))))
        else
            real_data_size;

        // write a bitmap header and DIB header
        try image_writer.writeAll("BM");
        try image_writer.writeInt(u32, reported_data_size, .little); // size of the file (including everything)
        try image_writer.writeInt(u16, rand.int(u16), .little); // reserved
        try image_writer.writeInt(u16, rand.int(u16), .little); // reserved
        // offset of bmp image data
        // Note: This being larger than the file size can trigger some weird behavior in the
        //       Win32 rc compiler
        try image_writer.writeInt(u32, if (rand.boolean()) bmp_file_header_len + 40 else rand.uintLessThanBiased(u32, real_data_size + 100), .little);

        const dib_header_size: u32 = if (rand.boolean()) 40 else rand.int(u32);
        try image_writer.writeInt(u32, dib_header_size, .little);
        try image_writer.writeInt(i32, rand.int(i32), .little); // width
        try image_writer.writeInt(i32, rand.int(i32), .little); // height
        try image_writer.writeInt(u16, rand.int(u16), .little); // planes
        try image_writer.writeInt(u16, rand.uintLessThanBiased(u16, 34), .little); // bits_per_pixel
        try image_writer.writeInt(u32, rand.uintLessThanBiased(u32, 15), .little); // compression
        try image_writer.writeInt(u32, rand.int(u32), .little); // image_size
        try image_writer.writeInt(i32, rand.int(i32), .little); // x_pixels_per_meter
        try image_writer.writeInt(i32, rand.int(i32), .little); // y_pixels_per_meter
        // This being large can trigger `out of memory` or really long .res compile
        // times in the Win32 rc compiler
        try image_writer.writeInt(u32, if (rand.boolean()) rand.uintLessThan(u32, 500) else rand.int(u32), .little); // num_colors
        try image_writer.writeInt(u32, rand.int(u32), .little); // important colors used

        // and now a bunch of random bytes
        try image_buffer.ensureUnusedCapacity(random_bytes_len);

        const slice_to_fill = image_buffer.unusedCapacitySlice()[0..random_bytes_len];
        rand.bytes(slice_to_fill);

        image_buffer.items.len += random_bytes_len;

        try tmp.dir.writeFile(.{ .sub_path = "test.bin", .data = image_buffer.items });

        // also write it to the top-level tmp dir for debugging
        if (fuzzy_options.fuzzy_debug)
            try std.fs.cwd().writeFile(.{ .sub_path = ".zig-cache/tmp/fuzzy_bitmaps.bin", .data = image_buffer.items });

        var diagnostics = resinator.errors.Diagnostics.init(allocator);
        defer diagnostics.deinit();

        buffer.shrinkRetainingCapacity(0);
        if (resinator.compile.compile(allocator, source, buffer.writer(), .{ .cwd = tmp.dir, .diagnostics = &diagnostics })) {
            diagnostics.renderToStdErrDetectTTY(tmp.dir, source, null);
        } else |err| switch (err) {
            error.ParseError, error.CompileError => {
                diagnostics.renderToStdErrDetectTTY(tmp.dir, source, null);
            },
            else => return err,
        }
    }
}
