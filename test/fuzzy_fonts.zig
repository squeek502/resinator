const std = @import("std");
const fuzzy_options = @import("fuzzy_options");
const iterations = fuzzy_options.max_iterations;
const resinator = @import("resinator");

test "FONT fuzz" {
    const allocator = std.testing.allocator;
    var random = std.rand.DefaultPrng.init(0);
    var rand = random.random();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);

    var font_buffer = std.ArrayList(u8).init(allocator);
    defer font_buffer.deinit();

    const source = "1 FONT test.fnt";

    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var i: u64 = 0;
    while (iterations == 0 or i < iterations) : (i += 1) {
        font_buffer.shrinkRetainingCapacity(0);
        const font_writer = font_buffer.writer();

        var fnt_header = std.mem.zeroes(resinator.fnt.FontDirEntry);

        const random_bytes_len = rand.uintLessThanBiased(u32, 1000);
        const real_data_size = resinator.fnt.FontDirEntry.len + random_bytes_len;

        // Add some jitter to the reported data size, but don't make it fully random
        const reported_data_size = if (rand.boolean())
            @intCast(u32, @max(0, @intCast(i33, real_data_size) - rand.int(i8)))
        else
            real_data_size;

        fnt_header.size = reported_data_size;
        fnt_header.version = if (rand.boolean()) 0x300 else 0x200;
        fnt_header.device_offset = rand.uintLessThanBiased(u32, real_data_size + 10);
        fnt_header.face_offset = rand.uintLessThanBiased(u32, real_data_size + 10);
        rand.bytes(&fnt_header.copyright);

        try fnt_header.write(font_writer);

        // and now a bunch of random bytes
        try font_buffer.ensureUnusedCapacity(random_bytes_len);

        var slice_to_fill = font_buffer.unusedCapacitySlice()[0..random_bytes_len];
        rand.bytes(slice_to_fill);

        font_buffer.items.len += random_bytes_len;

        try tmp.dir.writeFile("test.fnt", font_buffer.items);

        // also write it to the top-level tmp dir for debugging
        try std.fs.cwd().writeFile("zig-cache/tmp/fuzzy_fonts.fnt", font_buffer.items);

        var diagnostics = resinator.errors.Diagnostics.init(allocator);
        defer diagnostics.deinit();

        buffer.shrinkRetainingCapacity(0);
        if (resinator.compile.compile(allocator, source, buffer.writer(), .{ .cwd = tmp.dir, .diagnostics = &diagnostics })) {
            diagnostics.renderToStdErr(tmp.dir, source, null);
        } else |err| switch (err) {
            error.ParseError, error.CompileError => {
                diagnostics.renderToStdErr(tmp.dir, source, null);
            },
            else => return err,
        }
    }
}
