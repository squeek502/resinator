const std = @import("std");
const fuzzy_options = @import("fuzzy_options");
const iterations = fuzzy_options.max_iterations;
const resinator = @import("resinator");

test "FONT fuzz" {
    const allocator = std.testing.allocator;
    var random = std.Random.DefaultPrng.init(0);
    var rand = random.random();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);

    const max_file_len = 1000;
    var font_buffer = try std.array_list.Managed(u8).initCapacity(allocator, max_file_len);
    defer font_buffer.deinit();

    const source = "1 FONT test.fnt";

    var buffer: std.Io.Writer.Allocating = .init(allocator);
    defer buffer.deinit();

    var i: u64 = 0;
    while (iterations == 0 or i < iterations) : (i += 1) {
        font_buffer.shrinkRetainingCapacity(0);

        // Just a bunch of random bytes
        const random_bytes_len = rand.uintLessThanBiased(u32, max_file_len);
        try font_buffer.ensureUnusedCapacity(random_bytes_len);
        const slice_to_fill = font_buffer.unusedCapacitySlice()[0..random_bytes_len];
        rand.bytes(slice_to_fill);
        font_buffer.items.len += random_bytes_len;

        try tmp.dir.writeFile(.{ .sub_path = "test.fnt", .data = font_buffer.items });

        // also write it to the top-level tmp dir for debugging
        try std.fs.cwd().writeFile(.{ .sub_path = ".zig-cache/tmp/fuzzy_fonts.fnt", .data = font_buffer.items });

        var diagnostics = resinator.errors.Diagnostics.init(allocator);
        defer diagnostics.deinit();

        buffer.shrinkRetainingCapacity(0);
        if (resinator.compile.compile(allocator, source, &buffer.writer, .{ .cwd = tmp.dir, .diagnostics = &diagnostics })) {
            diagnostics.renderToStdErrDetectTTY(tmp.dir, source, null);
        } else |err| switch (err) {
            error.ParseError, error.CompileError => {
                diagnostics.renderToStdErrDetectTTY(tmp.dir, source, null);
            },
            else => return err,
        }
    }
}
