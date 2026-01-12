const std = @import("std");
const utils = @import("test_utils");
const fuzzy_options = @import("fuzzy_options");
const iterations = fuzzy_options.max_iterations;
const resinator = @import("resinator");
const Allocator = std.mem.Allocator;

test "res preface fuzz" {
    const io = std.testing.io;
    const allocator = std.testing.allocator;
    var random = std.Random.DefaultPrng.init(0);
    var rand = random.random();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(tmp_path);

    var res_buffer: std.Io.Writer.Allocating = .init(allocator);
    defer res_buffer.deinit();

    var i: u64 = 0;
    while (iterations == 0 or i < iterations) : (i += 1) {
        res_buffer.clearRetainingCapacity();

        switch (rand.boolean()) {
            true => _ = try utils.writeRandomValidResource(allocator, rand, &res_buffer.writer, .{}),
            false => try utils.writeRandomPotentiallyInvalidResource(allocator, rand, &res_buffer.writer),
        }

        // also write it to the top-level tmp dir for debugging
        if (fuzzy_options.fuzzy_debug)
            try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = ".zig-cache/tmp/fuzzy_res_preface.res", .data = res_buffer.written() });

        var fbs: std.Io.Reader = .fixed(res_buffer.written());
        var resources = resinator.cvtres.parseRes(allocator, &fbs, .{
            .max_size = res_buffer.written().len,
        }) catch continue;
        defer resources.deinit();
    }
}

test "res fuzz" {
    const io = std.testing.io;
    const allocator = std.testing.allocator;
    var random = std.Random.DefaultPrng.init(0);
    var rand = random.random();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(tmp_path);

    var res_buffer: std.Io.Writer.Allocating = .init(allocator);
    defer res_buffer.deinit();

    try utils.writePreface(&res_buffer.writer);
    const preface_len = res_buffer.written().len;

    var i: u64 = 0;
    while (iterations == 0 or i < iterations) : (i += 1) {
        res_buffer.shrinkRetainingCapacity(preface_len);

        const num_resources = rand.intRangeAtMost(usize, 1, 16);
        for (0..num_resources) |_| {
            try utils.writeRandomPotentiallyInvalidResource(allocator, rand, &res_buffer.writer);
        }

        // also write it to the top-level tmp dir for debugging
        if (fuzzy_options.fuzzy_debug)
            try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = ".zig-cache/tmp/fuzzy_res.res", .data = res_buffer.written() });

        var fbs: std.Io.Reader = .fixed(res_buffer.written());
        var resources = resinator.cvtres.parseRes(allocator, &fbs, .{
            .max_size = res_buffer.written().len,
        }) catch continue;
        defer resources.deinit();
    }
}
