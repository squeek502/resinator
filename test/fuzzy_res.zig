const std = @import("std");
const utils = @import("utils.zig");
const fuzzy_options = @import("fuzzy_options");
const iterations = fuzzy_options.max_iterations;
const resinator = @import("resinator");
const Allocator = std.mem.Allocator;

test "res preface fuzz" {
    const allocator = std.testing.allocator;
    var random = std.Random.DefaultPrng.init(0);
    var rand = random.random();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);

    var res_buffer = std.ArrayList(u8).init(allocator);
    defer res_buffer.deinit();

    var i: u64 = 0;
    while (iterations == 0 or i < iterations) : (i += 1) {
        res_buffer.clearRetainingCapacity();

        switch (rand.boolean()) {
            true => try utils.writeRandomValidResource(allocator, rand, res_buffer.writer(), .{}),
            false => try utils.writeRandomPotentiallyInvalidResource(allocator, rand, res_buffer.writer()),
        }

        // also write it to the top-level tmp dir for debugging
        try std.fs.cwd().writeFile(.{ .sub_path = ".zig-cache/tmp/fuzzy_res_preface.res", .data = res_buffer.items });

        var fbs = std.io.fixedBufferStream(res_buffer.items);
        const resources = resinator.cvtres.parseRes(allocator, fbs.reader(), .{
            .max_size = res_buffer.items.len,
        }) catch continue;
        defer resinator.cvtres.freeResources(allocator, resources);
    }
}

test "res fuzz" {
    const allocator = std.testing.allocator;
    var random = std.Random.DefaultPrng.init(0);
    var rand = random.random();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);

    var res_buffer = std.ArrayList(u8).init(allocator);
    defer res_buffer.deinit();

    try utils.writePreface(res_buffer.writer());
    const preface_len = res_buffer.items.len;

    var i: u64 = 0;
    while (iterations == 0 or i < iterations) : (i += 1) {
        res_buffer.shrinkRetainingCapacity(preface_len);

        const num_resources = rand.intRangeAtMost(usize, 1, 16);
        for (0..num_resources) |_| {
            try utils.writeRandomPotentiallyInvalidResource(allocator, rand, res_buffer.writer());
        }

        // also write it to the top-level tmp dir for debugging
        try std.fs.cwd().writeFile(.{ .sub_path = ".zig-cache/tmp/fuzzy_res.res", .data = res_buffer.items });

        var fbs = std.io.fixedBufferStream(res_buffer.items);
        const resources = resinator.cvtres.parseRes(allocator, fbs.reader(), .{
            .max_size = res_buffer.items.len,
        }) catch continue;
        defer resinator.cvtres.freeResources(allocator, resources);
    }
}
