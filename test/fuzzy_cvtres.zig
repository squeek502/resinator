const std = @import("std");
const utils = @import("utils.zig");
const fuzzy_options = @import("fuzzy_options");
const iterations = fuzzy_options.max_iterations;
const resinator = @import("resinator");

test "cvtres fuzz" {
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

        const common_name = try utils.getRandomNameOrOrdinal(allocator, rand, 32);
        defer common_name.deinit(allocator);
        const common_type = try utils.getRandomNameOrOrdinal(allocator, rand, 32);
        defer common_type.deinit(allocator);
        const common_lang: resinator.res.Language = @bitCast(rand.int(u16));

        const num_resources = rand.intRangeAtMost(usize, 1, 16);
        for (0..num_resources) |_| {
            const options: utils.RandomResourceOptions = switch (rand.uintAtMost(u8, 4)) {
                0 => .{ .set_name = common_name },
                1 => .{ .set_type = common_type },
                2 => .{ .set_language = common_lang },
                3 => .{ .set_name = common_name, .set_type = common_type },
                4 => .{},
                else => unreachable,
            };
            try utils.writeRandomValidResource(allocator, rand, res_buffer.writer(), options);
        }

        // also write it to the top-level tmp dir for debugging
        try std.fs.cwd().writeFile(.{ .sub_path = ".zig-cache/tmp/fuzzy_cvtres.res", .data = res_buffer.items });

        const random_target: std.coff.MachineType = switch (rand.uintLessThan(u8, 8)) {
            0 => .X64,
            1 => .I386,
            2 => .ARMNT,
            3 => .ARM64,
            4 => .ARM64EC,
            5 => .ARM64X,
            6 => .IA64,
            7 => .EBC,
            else => unreachable,
        };

        try utils.expectSameCvtResOutput(allocator, res_buffer.items, .{
            .cwd = tmp.dir,
            .cwd_path = tmp_path,
            .target = random_target,
        });
    }
}
