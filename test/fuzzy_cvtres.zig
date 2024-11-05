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

    var data_buffer = std.ArrayList(u8).init(allocator);
    defer data_buffer.deinit();

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

        data_buffer.clearRetainingCapacity();
        const data_size = rand.uintAtMostBiased(u32, 150);
        try data_buffer.resize(data_size);
        rand.bytes(data_buffer.items);
        const common_data = data_buffer.items;

        const num_resources = rand.intRangeAtMost(usize, 1, 16);
        for (0..num_resources) |resource_i| {
            var options: utils.RandomResourceOptions = switch (rand.uintAtMost(u8, 4)) {
                0 => .{ .set_name = common_name },
                1 => .{ .set_type = common_type },
                2 => .{ .set_language = common_lang },
                3 => .{ .set_name = common_name, .set_type = common_type },
                4 => .{},
                else => unreachable,
            };
            // Randomly set data to be a duplicate, but don't make the first resource have
            // the duplicate data to avoid what seems like a miscompilation when /FOLDDUPS is set.
            // If the first resource has the same data as another resource, the first
            // data is written but is unreferenced, and then the same data is written again
            // and that second data location is actually used for all duplicates of that data.
            if (resource_i > 0 and rand.float(f32) < 0.20) {
                options.set_data = common_data;
            }
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

        const random_symbol_define: ?[]const u8 = switch (rand.uintLessThan(u8, 3)) {
            0 => null,
            1 => "short", // fits within 8 bytes
            2 => "longerthan8", // needs to go in the string table
            else => unreachable,
        };

        try utils.expectSameCvtResOutput(allocator, res_buffer.items, .{
            .cwd = tmp.dir,
            .cwd_path = tmp_path,
            .target = random_target,
            .read_only = rand.boolean(),
            .define_external_symbol = random_symbol_define,
        });
    }
}
