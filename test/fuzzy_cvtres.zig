const std = @import("std");
const utils = @import("utils.zig");
const fuzzy_options = @import("fuzzy_options");
const iterations = fuzzy_options.max_iterations;
const resinator = @import("resinator");
const Allocator = std.mem.Allocator;

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

    try writePreface(res_buffer.writer());
    const preface_len = res_buffer.items.len;

    var i: u64 = 0;
    while (iterations == 0 or i < iterations) : (i += 1) {
        res_buffer.shrinkRetainingCapacity(preface_len);

        const common_name = try getRandomNameOrOrdinal(allocator, rand, 32);
        defer common_name.deinit(allocator);
        const common_type = try getRandomNameOrOrdinal(allocator, rand, 32);
        defer common_type.deinit(allocator);
        const common_lang: resinator.res.Language = @bitCast(rand.int(u16));

        const num_resources = rand.intRangeAtMost(usize, 1, 16);
        for (0..num_resources) |_| {
            const options: RandomResourceOptions = switch (rand.uintAtMost(u8, 4)) {
                0 => .{ .set_name = common_name },
                1 => .{ .set_type = common_type },
                2 => .{ .set_language = common_lang },
                3 => .{ .set_name = common_name, .set_type = common_type },
                4 => .{},
                else => unreachable,
            };
            try writeRandomResource(allocator, rand, res_buffer.writer(), options);
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

fn writePreface(writer: anytype) !void {
    try writer.writeAll("\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00");
}

const RandomResourceOptions = struct {
    set_name: ?resinator.res.NameOrOrdinal = null,
    set_type: ?resinator.res.NameOrOrdinal = null,
    set_language: ?resinator.res.Language = null,
};

fn writeRandomResource(allocator: Allocator, rand: std.Random, writer: anytype, options: RandomResourceOptions) !void {
    const data_size = rand.uintAtMostBiased(u32, 150);
    const name_value = options.set_name orelse try getRandomNameOrOrdinal(allocator, rand, 32);
    defer if (options.set_name == null) name_value.deinit(allocator);
    const type_value = options.set_type orelse try getRandomNameOrOrdinal(allocator, rand, 32);
    defer if (options.set_type == null) type_value.deinit(allocator);

    const header = resinator.compile.Compiler.ResourceHeader{
        .name_value = name_value,
        .type_value = type_value,
        .language = options.set_language orelse @bitCast(rand.int(u16)),
        .memory_flags = @bitCast(rand.int(u16)),
        .data_size = data_size,
        .version = rand.int(u32),
        .characteristics = rand.int(u32),
        .data_version = rand.int(u32),
    };
    // only possible error is a field overflowing a u32, which we know can't happen
    const size_info = header.calcSize() catch unreachable;
    try header.writeSizeInfo(writer, size_info);

    const data = try allocator.alloc(u8, data_size);
    defer allocator.free(data);

    rand.bytes(data);
    try writer.writeAll(data);
    const num_padding_bytes = resinator.compile.Compiler.numPaddingBytesNeeded(data_size);
    try writer.writeByteNTimes(0, num_padding_bytes);
}

fn getRandomNameOrOrdinal(allocator: Allocator, rand: std.Random, max_name_len: usize) !resinator.res.NameOrOrdinal {
    return switch (rand.boolean()) {
        true => resinator.res.NameOrOrdinal{ .name = try getRandomName(allocator, rand, max_name_len) },
        false => resinator.res.NameOrOrdinal{ .ordinal = rand.int(u16) },
    };
}

fn getRandomName(allocator: Allocator, rand: std.Random, max_len: usize) ![:0]const u16 {
    const code_unit_len = rand.uintAtMost(usize, max_len);
    const buf = try allocator.allocSentinel(u16, code_unit_len, 0);
    errdefer allocator.free(buf);

    for (buf) |*code_unit| {
        if (rand.boolean()) {
            // random ASCII codepoint, except NUL
            code_unit.* = rand.intRangeAtMost(u7, 1, std.math.maxInt(u7));
        } else {
            // entirely random code unit, except NUL
            code_unit.* = rand.intRangeAtMost(u16, 1, std.math.maxInt(u16));
        }
    }

    return buf;
}
