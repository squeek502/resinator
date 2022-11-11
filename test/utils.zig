const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn randomNumberLiteral(allocator: Allocator, rand: std.rand.Random) ![]const u8 {
    var buf = std.ArrayList(u8).init(allocator);
    errdefer buf.deinit();

    const Prefix = enum { none, minus, complement };
    var prefix = rand.enumValue(Prefix);

    switch (prefix) {
        .none => {},
        .minus => try buf.append('-'),
        .complement => try buf.append('~'),
    }

    const has_radix = rand.boolean();
    if (has_radix) {
        try buf.append('0');
        try buf.append(randomAlphanumeric(rand));
    } else {
        // needs to start with a digit
        try buf.append(randomNumeric(rand));
    }

    // TODO: increase this limit?
    const length = rand.int(u8);
    var i: usize = 0;
    while (i < length) : (i += 1) {
        try buf.append(randomAlphanumeric(rand));
    }

    return buf.toOwnedSlice();
}

pub fn randomAlphanumeric(rand: std.rand.Random) u8 {
    const dict = [_]u8{
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
        'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
        'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D',
        'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
        'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
        'Y', 'Z',
    };
    var index = rand.uintLessThanBiased(u8, dict.len);
    return dict[index];
}

pub fn randomNumeric(rand: std.rand.Random) u8 {
    return rand.uintLessThanBiased(u8, 10) + '0';
}
