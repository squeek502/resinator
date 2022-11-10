const std = @import("std");

/// rc is maximally liberal in terms of what it accepts as a number literal
/// for data values. As long as it starts with a number or - or ~, that's good enough.
pub fn isValidNumberDataLiteral(str: []const u8) bool {
    if (str.len == 0) return false;
    switch (str[0]) {
        '~', '-', '0'...'9' => return true,
        else => return false,
    }
}

/// TODO: Real implementation
pub fn parseQuotedAsciiString(str: []const u8) []const u8 {
    std.debug.assert(str.len >= 2); // must at least have 2 double quote chars
    return str[1..(str.len - 1)];
}

/// TODO: Real implemenation, probably needing to take code_page into account
pub fn parseQuotedWideStringAlloc(allocator: std.mem.Allocator, str: []const u8) ![:0]u16 {
    std.debug.assert(str.len >= 3); // L""
    const without_quotes = str[2..(str.len - 1)];
    return try std.unicode.utf8ToUtf16LeWithNull(allocator, without_quotes);
}

pub const Number = struct {
    value: u32,
    is_long: bool,

    pub fn asWord(self: Number) u16 {
        std.debug.assert(!self.is_long); // asWord should not be called on long numbers
        return @truncate(u16, self.value);
    }
};

pub fn parseNumberLiteral(str: []const u8) Number {
    std.debug.assert(str.len > 0);
    var result = Number{ .value = 0, .is_long = false };
    var radix: u8 = 10;
    var buf = str;

    const Prefix = enum { none, minus, complement };
    var prefix: Prefix = .none;
    switch (buf[0]) {
        '-' => {
            prefix = .minus;
            buf = buf[1..];
        },
        '~' => {
            prefix = .complement;
            buf = buf[1..];
        },
        else => {},
    }

    if (buf.len > 2 and buf[0] == '0') {
        switch (std.ascii.toLower(str[1])) {
            'o' => {
                radix = 8;
                buf = buf[2..];
            },
            'x' => {
                radix = 16;
                buf = buf[2..];
            },
            else => {},
        }
    }

    for (buf) |c| {
        if (c == 'L' or c == 'l') {
            result.is_long = true;
        }
        // on invalid digit for the radix, just stop parsing but don't fail
        const digit = std.fmt.charToDigit(c, radix) catch break;

        if (result.value != 0) {
            result.value *%= radix;
        }
        result.value +%= digit;
    }

    switch (prefix) {
        .none => {},
        .minus => result.value = 0 -% result.value,
        .complement => result.value = ~result.value,
    }

    return result;
}

test "parse number literal" {
    try std.testing.expectEqual(Number{ .value = 0, .is_long = false }, parseNumberLiteral("0"));
    try std.testing.expectEqual(Number{ .value = 1, .is_long = false }, parseNumberLiteral("1"));
    try std.testing.expectEqual(Number{ .value = 1, .is_long = true }, parseNumberLiteral("1L"));
    try std.testing.expectEqual(Number{ .value = 1, .is_long = true }, parseNumberLiteral("1l"));
    try std.testing.expectEqual(Number{ .value = 1, .is_long = false }, parseNumberLiteral("1garbageL"));
    try std.testing.expectEqual(Number{ .value = 4294967295, .is_long = false }, parseNumberLiteral("4294967295"));
    try std.testing.expectEqual(Number{ .value = 0, .is_long = false }, parseNumberLiteral("4294967296"));
    try std.testing.expectEqual(Number{ .value = 1, .is_long = true }, parseNumberLiteral("4294967297L"));

    // can handle any length of number, wraps on overflow appropriately
    const big_overflow = parseNumberLiteral("1000000000000000000000000000000000000000000000000000000000000000000000000000000090000000001");
    try std.testing.expectEqual(Number{ .value = 4100654081, .is_long = false }, big_overflow);
    try std.testing.expectEqual(@as(u16, 1025), big_overflow.asWord());

    try std.testing.expectEqual(Number{ .value = 0x20, .is_long = false }, parseNumberLiteral("0x20"));
    try std.testing.expectEqual(Number{ .value = 0x2A, .is_long = true }, parseNumberLiteral("0x2AL"));
    try std.testing.expectEqual(Number{ .value = 0x2A, .is_long = true }, parseNumberLiteral("0x2aL"));
    try std.testing.expectEqual(Number{ .value = 0x2A, .is_long = true }, parseNumberLiteral("0x2aL"));

    try std.testing.expectEqual(Number{ .value = 0o20, .is_long = false }, parseNumberLiteral("0o20"));
    try std.testing.expectEqual(Number{ .value = 0o20, .is_long = true }, parseNumberLiteral("0o20L"));
    try std.testing.expectEqual(Number{ .value = 0o2, .is_long = false }, parseNumberLiteral("0o29"));

    try std.testing.expectEqual(Number{ .value = 0xFFFFFFFF, .is_long = false }, parseNumberLiteral("-1"));
    try std.testing.expectEqual(Number{ .value = 0xFFFFFFFE, .is_long = false }, parseNumberLiteral("~1"));
    try std.testing.expectEqual(Number{ .value = 0xFFFFFFFF, .is_long = true }, parseNumberLiteral("-4294967297L"));
    try std.testing.expectEqual(Number{ .value = 0xFFFFFFFE, .is_long = true }, parseNumberLiteral("~4294967297L"));
}
