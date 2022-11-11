const std = @import("std");
const UncheckedSliceWriter = @import("utils.zig").UncheckedSliceWriter;

/// rc is maximally liberal in terms of what it accepts as a number literal
/// for data values. As long as it starts with a number or - or ~, that's good enough.
pub fn isValidNumberDataLiteral(str: []const u8) bool {
    if (str.len == 0) return false;
    switch (str[0]) {
        '~', '-', '0'...'9' => return true,
        else => return false,
    }
}

/// Valid escapes:
///  "" -> "
///  \a => 0x08 (not 0x07 like in C)
///  \n => 0x0A
///  \r => 0x0D
///  \t => 0x09
///  \\ => \
///  \nnn => byte with numeric value given by nnn interpreted as octal
///          (wraps on overflow, number of digits can be 1-3)
///  \xhh => byte with numeric value given by hh interpreted as hex
///          (number of digits can be 0-2)
pub fn parseQuotedAsciiString(str: []const u8, dest_buf: []u8) []u8 {
    std.debug.assert(str.len >= 2); // must at least have 2 double quote chars
    std.debug.assert(dest_buf.len >= str.len);

    var source = str[1..(str.len - 1)]; // remove start and end "
    var writer = UncheckedSliceWriter{ .slice = dest_buf };

    const State = enum {
        normal,
        quote,
        escaped,
        escaped_octal,
        escaped_hex,
    };

    var state: State = .normal;
    var string_escape_n: u8 = 0;
    var string_escape_i: std.math.IntFittingRange(0, 3) = 0;
    var index: usize = 0;
    while (index < source.len) : (index += 1) {
        const c = source[index];
        switch (state) {
            .normal => switch (c) {
                '\\' => state = .escaped,
                '"' => state = .quote,
                else => writer.write(c),
            },
            .quote => switch (c) {
                '"' => {
                    // "" => "
                    writer.write(c);
                    state = .normal;
                },
                else => unreachable, // this is a bug in the lexer
            },
            .escaped => switch (c) {
                '0'...'7' => {
                    string_escape_n = std.fmt.charToDigit(c, 8) catch unreachable;
                    string_escape_i = 1;
                    state = .escaped_octal;
                },
                'x', 'X' => {
                    string_escape_n = 0;
                    string_escape_i = 0;
                    state = .escaped_hex;
                },
                else => {
                    switch (c) {
                        'a', 'A' => writer.write('\x08'), // might be a bug in RC, but matches its behavior
                        'n', 'N' => writer.write('\n'),
                        'r', 'R' => writer.write('\r'),
                        't', 'T' => writer.write('\t'),
                        '\\' => writer.write('\\'),
                        else => {
                            writer.write('\\');
                            writer.write(c);
                        },
                    }
                    state = .normal;
                },
            },
            .escaped_octal => switch (c) {
                '0'...'7' => {
                    string_escape_n *%= 8;
                    string_escape_n +%= std.fmt.charToDigit(c, 8) catch unreachable;
                    string_escape_i += 1;
                    if (string_escape_i == 3) {
                        writer.write(string_escape_n);
                        state = .normal;
                    }
                },
                else => {
                    // write out whatever byte we have parsed so far
                    writer.write(string_escape_n);
                    // backtrack so that we handle the current char properly
                    index -= 1;
                    state = .normal;
                },
            },
            .escaped_hex => switch (c) {
                '0'...'9', 'a'...'f', 'A'...'F' => {
                    string_escape_n *= 16;
                    string_escape_n += std.fmt.charToDigit(c, 16) catch unreachable;
                    string_escape_i += 1;
                    if (string_escape_i == 2) {
                        writer.write(string_escape_n);
                        state = .normal;
                    }
                },
                else => {
                    // write out whatever byte we have parsed so far
                    // (even with 0 actual digits, \x alone parses to 0)
                    writer.write(string_escape_n);
                    // backtrack so that we handle the current char properly
                    index -= 1;
                    state = .normal;
                },
            },
        }
    }

    switch (state) {
        .normal => {},
        .escaped => writer.write('\\'),
        .escaped_octal, .escaped_hex => writer.write(string_escape_n),
        .quote => unreachable, // this is a bug in the lexer
    }

    return writer.getWritten();
}

pub fn parseQuotedAsciiStringAlloc(allocator: std.mem.Allocator, str: []const u8) ![]u8 {
    var buf = try std.ArrayList(u8).initCapacity(allocator, str.len);
    errdefer buf.deinit();

    buf.expandToCapacity();
    const parsed = parseQuotedAsciiString(str, buf.items);
    buf.shrinkRetainingCapacity(parsed.len);
    return buf.toOwnedSlice();
}

test "parse quoted ascii string" {
    var buf_arr: [100]u8 = undefined;
    var buf: []u8 = &buf_arr;

    try std.testing.expectEqualSlices(u8, "hello", parseQuotedAsciiString(
        \\"hello"
    , buf));
    // hex with 0 digits
    try std.testing.expectEqualSlices(u8, "\x00", parseQuotedAsciiString(
        \\"\x"
    , buf));
    // hex max of 2 digits
    try std.testing.expectEqualSlices(u8, "\xFFf", parseQuotedAsciiString(
        \\"\XfFf"
    , buf));
    // octal with invalid octal digit
    try std.testing.expectEqualSlices(u8, "\x019", parseQuotedAsciiString(
        \\"\19"
    , buf));
    // escaped quotes
    try std.testing.expectEqualSlices(u8, " \" ", parseQuotedAsciiString(
        \\" "" "
    , buf));
    // octal overflow
    try std.testing.expectEqualSlices(u8, "\x01", parseQuotedAsciiString(
        \\"\401"
    , buf));
    // escapes
    try std.testing.expectEqualSlices(u8, "\x08\n\r\t\\", parseQuotedAsciiString(
        \\"\a\n\r\t\\"
    , buf));
    // uppercase escapes
    try std.testing.expectEqualSlices(u8, "\x08\n\r\t\\", parseQuotedAsciiString(
        \\"\A\N\R\T\\"
    , buf));
    // backslash on its own
    try std.testing.expectEqualSlices(u8, "\\", parseQuotedAsciiString(
        \\"\"
    , buf));
    // unrecognized escapes
    try std.testing.expectEqualSlices(u8, "\\b", parseQuotedAsciiString(
        \\"\b"
    , buf));
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

/// Assumes that number literals normally rejected by RC's preprocessor
/// are similarly rejected before being parsed.
///
/// Relevant RC preprocessor errors:
///  RC2021: expected exponent value, not '<digit>'
///   example that is rejected: 1e1
///   example that is accepted: 1ea
///   (this function will parse the two examples above the same)
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
        switch (buf[1]) {
            'o' => { // octal radix prefix is case-sensitive
                radix = 8;
                buf = buf[2..];
            },
            'x', 'X' => {
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
    try std.testing.expectEqual(Number{ .value = 0, .is_long = false }, parseNumberLiteral("0O29"));

    try std.testing.expectEqual(Number{ .value = 0xFFFFFFFF, .is_long = false }, parseNumberLiteral("-1"));
    try std.testing.expectEqual(Number{ .value = 0xFFFFFFFE, .is_long = false }, parseNumberLiteral("~1"));
    try std.testing.expectEqual(Number{ .value = 0xFFFFFFFF, .is_long = true }, parseNumberLiteral("-4294967297L"));
    try std.testing.expectEqual(Number{ .value = 0xFFFFFFFE, .is_long = true }, parseNumberLiteral("~4294967297L"));
    try std.testing.expectEqual(Number{ .value = 0xFFFFFFFD, .is_long = false }, parseNumberLiteral("-0X3"));
}
