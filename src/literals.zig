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

/// Valid escapes:
///  "" -> "
///  \a, \A => 0x08 (not 0x07 like in C)
///  \n => 0x0A
///  \r => 0x0D
///  \t, \T => 0x09
///  \\ => \
///  \nnn => byte with numeric value given by nnn interpreted as octal
///          (wraps on overflow, number of digits can be 1-3)
///  \xhh => byte with numeric value given by hh interpreted as hex
///          (number of digits can be 0-2)
///  \<\r+> => \
///  \<[\r\n\t ]+> => <nothing>
///
/// Special cases:
///  <\t> => 1-8 spaces, dependent on columns in the source rc file itself
///  <\r> => <nothing>
///  <\n+><\w+?\n?> => <space><\n>
///
/// Special, especially weird case:
///  \"" => "
/// NOTE: This leads to footguns because the preprocessor can start parsing things
///       out-of-sync with the RC compiler, expanding macros within string literals, etc.
///       This parse function handles this case the same as the Windows RC compiler, but
///       \" within a string literal is treated as an error by the lexer, so the relevant
///       branches should never actually be hit during this function.
pub fn parseQuotedAsciiString(allocator: std.mem.Allocator, str: []const u8, start_column: usize) ![]u8 {
    std.debug.assert(str.len >= 2); // must at least have 2 double quote chars

    var buf = try std.ArrayList(u8).initCapacity(allocator, str.len);
    errdefer buf.deinit();

    var source = str[1..(str.len - 1)]; // remove start and end "

    const State = enum {
        normal,
        quote,
        newline,
        escaped,
        escaped_cr,
        escaped_newlines,
        escaped_octal,
        escaped_hex,
    };

    var column: usize = start_column + 1; // The starting " is included
    var state: State = .normal;
    var string_escape_n: u8 = 0;
    var string_escape_i: std.math.IntFittingRange(0, 3) = 0;
    var index: usize = 0;
    while (index < source.len) : (index += 1) {
        const c = source[index];
        var backtrack = false;
        defer {
            if (backtrack) {
                index -= 1;
            } else {
                if (c == '\t') {
                    column += columnsUntilTabStop(column, 8);
                } else {
                    column += 1;
                }
            }
        }
        switch (state) {
            .normal => switch (c) {
                '\\' => state = .escaped,
                '"' => state = .quote,
                '\r' => {},
                '\n' => state = .newline,
                '\t' => {
                    var space_i: usize = 0;
                    const cols = columnsUntilTabStop(column, 8);
                    while (space_i < cols) : (space_i += 1) {
                        try buf.append(' ');
                    }
                },
                else => try buf.append(c),
            },
            .quote => switch (c) {
                '"' => {
                    // "" => "
                    try buf.append(c);
                    state = .normal;
                },
                else => unreachable, // this is a bug in the lexer
            },
            .newline => switch (c) {
                '\r', ' ', '\t', '\n' => {},
                else => {
                    try buf.append(' ');
                    try buf.append('\n');
                    // backtrack so that we handle the current char properly
                    backtrack = true;
                    state = .normal;
                },
            },
            .escaped => switch (c) {
                '\r' => state = .escaped_cr,
                '\n' => state = .escaped_newlines,
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
                        'a', 'A' => try buf.append('\x08'), // might be a bug in RC, but matches its behavior
                        'n' => try buf.append('\n'),
                        'r' => try buf.append('\r'),
                        't', 'T' => try buf.append('\t'),
                        '\\' => try buf.append('\\'),
                        '"' => {
                            // \" is a special case that doesn't get the \ included,
                            backtrack = true;
                        },
                        else => {
                            try buf.append('\\');
                            // backtrack so that we handle the current char properly
                            backtrack = true;
                        },
                    }
                    state = .normal;
                },
            },
            .escaped_cr => switch (c) {
                '\r' => {},
                '\n' => state = .escaped_newlines,
                else => {
                    try buf.append('\\');
                    // backtrack so that we handle the current char properly
                    backtrack = true;
                    state = .normal;
                },
            },
            .escaped_newlines => switch (c) {
                '\r', '\n', '\t', ' ' => {},
                else => {
                    // backtrack so that we handle the current char properly
                    backtrack = true;
                    state = .normal;
                },
            },
            .escaped_octal => switch (c) {
                '0'...'7' => {
                    string_escape_n *%= 8;
                    string_escape_n +%= std.fmt.charToDigit(c, 8) catch unreachable;
                    string_escape_i += 1;
                    if (string_escape_i == 3) {
                        try buf.append(string_escape_n);
                        state = .normal;
                    }
                },
                else => {
                    // write out whatever byte we have parsed so far
                    try buf.append(string_escape_n);
                    // backtrack so that we handle the current char properly
                    backtrack = true;
                    state = .normal;
                },
            },
            .escaped_hex => switch (c) {
                '0'...'9', 'a'...'f', 'A'...'F' => {
                    string_escape_n *= 16;
                    string_escape_n += std.fmt.charToDigit(c, 16) catch unreachable;
                    string_escape_i += 1;
                    if (string_escape_i == 2) {
                        try buf.append(string_escape_n);
                        state = .normal;
                    }
                },
                else => {
                    // write out whatever byte we have parsed so far
                    // (even with 0 actual digits, \x alone parses to 0)
                    try buf.append(string_escape_n);
                    // backtrack so that we handle the current char properly
                    backtrack = true;
                    state = .normal;
                },
            },
        }
    }

    switch (state) {
        .normal, .escaped_newlines => {},
        .newline => {
            try buf.append(' ');
            try buf.append('\n');
        },
        .escaped, .escaped_cr => try buf.append('\\'),
        .escaped_octal, .escaped_hex => try buf.append(string_escape_n),
        .quote => unreachable, // this is a bug in the lexer
    }

    return buf.toOwnedSlice();
}

pub fn columnsUntilTabStop(column: usize, tab_columns: usize) usize {
    // 0 => 8, 1 => 7, 2 => 6, 3 => 5, 4 => 4
    // 5 => 3, 6 => 2, 7 => 1, 8 => 8
    return tab_columns - (column % tab_columns);
}

test "parse quoted ascii string" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    try std.testing.expectEqualSlices(u8, "hello", try parseQuotedAsciiString(arena,
        \\"hello"
    , 0));
    // hex with 0 digits
    try std.testing.expectEqualSlices(u8, "\x00", try parseQuotedAsciiString(arena,
        \\"\x"
    , 0));
    // hex max of 2 digits
    try std.testing.expectEqualSlices(u8, "\xFFf", try parseQuotedAsciiString(arena,
        \\"\XfFf"
    , 0));
    // octal with invalid octal digit
    try std.testing.expectEqualSlices(u8, "\x019", try parseQuotedAsciiString(arena,
        \\"\19"
    , 0));
    // escaped quotes
    try std.testing.expectEqualSlices(u8, " \" ", try parseQuotedAsciiString(arena,
        \\" "" "
    , 0));
    // backslash right before escaped quotes
    try std.testing.expectEqualSlices(u8, "\"", try parseQuotedAsciiString(arena,
        \\"\"""
    , 0));
    // octal overflow
    try std.testing.expectEqualSlices(u8, "\x01", try parseQuotedAsciiString(arena,
        \\"\401"
    , 0));
    // escapes
    try std.testing.expectEqualSlices(u8, "\x08\n\r\t\\", try parseQuotedAsciiString(arena,
        \\"\a\n\r\t\\"
    , 0));
    // uppercase escapes
    try std.testing.expectEqualSlices(u8, "\x08\\N\\R\t\\", try parseQuotedAsciiString(arena,
        \\"\A\N\R\T\\"
    , 0));
    // backslash on its own
    try std.testing.expectEqualSlices(u8, "\\", try parseQuotedAsciiString(arena,
        \\"\"
    , 0));
    // unrecognized escapes
    try std.testing.expectEqualSlices(u8, "\\b", try parseQuotedAsciiString(arena,
        \\"\b"
    , 0));
    // escaped carriage returns
    try std.testing.expectEqualSlices(u8, "\\", try parseQuotedAsciiString(
        arena,
        "\"\\\r\r\r\r\r\"",
        0,
    ));
    // escaped newlines
    try std.testing.expectEqualSlices(u8, "", try parseQuotedAsciiString(
        arena,
        "\"\\\n\n\n\n\n\"",
        0,
    ));
    // escaped CRLF pairs
    try std.testing.expectEqualSlices(u8, "", try parseQuotedAsciiString(
        arena,
        "\"\\\r\n\r\n\r\n\r\n\r\n\"",
        0,
    ));
    // escaped newlines with other whitespace
    try std.testing.expectEqualSlices(u8, "", try parseQuotedAsciiString(
        arena,
        "\"\\\n    \t\r\n \r\t\n  \t\"",
        0,
    ));
    // literal tab characters get converted to spaces (dependent on source file columns)
    try std.testing.expectEqualSlices(u8, "       ", try parseQuotedAsciiString(
        arena,
        "\"\t\"",
        0,
    ));
    try std.testing.expectEqualSlices(u8, "abc    ", try parseQuotedAsciiString(
        arena,
        "\"abc\t\"",
        0,
    ));
    try std.testing.expectEqualSlices(u8, "abcdefg        ", try parseQuotedAsciiString(
        arena,
        "\"abcdefg\t\"",
        0,
    ));
    try std.testing.expectEqualSlices(u8, "\\      ", try parseQuotedAsciiString(
        arena,
        "\"\\\t\"",
        0,
    ));
    // literal CR's get dropped
    try std.testing.expectEqualSlices(u8, "", try parseQuotedAsciiString(
        arena,
        "\"\r\r\r\r\r\"",
        0,
    ));
    // contiguous newlines and whitespace get collapsed to <space><newline>
    try std.testing.expectEqualSlices(u8, " \n", try parseQuotedAsciiString(
        arena,
        "\"\n\r\r  \r\n \t  \"",
        0,
    ));
}

/// TODO: Real implemenation, probably needing to take code_page into account
/// Notes:
/// - Wide strings allow for 4 hex digits in escapes (e.g. \xC2AD gets parsed as 0xC2AD)
pub fn parseQuotedWideStringAlloc(allocator: std.mem.Allocator, str: []const u8, start_column: usize) ![:0]u16 {
    std.debug.assert(str.len >= 3); // L""
    const parsed = try parseQuotedAsciiString(allocator, str[1..], start_column);
    defer allocator.free(parsed);
    return try std.unicode.utf8ToUtf16LeWithNull(allocator, parsed);
}

pub const Number = struct {
    value: u32,
    is_long: bool,

    pub fn asWord(self: Number) u16 {
        return @truncate(u16, self.value);
    }

    pub fn evaluateOperator(lhs: Number, operator_char: u8, rhs: Number) Number {
        const result = switch (operator_char) {
            '-' => lhs.value -% rhs.value,
            '+' => lhs.value +% rhs.value,
            '|' => lhs.value | rhs.value,
            '&' => lhs.value & rhs.value,
            else => unreachable, // invalid operator, this would be a lexer/parser bug
        };
        return .{
            .value = result,
            .is_long = lhs.is_long or rhs.is_long,
        };
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
            break;
        }
        // on invalid digit for the radix, just stop parsing but don't fail
        const digit = std.fmt.charToDigit(c, radix) catch switch (c) {
            // I have no idea why this is the case, but the Windows RC compiler
            // treats ¹, ², and ³ characters as valid digits when the radix is 10
            // TODO: This is the Windows-1252-encoded version of this, need to
            //       handle UTF-8, etc.
            '\xb2', '\xb3', '\xb9' => if (radix != 10) break else c - 0x30,
            else => break,
        };

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

    // anything after L is ignored
    try std.testing.expectEqual(Number{ .value = 0x2A, .is_long = true }, parseNumberLiteral("0x2aL5"));
}

test "superscript characters in number literals" {
    // In Windows-1252, ² is \xb2, ³ is \xb3, ¹ is \xb9
    try std.testing.expectEqual(Number{ .value = 130, .is_long = false }, parseNumberLiteral("\xb2"));
    try std.testing.expectEqual(Number{ .value = 131, .is_long = false }, parseNumberLiteral("\xb3"));
    try std.testing.expectEqual(Number{ .value = 137, .is_long = false }, parseNumberLiteral("\xb9"));

    try std.testing.expectEqual(Number{ .value = 147, .is_long = false }, parseNumberLiteral("1\xb9"));
    try std.testing.expectEqual(Number{ .value = 157, .is_long = false }, parseNumberLiteral("2\xb9"));
    try std.testing.expectEqual(Number{ .value = 167, .is_long = false }, parseNumberLiteral("3\xb9"));
    try std.testing.expectEqual(Number{ .value = 227, .is_long = false }, parseNumberLiteral("9\xb9"));

    try std.testing.expectEqual(Number{ .value = 237, .is_long = false }, parseNumberLiteral("10\xb9"));
    try std.testing.expectEqual(Number{ .value = 337, .is_long = false }, parseNumberLiteral("20\xb9"));

    try std.testing.expectEqual(Number{ .value = 1507, .is_long = false }, parseNumberLiteral("\xb9\xb9"));
    try std.testing.expectEqual(Number{ .value = 15131, .is_long = false }, parseNumberLiteral("\xb9\xb2\xb3"));
}
