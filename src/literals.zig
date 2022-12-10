const std = @import("std");
const code_pages = @import("code_pages.zig");
const CodePage = code_pages.CodePage;
const windows1252 = @import("windows1252.zig");

/// rc is maximally liberal in terms of what it accepts as a number literal
/// for data values. As long as it starts with a number or - or ~, that's good enough.
pub fn isValidNumberDataLiteral(str: []const u8) bool {
    if (str.len == 0) return false;
    switch (str[0]) {
        '~', '-', '0'...'9' => return true,
        else => return false,
    }
}

pub const SourceBytes = struct {
    slice: []const u8,
    code_page: CodePage,
};

/// Valid escapes:
///  "" -> "
///  \a, \A => 0x08 (not 0x07 like in C)
///  \n => 0x0A
///  \r => 0x0D
///  \t, \T => 0x09
///  \\ => \
///  \nnn => byte with numeric value given by nnn interpreted as octal
///          (wraps on overflow, number of digits can be 1-3 for ASCII strings
///          and 1-7 for wide strings)
///  \xhh => byte with numeric value given by hh interpreted as hex
///          (number of digits can be 0-2 for ASCII strings and 0-4 for
///          wide strings)
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
///
/// This function expects the leading L of wide strings to be omitted from the `bytes.slice`,
/// i.e. `bytes.slice` should always start and end with "
pub fn parseQuotedString(
    comptime literal_type: enum { ascii, wide },
    allocator: std.mem.Allocator,
    bytes: SourceBytes,
    start_column: usize,
) !(switch (literal_type) {
    .ascii => []u8,
    .wide => [:0]u16,
}) {
    const T = if (literal_type == .ascii) u8 else u16;
    std.debug.assert(bytes.slice.len >= 2); // must at least have 2 double quote chars

    var buf = try std.ArrayList(T).initCapacity(allocator, bytes.slice.len);
    errdefer buf.deinit();

    var source = bytes.slice[1..(bytes.slice.len - 1)]; // remove start and end "

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
    var string_escape_n: T = 0;
    var string_escape_i: switch (literal_type) {
        .ascii => std.math.IntFittingRange(0, 3),
        .wide => std.math.IntFittingRange(0, 7),
    } = 0;
    var index: usize = 0;
    while (bytes.code_page.codepointAt(index, source)) |codepoint| : (index += codepoint.byte_len) {
        const c = codepoint.value;
        var backtrack = false;
        defer {
            if (backtrack) {
                index -= codepoint.byte_len;
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
                else => switch (literal_type) {
                    .ascii => {
                        if (windows1252.bestFitFromCodepoint(c)) |best_fit| {
                            try buf.append(best_fit);
                        } else if (c < 0x10000 or c == code_pages.Codepoint.invalid) {
                            try buf.append('?');
                        } else {
                            try buf.appendSlice("??");
                        }
                    },
                    .wide => {
                        if (c == code_pages.Codepoint.invalid) {
                            try buf.append(std.mem.nativeToLittle(u16, '�'));
                        } else if (c < 0x10000) {
                            const short = @intCast(u16, c);
                            try buf.append(std.mem.nativeToLittle(u16, short));
                        } else {
                            const high = @intCast(u16, (c - 0x10000) >> 10) + 0xD800;
                            try buf.append(std.mem.nativeToLittle(u16, high));
                            const low = @intCast(u16, c & 0x3FF) + 0xDC00;
                            try buf.append(std.mem.nativeToLittle(u16, low));
                        }
                    },
                },
            },
            .quote => switch (c) {
                '"' => {
                    // "" => "
                    try buf.append('"');
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
                    string_escape_n = std.fmt.charToDigit(@intCast(u8, c), 8) catch unreachable;
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
                    string_escape_n +%= std.fmt.charToDigit(@intCast(u8, c), 8) catch unreachable;
                    string_escape_i += 1;
                    if ((literal_type == .ascii and string_escape_i == 3) or (literal_type == .wide and string_escape_i == 7)) {
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
                    string_escape_n += std.fmt.charToDigit(@intCast(u8, c), 16) catch unreachable;
                    string_escape_i += 1;
                    if ((literal_type == .ascii and string_escape_i == 2) or (literal_type == .wide and string_escape_i == 4)) {
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

    if (literal_type == .wide) {
        const len = buf.items.len;
        try buf.append(0);
        return buf.toOwnedSlice()[0..len :0];
    } else {
        return buf.toOwnedSlice();
    }
}

pub fn parseQuotedAsciiString(allocator: std.mem.Allocator, bytes: SourceBytes, start_column: usize) ![]u8 {
    return parseQuotedString(.ascii, allocator, bytes, start_column);
}

pub fn parseQuotedWideString(allocator: std.mem.Allocator, bytes: SourceBytes, start_column: usize) ![:0]u16 {
    std.debug.assert(bytes.slice.len >= 3); // L""
    return parseQuotedString(.wide, allocator, .{ .slice = bytes.slice[1..], .code_page = bytes.code_page }, start_column);
}

test "parse quoted ascii string" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    try std.testing.expectEqualSlices(u8, "hello", try parseQuotedAsciiString(arena, .{
        .slice = 
        \\"hello"
        ,
        .code_page = .windows1252,
    }, 0));
    // hex with 0 digits
    try std.testing.expectEqualSlices(u8, "\x00", try parseQuotedAsciiString(arena, .{
        .slice = 
        \\"\x"
        ,
        .code_page = .windows1252,
    }, 0));
    // hex max of 2 digits
    try std.testing.expectEqualSlices(u8, "\xFFf", try parseQuotedAsciiString(arena, .{
        .slice = 
        \\"\XfFf"
        ,
        .code_page = .windows1252,
    }, 0));
    // octal with invalid octal digit
    try std.testing.expectEqualSlices(u8, "\x019", try parseQuotedAsciiString(arena, .{
        .slice = 
        \\"\19"
        ,
        .code_page = .windows1252,
    }, 0));
    // escaped quotes
    try std.testing.expectEqualSlices(u8, " \" ", try parseQuotedAsciiString(arena, .{
        .slice = 
        \\" "" "
        ,
        .code_page = .windows1252,
    }, 0));
    // backslash right before escaped quotes
    try std.testing.expectEqualSlices(u8, "\"", try parseQuotedAsciiString(arena, .{
        .slice = 
        \\"\"""
        ,
        .code_page = .windows1252,
    }, 0));
    // octal overflow
    try std.testing.expectEqualSlices(u8, "\x01", try parseQuotedAsciiString(arena, .{
        .slice = 
        \\"\401"
        ,
        .code_page = .windows1252,
    }, 0));
    // escapes
    try std.testing.expectEqualSlices(u8, "\x08\n\r\t\\", try parseQuotedAsciiString(arena, .{
        .slice = 
        \\"\a\n\r\t\\"
        ,
        .code_page = .windows1252,
    }, 0));
    // uppercase escapes
    try std.testing.expectEqualSlices(u8, "\x08\\N\\R\t\\", try parseQuotedAsciiString(arena, .{
        .slice = 
        \\"\A\N\R\T\\"
        ,
        .code_page = .windows1252,
    }, 0));
    // backslash on its own
    try std.testing.expectEqualSlices(u8, "\\", try parseQuotedAsciiString(arena, .{
        .slice = 
        \\"\"
        ,
        .code_page = .windows1252,
    }, 0));
    // unrecognized escapes
    try std.testing.expectEqualSlices(u8, "\\b", try parseQuotedAsciiString(arena, .{
        .slice = 
        \\"\b"
        ,
        .code_page = .windows1252,
    }, 0));
    // escaped carriage returns
    try std.testing.expectEqualSlices(u8, "\\", try parseQuotedAsciiString(
        arena,
        .{ .slice = "\"\\\r\r\r\r\r\"", .code_page = .windows1252 },
        0,
    ));
    // escaped newlines
    try std.testing.expectEqualSlices(u8, "", try parseQuotedAsciiString(
        arena,
        .{ .slice = "\"\\\n\n\n\n\n\"", .code_page = .windows1252 },
        0,
    ));
    // escaped CRLF pairs
    try std.testing.expectEqualSlices(u8, "", try parseQuotedAsciiString(
        arena,
        .{ .slice = "\"\\\r\n\r\n\r\n\r\n\r\n\"", .code_page = .windows1252 },
        0,
    ));
    // escaped newlines with other whitespace
    try std.testing.expectEqualSlices(u8, "", try parseQuotedAsciiString(
        arena,
        .{ .slice = "\"\\\n    \t\r\n \r\t\n  \t\"", .code_page = .windows1252 },
        0,
    ));
    // literal tab characters get converted to spaces (dependent on source file columns)
    try std.testing.expectEqualSlices(u8, "       ", try parseQuotedAsciiString(
        arena,
        .{ .slice = "\"\t\"", .code_page = .windows1252 },
        0,
    ));
    try std.testing.expectEqualSlices(u8, "abc    ", try parseQuotedAsciiString(
        arena,
        .{ .slice = "\"abc\t\"", .code_page = .windows1252 },
        0,
    ));
    try std.testing.expectEqualSlices(u8, "abcdefg        ", try parseQuotedAsciiString(
        arena,
        .{ .slice = "\"abcdefg\t\"", .code_page = .windows1252 },
        0,
    ));
    try std.testing.expectEqualSlices(u8, "\\      ", try parseQuotedAsciiString(
        arena,
        .{ .slice = "\"\\\t\"", .code_page = .windows1252 },
        0,
    ));
    // literal CR's get dropped
    try std.testing.expectEqualSlices(u8, "", try parseQuotedAsciiString(
        arena,
        .{ .slice = "\"\r\r\r\r\r\"", .code_page = .windows1252 },
        0,
    ));
    // contiguous newlines and whitespace get collapsed to <space><newline>
    try std.testing.expectEqualSlices(u8, " \n", try parseQuotedAsciiString(
        arena,
        .{ .slice = "\"\n\r\r  \r\n \t  \"", .code_page = .windows1252 },
        0,
    ));
}

test "parse quoted ascii string with utf8 code page" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    try std.testing.expectEqualSlices(u8, "", try parseQuotedAsciiString(
        arena,
        .{ .slice = "\"\"", .code_page = .utf8 },
        0,
    ));
    // Codepoints that don't have a Windows-1252 representation get converted to ?
    try std.testing.expectEqualSlices(u8, "?????????", try parseQuotedAsciiString(
        arena,
        .{ .slice = "\"кириллица\"", .code_page = .utf8 },
        0,
    ));
    // Codepoints that have a best fit mapping get converted accordingly,
    // these are box drawing codepoints
    try std.testing.expectEqualSlices(u8, "\x2b\x2d\x2b", try parseQuotedAsciiString(
        arena,
        .{ .slice = "\"┌─┐\"", .code_page = .utf8 },
        0,
    ));
    // Invalid UTF-8 gets converted to ? depending on well-formedness
    try std.testing.expectEqualSlices(u8, "????", try parseQuotedAsciiString(
        arena,
        .{ .slice = "\"\xf0\xf0\x80\x80\x80\"", .code_page = .utf8 },
        0,
    ));
    // Codepoints that would require a UTF-16 surrogate pair get converted to ??
    try std.testing.expectEqualSlices(u8, "??", try parseQuotedAsciiString(
        arena,
        .{ .slice = "\"\xF2\xAF\xBA\xB4\"", .code_page = .utf8 },
        0,
    ));
}

test "parse quoted wide string" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    try std.testing.expectEqualSentinel(u16, 0, &[_:0]u16{ 'h', 'e', 'l', 'l', 'o' }, try parseQuotedWideString(arena, .{
        .slice = 
        \\L"hello"
        ,
        .code_page = .windows1252,
    }, 0));
    // hex with 0 digits
    try std.testing.expectEqualSentinel(u16, 0, &[_:0]u16{0x0}, try parseQuotedWideString(arena, .{
        .slice = 
        \\L"\x"
        ,
        .code_page = .windows1252,
    }, 0));
    // hex max of 4 digits
    try std.testing.expectEqualSentinel(u16, 0, &[_:0]u16{ 0xFFFF, 'f' }, try parseQuotedWideString(arena, .{
        .slice = 
        \\L"\XfFfFf"
        ,
        .code_page = .windows1252,
    }, 0));
    // octal max of 7 digits
    try std.testing.expectEqualSentinel(u16, 0, &[_:0]u16{ 0x9493, '3', '3' }, try parseQuotedWideString(arena, .{
        .slice = 
        \\L"\111222333"
        ,
        .code_page = .windows1252,
    }, 0));
    // octal overflow
    try std.testing.expectEqualSentinel(u16, 0, &[_:0]u16{0xFF01}, try parseQuotedWideString(arena, .{
        .slice = 
        \\L"\777401"
        ,
        .code_page = .windows1252,
    }, 0));
    // Windows-1252 conversion
    try std.testing.expectEqualSentinel(u16, 0, std.unicode.utf8ToUtf16LeStringLiteral("ðð€€€"), try parseQuotedWideString(
        arena,
        .{ .slice = "L\"\xf0\xf0\x80\x80\x80\"", .code_page = .windows1252 },
        0,
    ));
}

test "parse quoted wide string with utf8 code page" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    try std.testing.expectEqualSentinel(u16, 0, &[_:0]u16{}, try parseQuotedWideString(
        arena,
        .{ .slice = "L\"\"", .code_page = .utf8 },
        0,
    ));
    try std.testing.expectEqualSentinel(u16, 0, std.unicode.utf8ToUtf16LeStringLiteral("кириллица"), try parseQuotedWideString(
        arena,
        .{ .slice = "L\"кириллица\"", .code_page = .utf8 },
        0,
    ));
    // Invalid UTF-8 gets converted to � depending on well-formedness
    try std.testing.expectEqualSentinel(u16, 0, std.unicode.utf8ToUtf16LeStringLiteral("����"), try parseQuotedWideString(
        arena,
        .{ .slice = "L\"\xf0\xf0\x80\x80\x80\"", .code_page = .utf8 },
        0,
    ));
}

pub fn columnsUntilTabStop(column: usize, tab_columns: usize) usize {
    // 0 => 8, 1 => 7, 2 => 6, 3 => 5, 4 => 4
    // 5 => 3, 6 => 2, 7 => 1, 8 => 8
    return tab_columns - (column % tab_columns);
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
pub fn parseNumberLiteral(bytes: SourceBytes) Number {
    std.debug.assert(bytes.slice.len > 0);
    var result = Number{ .value = 0, .is_long = false };
    var radix: u8 = 10;
    var buf = bytes.slice;

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

    var i: usize = 0;
    while (bytes.code_page.codepointAt(i, buf)) |codepoint| : (i += codepoint.byte_len) {
        const c = codepoint.value;
        if (c == 'L' or c == 'l') {
            result.is_long = true;
            break;
        }
        const digit = switch (c) {
            // I have no idea why this is the case, but the Windows RC compiler
            // treats ¹, ², and ³ characters as valid digits when the radix is 10
            '¹', '²', '³' => if (radix != 10) break else c - 0x30,
            // On invalid digit for the radix, just stop parsing but don't fail
            0x00...0x7F => std.fmt.charToDigit(@intCast(u8, c), radix) catch break,
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
    try std.testing.expectEqual(Number{ .value = 0, .is_long = false }, parseNumberLiteral(.{ .slice = "0", .code_page = .windows1252 }));
    try std.testing.expectEqual(Number{ .value = 1, .is_long = false }, parseNumberLiteral(.{ .slice = "1", .code_page = .windows1252 }));
    try std.testing.expectEqual(Number{ .value = 1, .is_long = true }, parseNumberLiteral(.{ .slice = "1L", .code_page = .windows1252 }));
    try std.testing.expectEqual(Number{ .value = 1, .is_long = true }, parseNumberLiteral(.{ .slice = "1l", .code_page = .windows1252 }));
    try std.testing.expectEqual(Number{ .value = 1, .is_long = false }, parseNumberLiteral(.{ .slice = "1garbageL", .code_page = .windows1252 }));
    try std.testing.expectEqual(Number{ .value = 4294967295, .is_long = false }, parseNumberLiteral(.{ .slice = "4294967295", .code_page = .windows1252 }));
    try std.testing.expectEqual(Number{ .value = 0, .is_long = false }, parseNumberLiteral(.{ .slice = "4294967296", .code_page = .windows1252 }));
    try std.testing.expectEqual(Number{ .value = 1, .is_long = true }, parseNumberLiteral(.{ .slice = "4294967297L", .code_page = .windows1252 }));

    // can handle any length of number, wraps on overflow appropriately
    const big_overflow = parseNumberLiteral(.{ .slice = "1000000000000000000000000000000000000000000000000000000000000000000000000000000090000000001", .code_page = .windows1252 });
    try std.testing.expectEqual(Number{ .value = 4100654081, .is_long = false }, big_overflow);
    try std.testing.expectEqual(@as(u16, 1025), big_overflow.asWord());

    try std.testing.expectEqual(Number{ .value = 0x20, .is_long = false }, parseNumberLiteral(.{ .slice = "0x20", .code_page = .windows1252 }));
    try std.testing.expectEqual(Number{ .value = 0x2A, .is_long = true }, parseNumberLiteral(.{ .slice = "0x2AL", .code_page = .windows1252 }));
    try std.testing.expectEqual(Number{ .value = 0x2A, .is_long = true }, parseNumberLiteral(.{ .slice = "0x2aL", .code_page = .windows1252 }));
    try std.testing.expectEqual(Number{ .value = 0x2A, .is_long = true }, parseNumberLiteral(.{ .slice = "0x2aL", .code_page = .windows1252 }));

    try std.testing.expectEqual(Number{ .value = 0o20, .is_long = false }, parseNumberLiteral(.{ .slice = "0o20", .code_page = .windows1252 }));
    try std.testing.expectEqual(Number{ .value = 0o20, .is_long = true }, parseNumberLiteral(.{ .slice = "0o20L", .code_page = .windows1252 }));
    try std.testing.expectEqual(Number{ .value = 0o2, .is_long = false }, parseNumberLiteral(.{ .slice = "0o29", .code_page = .windows1252 }));
    try std.testing.expectEqual(Number{ .value = 0, .is_long = false }, parseNumberLiteral(.{ .slice = "0O29", .code_page = .windows1252 }));

    try std.testing.expectEqual(Number{ .value = 0xFFFFFFFF, .is_long = false }, parseNumberLiteral(.{ .slice = "-1", .code_page = .windows1252 }));
    try std.testing.expectEqual(Number{ .value = 0xFFFFFFFE, .is_long = false }, parseNumberLiteral(.{ .slice = "~1", .code_page = .windows1252 }));
    try std.testing.expectEqual(Number{ .value = 0xFFFFFFFF, .is_long = true }, parseNumberLiteral(.{ .slice = "-4294967297L", .code_page = .windows1252 }));
    try std.testing.expectEqual(Number{ .value = 0xFFFFFFFE, .is_long = true }, parseNumberLiteral(.{ .slice = "~4294967297L", .code_page = .windows1252 }));
    try std.testing.expectEqual(Number{ .value = 0xFFFFFFFD, .is_long = false }, parseNumberLiteral(.{ .slice = "-0X3", .code_page = .windows1252 }));

    // anything after L is ignored
    try std.testing.expectEqual(Number{ .value = 0x2A, .is_long = true }, parseNumberLiteral(.{ .slice = "0x2aL5", .code_page = .windows1252 }));
}

test "superscript characters in number literals" {
    // In Windows-1252, ² is \xb2, ³ is \xb3, ¹ is \xb9
    try std.testing.expectEqual(Number{ .value = 130, .is_long = false }, parseNumberLiteral(.{ .slice = "\xb2", .code_page = .windows1252 }));
    try std.testing.expectEqual(Number{ .value = 131, .is_long = false }, parseNumberLiteral(.{ .slice = "\xb3", .code_page = .windows1252 }));
    try std.testing.expectEqual(Number{ .value = 137, .is_long = false }, parseNumberLiteral(.{ .slice = "\xb9", .code_page = .windows1252 }));

    try std.testing.expectEqual(Number{ .value = 147, .is_long = false }, parseNumberLiteral(.{ .slice = "1\xb9", .code_page = .windows1252 }));
    try std.testing.expectEqual(Number{ .value = 157, .is_long = false }, parseNumberLiteral(.{ .slice = "2\xb9", .code_page = .windows1252 }));
    try std.testing.expectEqual(Number{ .value = 167, .is_long = false }, parseNumberLiteral(.{ .slice = "3\xb9", .code_page = .windows1252 }));
    try std.testing.expectEqual(Number{ .value = 227, .is_long = false }, parseNumberLiteral(.{ .slice = "9\xb9", .code_page = .windows1252 }));

    try std.testing.expectEqual(Number{ .value = 237, .is_long = false }, parseNumberLiteral(.{ .slice = "10\xb9", .code_page = .windows1252 }));
    try std.testing.expectEqual(Number{ .value = 337, .is_long = false }, parseNumberLiteral(.{ .slice = "20\xb9", .code_page = .windows1252 }));

    try std.testing.expectEqual(Number{ .value = 1507, .is_long = false }, parseNumberLiteral(.{ .slice = "\xb9\xb9", .code_page = .windows1252 }));
    try std.testing.expectEqual(Number{ .value = 15131, .is_long = false }, parseNumberLiteral(.{ .slice = "\xb9\xb2\xb3", .code_page = .windows1252 }));

    try std.testing.expectEqual(Number{ .value = 15131, .is_long = false }, parseNumberLiteral(.{ .slice = "¹²³", .code_page = .utf8 }));
}
