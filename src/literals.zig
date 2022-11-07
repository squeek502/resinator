const std = @import("std");

/// rc is maximally liberal in terms of what it accepts as a number literal
/// for data values. As long as it starts with a number or -, that's good enough.
pub fn isValidNumberDataLiteral(str: []const u8) bool {
    if (str.len == 0) return false;
    switch (str[0]) {
        '-', '0'...'9' => return true,
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
