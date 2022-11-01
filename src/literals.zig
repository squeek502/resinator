const std = @import("std");

/// rc is maximally liberal in terms of what it accepts as a number literal
/// for data values. As long as it starts with a number, that's good enough.
pub fn isValidNumberDataLiteral(str: []const u8) bool {
    if (str.len == 0) return false;
    switch (str[0]) {
        '0'...'9' => return true,
        else => return false,
    }
}
