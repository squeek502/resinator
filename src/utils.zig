/// Like std.io.FixedBufferStream but does no bounds checking
pub const UncheckedSliceWriter = struct {
    const Self = @This();

    pos: usize = 0,
    slice: []u8,

    pub fn write(self: *Self, char: u8) void {
        self.slice[self.pos] = char;
        self.pos += 1;
    }

    pub fn writeSlice(self: *Self, slice: []const u8) void {
        for (slice) |c| {
            self.write(c);
        }
    }

    pub fn getWritten(self: Self) []u8 {
        return self.slice[0..self.pos];
    }
};
