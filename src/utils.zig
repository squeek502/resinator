const std = @import("std");
const builtin = @import("builtin");
const windows = std.os.windows;

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

// Similar to std.debug.TTY.Config
pub const Colors = enum {
    no_color,
    escape_codes,
    windows_api,

    pub fn detect() Colors {
        if (std.process.hasEnvVarConstant("NO_COLOR")) {
            return .no_color;
        } else {
            const stderr_file = std.io.getStdErr();
            if (stderr_file.supportsAnsiEscapeCodes()) {
                return .escape_codes;
            } else if (builtin.os.tag == .windows and stderr_file.isTty()) {
                return .windows_api;
            } else {
                return .no_color;
            }
        }
    }

    pub const Color = enum {
        reset,
        red,
        green,
        cyan,
        yellow,
        white,
        dim,
        bold,

        pub fn escapeSequence(self: Color) []const u8 {
            return switch (self) {
                .reset => return "\x1b[0m",
                .red => "\x1b[31;1m",
                .green => "\x1b[32;1m",
                .cyan => "\x1b[36;1m",
                .yellow => "\x1b[93;1m",
                .white => "\x1b[37;1m",
                .dim => "\x1b[2m",
                .bold => "\x1b[1m",
            };
        }

        pub fn characterAttributes(self: Color, reset_attrs: windows.WORD) windows.WORD {
            return switch (self) {
                .reset => reset_attrs,
                .red => windows.FOREGROUND_RED | windows.FOREGROUND_INTENSITY,
                .green => windows.FOREGROUND_GREEN | windows.FOREGROUND_INTENSITY,
                .cyan => windows.FOREGROUND_GREEN | windows.FOREGROUND_BLUE | windows.FOREGROUND_INTENSITY,
                .yellow => windows.FOREGROUND_RED | windows.FOREGROUND_GREEN | windows.FOREGROUND_INTENSITY,
                .white, .bold => windows.FOREGROUND_RED | windows.FOREGROUND_GREEN | windows.FOREGROUND_BLUE | windows.FOREGROUND_INTENSITY,
                .dim => windows.FOREGROUND_INTENSITY,
            };
        }
    };

    pub fn set(self: Colors, out_stream: anytype, color: Color) void {
        nosuspend switch (self) {
            .no_color => return,
            .escape_codes => out_stream.writeAll(color.escapeSequence()) catch return,
            .windows_api => if (builtin.os.tag == .windows) {
                const stderr_file = std.io.getStdErr();
                const S = struct {
                    var attrs: windows.WORD = undefined;
                    var init_attrs = false;
                };
                if (!S.init_attrs) {
                    S.init_attrs = true;
                    var info: windows.CONSOLE_SCREEN_BUFFER_INFO = undefined;
                    _ = windows.kernel32.GetConsoleScreenBufferInfo(stderr_file.handle, &info);
                    S.attrs = info.wAttributes;
                }
                _ = windows.SetConsoleTextAttribute(
                    stderr_file.handle,
                    color.characterAttributes(S.attrs),
                ) catch {};
            } else {
                unreachable;
            },
        };
    }
};

pub const testing = struct {
    /// https://github.com/ziglang/zig/pull/13720
    pub fn expectEqualBytes(expected: []const u8, actual: []const u8) !void {
        std.testing.expectEqualSlices(u8, expected, actual) catch |err| {
            var differ = BytesDiffer{
                .expected = expected,
                .actual = actual,
                .ttyconf = std.debug.detectTTYConfig(),
            };
            const stderr = std.io.getStdErr();

            std.debug.print("\n============ expected this output: =============\n\n", .{});
            differ.write(stderr.writer()) catch {};

            // now print the reverse
            differ.expected = actual;
            differ.actual = expected;
            std.debug.print("\n============= instead found this: ==============\n\n", .{});
            differ.write(stderr.writer()) catch {};
            std.debug.print("\n================================================\n\n", .{});

            return err;
        };
    }

    pub const BytesDiffer = struct {
        expected: []const u8,
        actual: []const u8,
        ttyconf: std.debug.TTY.Config,

        pub fn write(self: BytesDiffer, writer: anytype) !void {
            var expected_iterator = ChunkIterator{ .bytes = self.expected };
            while (expected_iterator.next()) |chunk| {
                // to avoid having to calculate diffs twice per chunk
                var diffs: std.bit_set.IntegerBitSet(16) = .{ .mask = 0 };
                for (chunk) |byte, i| {
                    var absolute_byte_index = (expected_iterator.index - chunk.len) + i;
                    const diff = if (absolute_byte_index < self.actual.len) self.actual[absolute_byte_index] != byte else true;
                    if (diff) diffs.set(i);
                    try self.writeByteDiff(writer, "{X:0>2} ", byte, diff);
                    if (i == 7) try writer.writeByte(' ');
                }
                try writer.writeByte(' ');
                if (chunk.len < 16) {
                    var missing_columns = (16 - chunk.len) * 3;
                    if (chunk.len < 8) missing_columns += 1;
                    try writer.writeByteNTimes(' ', missing_columns);
                }
                for (chunk) |byte, i| {
                    const byte_to_print = if (std.ascii.isPrint(byte)) byte else '.';
                    try self.writeByteDiff(writer, "{c}", byte_to_print, diffs.isSet(i));
                }
                try writer.writeByte('\n');
            }
        }

        fn writeByteDiff(self: BytesDiffer, writer: anytype, comptime fmt: []const u8, byte: u8, diff: bool) !void {
            if (diff) self.ttyconf.setColor(writer, .Red);
            try writer.print(fmt, .{byte});
            if (diff) self.ttyconf.setColor(writer, .Reset);
        }

        const ChunkIterator = struct {
            bytes: []const u8,
            index: usize = 0,

            pub fn next(self: *ChunkIterator) ?[]const u8 {
                if (self.index == self.bytes.len) return null;

                const start_index = self.index;
                const end_index = @min(self.bytes.len, start_index + 16);
                self.index = end_index;
                return self.bytes[start_index..end_index];
            }
        };
    };
};

// test {
//     const a = "helloajs\xFF\xFFnakjnfnjkasfjnks";
//     const b = "hello\x01\x05\x7F\xFF\xFFabcde\xFF\xFF\xFF\xFF\x00\x00fj\x00\x00s";

//     try testing.expectEqualBytes(a, b);
// }

// test {
//     const a = "helloajsfbnakjnfnjkasfjaa";
//     const b = "hellobye";
//     try testing.expectEqualBytes(b, a);
// }
