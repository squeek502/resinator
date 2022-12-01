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

/// std.ComptimeStringMap, but case-insensitive and therefore only works for ASCII strings
pub fn ComptimeCaseInsensitiveStringMap(comptime V: type, comptime kvs_list: anytype) type {
    const precomputed = comptime blk: {
        @setEvalBranchQuota(2000);
        const KV = struct {
            key: []const u8,
            value: V,
        };
        var sorted_kvs: [kvs_list.len]KV = undefined;
        const lenAsc = (struct {
            fn lenAsc(context: void, a: KV, b: KV) bool {
                _ = context;
                return a.key.len < b.key.len;
            }
        }).lenAsc;
        for (kvs_list) |kv, i| {
            if (V != void) {
                sorted_kvs[i] = .{ .key = kv.@"0", .value = kv.@"1" };
            } else {
                sorted_kvs[i] = .{ .key = kv.@"0", .value = {} };
            }
        }
        std.sort.sort(KV, &sorted_kvs, {}, lenAsc);
        const min_len = sorted_kvs[0].key.len;
        const max_len = sorted_kvs[sorted_kvs.len - 1].key.len;
        var len_indexes: [max_len + 1]usize = undefined;
        var len: usize = 0;
        var i: usize = 0;
        while (len <= max_len) : (len += 1) {
            // find the first keyword len == len
            while (len > sorted_kvs[i].key.len) {
                i += 1;
            }
            len_indexes[len] = i;
        }
        break :blk .{
            .min_len = min_len,
            .max_len = max_len,
            .sorted_kvs = sorted_kvs,
            .len_indexes = len_indexes,
        };
    };

    return struct {
        pub const kvs = precomputed.sorted_kvs;

        pub fn has(str: []const u8) bool {
            return get(str) != null;
        }

        pub fn get(str: []const u8) ?V {
            if (str.len < precomputed.min_len or str.len > precomputed.max_len)
                return null;

            var i = precomputed.len_indexes[str.len];
            while (true) {
                const kv = precomputed.sorted_kvs[i];
                if (kv.key.len != str.len)
                    return null;
                if (std.ascii.eqlIgnoreCase(kv.key, str))
                    return kv.value;
                i += 1;
                if (i >= precomputed.sorted_kvs.len)
                    return null;
            }
        }
    };
}

test "comptime case insensitive string map" {
    const TestEnum = enum {
        A,
        B,
        C,
        D,
        E,
    };

    const map = ComptimeCaseInsensitiveStringMap(TestEnum, .{
        .{ "THESE", .D },
        .{ "hAvE", .A },
        .{ "nothing", .B },
        .{ "incommon", .C },
        .{ "SameLen", .E },
    });

    try std.testing.expectEqual(TestEnum.A, map.get("have").?);
    try std.testing.expectEqual(TestEnum.B, map.get("Nothing").?);
    try std.testing.expect(null == map.get("missing"));
    try std.testing.expectEqual(TestEnum.D, map.get("these").?);
    try std.testing.expectEqual(TestEnum.E, map.get("SAMELEN").?);

    try std.testing.expect(!map.has("missing"));
    try std.testing.expect(map.has("These"));
}

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
    /// https://github.com/ziglang/zig/pull/13723
    pub fn expectEqualBytes(expected: []const u8, actual: []const u8) !void {
        if (std.mem.indexOfDiff(u8, actual, expected)) |diff_index| {
            std.debug.print("byte slices differ. first difference occurs at offset {d} (0x{X})\n", .{ diff_index, diff_index });

            const max_window_size: usize = 256;

            // Print a maximum of max_window_size bytes of each input, starting just before the
            // first difference.
            var window_start: usize = 0;
            if (@max(actual.len, expected.len) > max_window_size) {
                window_start = std.mem.alignBackward(diff_index - @min(diff_index, 16), 16);
            }
            const expected_window = expected[window_start..@min(expected.len, window_start + max_window_size)];
            const expected_truncated = window_start + expected_window.len < expected.len;
            const actual_window = actual[window_start..@min(actual.len, window_start + max_window_size)];
            const actual_truncated = window_start + actual_window.len < actual.len;

            var differ = BytesDiffer{
                .expected = expected_window,
                .actual = actual_window,
                .ttyconf = std.debug.detectTTYConfig(),
            };
            const stderr = std.io.getStdErr();

            std.debug.print("\n============ expected this output: =============  len: {} (0x{X})\n\n", .{ expected.len, expected.len });
            if (window_start > 0) {
                std.debug.print("... truncated, start offset: 0x{X} ...\n", .{window_start});
            }
            differ.write(stderr.writer()) catch {};
            if (expected_truncated) {
                const end_offset = window_start + expected_window.len;
                const num_missing_bytes = expected.len - (window_start + expected_window.len);
                std.debug.print("... truncated, end offset: 0x{X}, remaining bytes: 0x{X} ...\n", .{ end_offset, num_missing_bytes });
            }

            // now print the reverse
            differ.expected = actual_window;
            differ.actual = expected_window;
            std.debug.print("\n============= instead found this: ==============  len: {} (0x{X})\n\n", .{ actual.len, actual.len });
            if (window_start > 0) {
                std.debug.print("... truncated, start offset: 0x{X} ...\n", .{window_start});
            }
            differ.write(stderr.writer()) catch {};
            if (actual_truncated) {
                const end_offset = window_start + expected_window.len;
                const num_missing_bytes = actual.len - (window_start + actual_window.len);
                std.debug.print("... truncated, end offset: 0x{X}, remaining bytes: 0x{X} ...\n", .{ end_offset, num_missing_bytes });
            }
            std.debug.print("\n================================================\n\n", .{});

            return error.TestExpectedEqual;
        }
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

// test {
//     const a = ([_]u8{'a'} ** 1017) ++ ([_]u8{'b'} ** 128) ++ ([_]u8{'c'} ** 1024);
//     const b = ([_]u8{'a'} ** 1024) ++ ([_]u8{'c'} ** 170);
//     try testing.expectEqualBytes(&a, &b);
// }

// test {
//     const a = ([_]u8{'a'} ** 1024);
//     const b = ([_]u8{'b'} ** 14 ++ [_]u8{'a'} ** 128);
//     try testing.expectEqualBytes(&b, &a);
// }

// test {
//     const a = ([_]u8{'a'} ** 1024);
//     const b = ([_]u8{'a'} ** 128);
//     try testing.expectEqualBytes(&a, &b);
// }
