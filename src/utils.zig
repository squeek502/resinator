const std = @import("std");
const builtin = @import("builtin");

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

/// Slurps the first `size` bytes read into `slurped_header`
pub fn HeaderSlurpingReader(comptime size: usize, comptime ReaderType: anytype) type {
    return struct {
        child_reader: ReaderType,
        bytes_read: u64 = 0,
        slurped_header: [size]u8 = [_]u8{0x00} ** size,

        pub const Error = ReaderType.Error;
        pub const Reader = std.io.Reader(*@This(), Error, read);

        pub fn read(self: *@This(), buf: []u8) Error!usize {
            const amt = try self.child_reader.read(buf);
            if (self.bytes_read < size) {
                const bytes_to_add = @min(amt, size - self.bytes_read);
                const end_index = self.bytes_read + bytes_to_add;
                std.mem.copy(u8, self.slurped_header[self.bytes_read..end_index], buf[0..bytes_to_add]);
            }
            self.bytes_read += amt;
            return amt;
        }

        pub fn reader(self: *@This()) Reader {
            return .{ .context = self };
        }
    };
}

pub fn headerSlurpingReader(comptime size: usize, reader: anytype) HeaderSlurpingReader(size, @TypeOf(reader)) {
    return .{ .child_reader = reader };
}

pub const ascii = struct {
    /// Compares ASCII values case-insensitively, non-ASCII values are compared directly
    pub fn eqlIgnoreCaseW(a: []const u16, b: []const u16) bool {
        if (a.len != b.len) return false;
        for (a, b) |a_c, b_c| {
            if (a_c < 128) {
                if (std.ascii.toLower(@intCast(a_c)) != std.ascii.toLower(@intCast(b_c))) return false;
            } else {
                if (a_c != b_c) return false;
            }
        }
        return true;
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
        for (kvs_list, 0..) |kv, i| {
            if (V != void) {
                sorted_kvs[i] = .{ .key = kv.@"0", .value = kv.@"1" };
            } else {
                sorted_kvs[i] = .{ .key = kv.@"0", .value = {} };
            }
        }
        std.mem.sort(KV, &sorted_kvs, {}, lenAsc);
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

/// Sort of like std.io.LimitedReader, but a Writer.
/// Returns an error if writing the requested number of bytes
/// would ever exceed bytes_left, i.e. it does not always
/// write up to the limit and instead will error if the
/// limit would be breached if the entire slice was written.
pub fn LimitedWriter(comptime WriterType: type) type {
    return struct {
        inner_writer: WriterType,
        bytes_left: u64,

        pub const Error = error{NoSpaceLeft} || WriterType.Error;
        pub const Writer = std.io.Writer(*Self, Error, write);

        const Self = @This();

        pub fn write(self: *Self, bytes: []const u8) Error!usize {
            if (bytes.len > self.bytes_left) return error.NoSpaceLeft;
            const amt = try self.inner_writer.write(bytes);
            self.bytes_left -= amt;
            return amt;
        }

        pub fn writer(self: *Self) Writer {
            return .{ .context = self };
        }
    };
}

/// Returns an initialised `LimitedWriter`
/// `bytes_left` is a `u64` to be able to take 64 bit file offsets
pub fn limitedWriter(inner_writer: anytype, bytes_left: u64) LimitedWriter(@TypeOf(inner_writer)) {
    return .{ .inner_writer = inner_writer, .bytes_left = bytes_left };
}

test "limitedWriter basic usage" {
    var buf: [4]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    var limited_stream = limitedWriter(fbs.writer(), 4);
    var writer = limited_stream.writer();

    try std.testing.expectEqual(@as(usize, 3), try writer.write("123"));
    try std.testing.expectEqualSlices(u8, "123", buf[0..3]);
    try std.testing.expectError(error.NoSpaceLeft, writer.write("45"));
    try std.testing.expectEqual(@as(usize, 1), try writer.write("4"));
    try std.testing.expectEqualSlices(u8, "1234", buf[0..4]);
    try std.testing.expectError(error.NoSpaceLeft, writer.write("5"));
}

/// Cross-platform 'std.fs.Dir.openFile' wrapper that will always return IsDir if
/// a directory is attempted to be opened.
/// TODO: Remove once https://github.com/ziglang/zig/issues/5732 is addressed.
pub fn openFileNotDir(cwd: std.fs.Dir, path: []const u8, flags: std.fs.File.OpenFlags) std.fs.File.OpenError!std.fs.File {
    const file = try cwd.openFile(path, flags);
    errdefer file.close();
    // https://github.com/ziglang/zig/issues/5732
    if (builtin.os.tag != .windows) {
        const stat = try file.stat();

        if (stat.kind == .directory)
            return error.IsDir;
    }
    return file;
}

/// Emulates the Windows implementation of `iswdigit`, but only returns true
/// for the non-ASCII digits that `iswdigit` on Windows would return true for.
pub fn isNonAsciiDigit(c: u21) bool {
    return switch (c) {
        '²',
        '³',
        '¹',
        '\u{660}'...'\u{669}',
        '\u{6F0}'...'\u{6F9}',
        '\u{7C0}'...'\u{7C9}',
        '\u{966}'...'\u{96F}',
        '\u{9E6}'...'\u{9EF}',
        '\u{A66}'...'\u{A6F}',
        '\u{AE6}'...'\u{AEF}',
        '\u{B66}'...'\u{B6F}',
        '\u{BE6}'...'\u{BEF}',
        '\u{C66}'...'\u{C6F}',
        '\u{CE6}'...'\u{CEF}',
        '\u{D66}'...'\u{D6F}',
        '\u{E50}'...'\u{E59}',
        '\u{ED0}'...'\u{ED9}',
        '\u{F20}'...'\u{F29}',
        '\u{1040}'...'\u{1049}',
        '\u{1090}'...'\u{1099}',
        '\u{17E0}'...'\u{17E9}',
        '\u{1810}'...'\u{1819}',
        '\u{1946}'...'\u{194F}',
        '\u{19D0}'...'\u{19D9}',
        '\u{1B50}'...'\u{1B59}',
        '\u{1BB0}'...'\u{1BB9}',
        '\u{1C40}'...'\u{1C49}',
        '\u{1C50}'...'\u{1C59}',
        '\u{A620}'...'\u{A629}',
        '\u{A8D0}'...'\u{A8D9}',
        '\u{A900}'...'\u{A909}',
        '\u{AA50}'...'\u{AA59}',
        '\u{FF10}'...'\u{FF19}',
        => true,
        else => false,
    };
}
