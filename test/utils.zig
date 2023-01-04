const std = @import("std");
const resinator = @import("resinator");
const Allocator = std.mem.Allocator;

pub fn expectSameResOutput(allocator: Allocator, source: []const u8, buffer: *std.ArrayList(u8)) !void {
    const cwd_path = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(cwd_path);
    return expectSameResOutputWithDir(allocator, source, buffer, std.fs.cwd(), cwd_path);
}

pub fn expectSameResOutputWithDir(allocator: Allocator, source: []const u8, buffer: *std.ArrayList(u8), cwd: std.fs.Dir, cwd_path: []const u8) !void {
    const expected_res: ?[]const u8 = resinator.compile.getExpectedFromWindowsRCWithDir(allocator, source, cwd, cwd_path) catch |err| switch (err) {
        error.ExitCodeFailure, error.ProcessTerminated => null,
        else => |e| return e,
    };
    defer if (expected_res != null) allocator.free(expected_res.?);

    var diagnostics = resinator.errors.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    buffer.shrinkRetainingCapacity(0);
    resinator.compile.compile(allocator, source, buffer.writer(), cwd, &diagnostics) catch |err| switch (err) {
        error.ParseError, error.CompileError => {
            diagnostics.renderToStdErr(cwd, source, null);
            // Allow certain errors from resinator if RC succeeds
            const first_err = diagnostics.errors.items[0];
            switch (first_err.err) {
                .illegal_byte_order_mark,
                .illegal_private_use_character,
                => return,
                else => {},
            }
            if (expected_res == null) {
                return;
            } else {
                std.debug.print("\nSource:\n{s}\n\n--------------------------------\n\n", .{std.fmt.fmtSliceEscapeLower(source)});
                return error.DidNotExpectErrorButGotOne;
            }
        },
        else => return err,
    };

    if (expected_res == null) {
        diagnostics.renderToStdErr(cwd, source, null);
        if (diagnostics.errors.items.len > 0) {
            const first_err = diagnostics.errors.items[0];
            switch (first_err.err) {
                .rc_would_error_on_bitmap_version => {
                    return;
                },
                else => {},
            }
        }

        std.debug.print("\nSource:\n{s}\n\n--------------------------------\n\n", .{std.fmt.fmtSliceEscapeLower(source)});
        return error.ExpectedErrorButDidntGetOne;
    }

    std.testing.expectEqualSlices(u8, expected_res.?, buffer.items) catch |err| {
        diagnostics.renderToStdErr(cwd, source, null);
        std.debug.print("\nSource:\n{s}\n\n--------------------------------\n\n", .{std.fmt.fmtSliceEscapeLower(source)});
        return err;
    };
}

pub fn randomNumberLiteral(allocator: Allocator, rand: std.rand.Random, comptime include_superscripts: bool) ![]const u8 {
    var buf = std.ArrayList(u8).init(allocator);
    errdefer buf.deinit();

    const Prefix = enum { none, minus, complement };
    var prefix = rand.enumValue(Prefix);

    switch (prefix) {
        .none => {},
        .minus => try buf.append('-'),
        .complement => try buf.append('~'),
    }

    const getRandomNumeric = if (include_superscripts) randomNumericRC else randomNumeric;
    const getRandomAlphanumeric = if (include_superscripts) randomAlphanumericRC else randomAlphanumeric;

    const has_radix = rand.boolean();
    if (has_radix) {
        try buf.append('0');
        var radix_specifier = randomAlphanumeric(rand);
        // The Windows RC preprocessor rejects number literals of the pattern \d+[eE]\d
        // so just switch to x to avoid this cropping up a ton, and to test with a valid
        // radix more.
        if (std.ascii.toLower(radix_specifier) == 'e') radix_specifier += 'x' - 'e';
        try buf.append(radix_specifier);
    } else {
        // needs to start with a digit
        try buf.append(getRandomNumeric(rand));
    }

    // TODO: increase this limit?
    var length = rand.int(u8);
    if (length == 0 and !has_radix and prefix == .none) {
        length = 1;
    }
    const num_numeric_digits = rand.uintLessThanBiased(usize, @as(usize, length) + 1);
    var i: usize = 0;
    while (i < length) : (i += 1) {
        if (i < num_numeric_digits) {
            try buf.append(getRandomNumeric(rand));
        } else {
            const alphanum = getRandomAlphanumeric(rand);
            try buf.append(alphanum);
            // [eE] in number literals are rejected by the RC preprocessor if they
            // are followed by any digit, so append a non-digit here
            if (alphanum == 'e' or alphanum == 'E') {
                try buf.append(randomAlpha(rand));
            }
        }
    }

    return buf.toOwnedSlice();
}

const dicts = struct {
    const digits = [_]u8{ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' };
    const lowercase_alpha = [_]u8{
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
        'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
    };
    const uppercase_alpha = [_]u8{
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
        'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    };
    const alpha = lowercase_alpha ++ uppercase_alpha;
    const alphanumeric = digits ++ lowercase_alpha ++ uppercase_alpha;
};

pub fn randomAlpha(rand: std.rand.Random) u8 {
    var index = rand.uintLessThanBiased(u8, dicts.alpha.len);
    return dicts.alpha[index];
}

pub fn randomAlphanumeric(rand: std.rand.Random) u8 {
    var index = rand.uintLessThanBiased(u8, dicts.alphanumeric.len);
    return dicts.alphanumeric[index];
}

pub fn randomNumeric(rand: std.rand.Random) u8 {
    return rand.uintLessThanBiased(u8, 10) + '0';
}

/// Includes Windows-1252 encoded ¹ ² ³
pub fn randomNumericRC(rand: std.rand.Random) u8 {
    const dict = dicts.digits ++ [_]u8{ '\xb2', '\xb3', '\xb9' };
    var index = rand.uintLessThanBiased(u8, dict.len);
    return dict[index];
}

/// Includes Windows-1252 encoded ¹ ² ³
pub fn randomAlphanumericRC(rand: std.rand.Random) u8 {
    const dict = dicts.alphanumeric ++ [_]u8{ '\xb2', '\xb3', '\xb9' };
    var index = rand.uintLessThanBiased(u8, dict.len);
    return dict[index];
}

pub fn randomOperator(rand: std.rand.Random) u8 {
    const dict = [_]u8{ '-', '+', '|', '&' };
    const index = rand.uintLessThanBiased(u8, dict.len);
    return dict[index];
}

pub fn randomAsciiStringLiteral(allocator: Allocator, rand: std.rand.Random) ![]const u8 {
    // max string literal length is 4097 so this will generate some invalid string literals
    // need at least two for the ""
    var slice_len = rand.uintAtMostBiased(u16, 256) + 2;
    var buf = try allocator.alloc(u8, slice_len);
    errdefer allocator.free(buf);

    buf[0] = '"';
    var i: usize = 1;
    while (i < slice_len - 1) : (i += 1) {
        var byte = rand.int(u8);
        switch (byte) {
            // these are currently invalid within string literals, so swap them out
            0x00, 0x1A, 0x7F => byte += 1,
            // \r is a mess, so just change it to a space
            // (clang's preprocessor converts \r to \n, rc skips them entirely)
            '\r' => byte = ' ',
            // \n within string literals are similarly fraught but they mostly expose
            // bugs within the Windows RC compiler (where "\n\x01" causes a compile error,
            // but "<anything besides newlines>\x01" doesn't).
            // For sanity's sake, just don't put newlines in for now.
            '\n' => byte = ' ',
            '\\' => {
                // backslash at the very end of the string leads to \" which is
                // currently disallowed, so avoid that.
                if (i + 1 == slice_len - 1) {
                    byte += 1;
                }
            },
            '"' => {
                // Double quotes need to be escaped to keep this a single string literal
                // so try to add one before this char if it'll create an escaped quote ("")
                // but not a \"" sequence.
                // Otherwise, just swap it out by incrementing it.
                if (i >= 2 and buf[i - 1] != '"' and buf[i - 2] != '\\') {
                    buf[i - 1] = '"';
                } else {
                    byte += 1;
                }
            },
            else => {},
        }
        buf[i] = byte;
    }
    buf[slice_len - 1] = '"';

    return buf;
}

/// Alphanumeric ASCII + any bytes >= 128
pub fn randomAlphanumExtendedBytes(allocator: Allocator, rand: std.rand.Random) ![]const u8 {
    const extended = extended: {
        var buf: [128]u8 = undefined;
        for (buf) |*c, i| {
            c.* = @intCast(u8, i) + 128;
        }
        break :extended buf;
    };
    const dict = dicts.alphanumeric ++ extended;
    // at least 1 byte
    var slice_len = rand.uintAtMostBiased(u16, 512) + 1;
    var buf = try allocator.alloc(u8, slice_len);
    errdefer allocator.free(buf);

    for (buf) |*c| {
        var index = rand.uintLessThanBiased(u8, dict.len);
        var byte = dict[index];
        // swap out e/E to avoid 'expected exponent value' errors
        if (byte == 'e' or byte == 'E') byte += 1;
        c.* = byte;
    }

    return buf;
}

/// Iterates all K-permutations of the given size `n` where k varies from (0..n),
/// or (0..max_k) if specified via `initMax`.
/// e.g. for AllKPermutationsIterator(3) the returns from `next` will be (in this order):
/// k=0 {  }
/// k=1 { 0 } { 1 } { 2 }
/// k=2 { 0, 1 } { 0, 2 } { 1, 0 } { 1, 2 } { 2, 0 } { 2, 1 }
/// k=3 { 0, 1, 2 } { 0, 2, 1 } { 1, 0, 2 } { 1, 2, 0 } { 2, 0, 1 } { 2, 1, 0 }
pub fn AllKPermutationsIterator(comptime n: usize) type {
    return struct {
        buf: [n]usize,
        iterator: KPermutationsIterator,
        k: usize,
        max_k: usize,

        const Self = @This();

        pub fn init() Self {
            return initMax(n);
        }

        pub fn initMax(max_k: usize) Self {
            var self = Self{
                .buf = undefined,
                .iterator = undefined,
                .k = 0,
                .max_k = max_k,
            };
            self.resetBuf();
            return self;
        }

        fn resetBuf(self: *Self) void {
            var i: usize = 0;
            while (i < n) : (i += 1) {
                self.buf[i] = i;
            }
        }

        fn nextK(self: *Self) void {
            self.resetBuf();
            self.k += 1;
            self.iterator = KPermutationsIterator.init(&self.buf, self.k);
        }

        pub fn next(self: *Self) ?[]usize {
            if (self.k == 0) {
                self.nextK();
                return self.buf[0..0];
            }

            if (self.iterator.next()) |perm| {
                return perm;
            } else {
                if (self.k == self.max_k) {
                    return null;
                }
                self.nextK();
                return self.iterator.next().?;
            }
        }
    };
}

test "AllKPermutationsIterator" {
    const n = 5;
    var iterator = AllKPermutationsIterator(n).init();
    var i: usize = 0;
    while (iterator.next()) |_| {
        i += 1;
    }
    try std.testing.expectEqual(numAllKPermutations(n), i);
}

pub const KPermutationsIterator = struct {
    indexes: []usize,
    k: usize,
    initial: bool = true,

    pub fn init(indexes_in_order: []usize, k: usize) KPermutationsIterator {
        return .{
            .indexes = indexes_in_order,
            .k = k,
        };
    }

    /// Adapted from https://stackoverflow.com/a/51292710
    pub fn next(self: *KPermutationsIterator) ?[]usize {
        if (self.initial) {
            self.initial = false;
            return self.indexes[0..self.k];
        }
        const n = self.indexes.len;
        var tailmax = self.indexes[n - 1];
        var tail: usize = self.k;
        while (tail > 0 and self.indexes[tail - 1] >= tailmax) {
            tail -= 1;
            tailmax = self.indexes[tail];
        }

        if (tail > 0) {
            var swap_in: usize = 0;
            var pivot: usize = self.indexes[tail - 1];

            if (pivot >= self.indexes[n - 1]) {
                swap_in = tail;
                while (swap_in + 1 < self.k and self.indexes[swap_in + 1] > pivot) : (swap_in += 1) {}
            } else {
                swap_in = n - 1;
                while (swap_in > self.k and self.indexes[swap_in - 1] > pivot) : (swap_in -= 1) {}
            }

            // swap the pivots
            self.indexes[tail - 1] = self.indexes[swap_in];
            self.indexes[swap_in] = pivot;

            // flip the tail
            flip(self.indexes, self.k, n);
            flip(self.indexes, tail, n);
        }

        if (tail > 0) {
            return self.indexes[0..self.k];
        } else {
            return null;
        }
    }
};

test "KPermutationsIterator" {
    var buf = [_]usize{ 0, 1, 2, 3, 4 };
    var iterator = KPermutationsIterator.init(&buf, 2);
    var i: usize = 0;
    while (iterator.next()) |_| {
        i += 1;
    }
    try std.testing.expectEqual(numKPermutationsWithoutRepetition(buf.len, 2), i);
}

fn flip(elements: []usize, lo: usize, hi: usize) void {
    var _lo = lo;
    var _hi = hi;
    while (_lo + 1 < _hi) : ({
        _lo += 1;
        _hi -= 1;
    }) {
        swap(elements, _lo, _hi - 1);
    }
}

fn swap(elements: []usize, a: usize, b: usize) void {
    const tmp = elements[a];
    elements[a] = elements[b];
    elements[b] = tmp;
}

pub fn numAllKPermutations(n: usize) usize {
    return numAllKPermutationsMax(n, n);
}

pub fn numAllKPermutationsMax(n: usize, max_k: usize) usize {
    // P(n, 0) = n!/(n-0)! = 1
    // P(n, 1) = n!/(n-1)! = choices
    // P(n, 2) = n!/(n-2)!
    // ...
    // P(n, n) = n!
    var k: usize = 0;
    var num: usize = 0;
    while (k <= max_k) : (k += 1) {
        num += numKPermutationsWithoutRepetition(n, k);
    }
    return num;
}

fn numKPermutationsWithoutRepetition(n: usize, k: usize) usize {
    if (n == k) return factorial(n);
    if (k == 0) return 1;
    // n! / (n - k)! can overflow if n! overflows, this avoids overflow
    // by only calculating n*(n-1)*(n-2)*...*(n-k+1)
    var result = n;
    var i: usize = n - 1;
    while (i > n - k) : (i -= 1) {
        result *= i;
    }
    return result;
}

fn factorial(n: usize) usize {
    var result: usize = 1;
    var i: u32 = 1;
    while (i <= n) : (i += 1) {
        result *= i;
    }
    return result;
}
