const std = @import("std");
const resinator = @import("resinator");
const Allocator = std.mem.Allocator;

pub fn expectSameResOutput(allocator: Allocator, source: []const u8, options: GetResultOptions) !void {
    const input_filepath = "test.rc";
    try options.cwd.writeFile(input_filepath, source);

    var resinator_result = try getResinatorResultFromFile(allocator, input_filepath, options);
    defer resinator_result.deinit(allocator);

    var win32_result = try getWin32ResultFromFile(allocator, input_filepath, options);
    defer win32_result.deinit(allocator);

    try compare(&win32_result, &resinator_result);
}

pub fn compare(win32_result: *Win32Result, resinator_result: *ResinatorResult) !void {
    // Both erroring is fine
    if (win32_result.res == null and resinator_result.res == null) {
        return;
    }

    const source = resinator_result.processed_rc.?;

    if (win32_result.res == null and resinator_result.res != null) {
        if (resinator_result.diagnostics.containsAny(&.{
            .rc_would_error_on_bitmap_version,
            .rc_would_error_on_icon_dir,
            .rc_could_miscompile_control_params,
        })) {
            return;
        }
        std.debug.print("stdout:\n{s}\nstderr:\n{s}\n", .{ win32_result.exec.stdout, win32_result.exec.stderr });
        return error.ExpectedErrorButDidntGetOne;
    }
    if (win32_result.res != null and resinator_result.res == null) {
        if (resinator_result.diagnostics.containsAny(&.{
            .illegal_byte_order_mark,
            .illegal_private_use_character,
            .illegal_byte_outside_string_literals,
            .illegal_byte,
            .close_paren_expression,
            .found_c_style_escaped_quote,
            .unary_plus_expression,
            .invalid_digit_character_in_number_literal,
            .invalid_digit_character_in_ordinal,
        })) {
            return;
        }
        for (resinator_result.diagnostics.errors.items) |details| {
            if (details.err == .icon_read_error) {
                switch (details.extra.icon_read_error.err) {
                    .ImpossibleDataSize,
                    .InvalidHeader,
                    .InvalidImageType,
                    .UnexpectedEOF,
                    => return,
                    else => {},
                }
            }
        }
        resinator_result.diagnostics.renderToStdErrDetectTTY(std.fs.cwd(), source, null);
        return error.DidNotExpectErrorButGotOne;
    }

    std.testing.expectEqualSlices(u8, win32_result.res.?, resinator_result.res.?) catch |err| {
        if (resinator_result.diagnostics.containsAny(&.{
            .rc_would_miscompile_version_value_padding,
            .rc_would_miscompile_version_value_byte_count,
            .rc_would_miscompile_control_padding,
            .rc_would_miscompile_control_class_ordinal,
            .rc_would_miscompile_bmp_palette_padding,
            .rc_would_miscompile_codepoint_byte_swap,
            .rc_would_miscompile_codepoint_skip,
            .rc_could_miscompile_control_params,
        })) {
            std.debug.print("intentional difference, ignoring\n", .{});
            return;
        }
        resinator_result.diagnostics.renderToStdErrDetectTTY(std.fs.cwd(), source, null);
        return err;
    };
}

pub const ResinatorResult = struct {
    res: ?[]const u8 = null,
    diagnostics: resinator.errors.Diagnostics,
    rc_data: []u8,
    /// This is a slice of rc_data
    /// so it doesn't need to be freed separately
    processed_rc: ?[]const u8 = null,

    pub fn deinit(self: *ResinatorResult, allocator: Allocator) void {
        self.diagnostics.deinit();
        if (self.res) |res| {
            allocator.free(res);
        }
        allocator.free(self.rc_data);
    }
};

fn inputContainsKnownPreprocessorDifference(data: []const u8) bool {
    // Look for \r without any newlines before/after
    for (data, 0..) |c, i| {
        if (c == '\r') {
            const newline_before = i > 0 and data[i - 1] == '\n';
            const newline_after = i < data.len - 1 and data[i + 1] == '\n';
            if (!newline_before and !newline_after) return true;
        }
    }
    return false;
}

pub const GetResultOptions = struct {
    cwd: std.fs.Dir,
    cwd_path: []const u8,
    default_code_page: enum { windows1252, utf8 } = .windows1252,
    /// Only used in the Win32 version
    output_path: ?[]const u8 = null,
};

pub fn getResinatorResult(allocator: Allocator, source: []const u8, options: GetResultOptions) !ResinatorResult {
    // TODO: Bypass the intermediate file
    try options.cwd.writeFile("test.rc", source);
    return getResinatorResultFromFile(allocator, "test.rc", options);
}

pub fn getResinatorResultFromFile(allocator: Allocator, input_filepath: []const u8, options: GetResultOptions) !ResinatorResult {
    var result = ResinatorResult{
        .diagnostics = resinator.errors.Diagnostics.init(allocator),
        .rc_data = try options.cwd.readFileAlloc(allocator, input_filepath, std.math.maxInt(usize)),
    };
    errdefer result.deinit(allocator);

    const data = result.rc_data;

    var mapping_results = try resinator.source_mapping.parseAndRemoveLineCommands(
        allocator,
        data,
        data,
        .{ .initial_filename = input_filepath },
    );
    defer mapping_results.mappings.deinit(allocator);

    const final_input = try resinator.comments.removeComments(mapping_results.result, mapping_results.result, &mapping_results.mappings);
    result.processed_rc = final_input;

    // TODO: Somehow make this re-usable between calls
    var output_buf = std.ArrayList(u8).init(allocator);
    defer output_buf.deinit();

    const compile_options = resinator.compile.CompileOptions{
        .cwd = options.cwd,
        .diagnostics = &result.diagnostics,
        .source_mappings = &mapping_results.mappings,
        .default_code_page = switch (options.default_code_page) {
            .windows1252 => .windows1252,
            .utf8 => .utf8,
        },
        // TODO: Make this configurable
        .ignore_include_env_var = true,
    };

    var did_error = false;
    resinator.compile.compile(allocator, final_input, output_buf.writer(), compile_options) catch |err| switch (err) {
        error.ParseError, error.CompileError => {
            did_error = true;
        },
        else => |e| return e,
    };

    if (!did_error) {
        result.res = try output_buf.toOwnedSlice();
    }
    return result;
}

pub const Win32Result = struct {
    res: ?[]const u8 = null,
    exec: std.ChildProcess.RunResult,

    pub fn deinit(self: *Win32Result, allocator: Allocator) void {
        if (self.res) |res| {
            allocator.free(res);
        }
        allocator.free(self.exec.stdout);
        allocator.free(self.exec.stderr);
    }
};

pub fn getWin32Result(allocator: Allocator, source: []const u8, options: GetResultOptions) !Win32Result {
    try options.cwd.writeFile("test.rc", source);
    return getWin32ResultFromFile(allocator, "test.rc", options);
}

pub fn getWin32ResultFromFile(allocator: Allocator, input_path: []const u8, options: GetResultOptions) !Win32Result {
    const output_path = options.output_path orelse "test_win32.res";
    const exec_result = try std.ChildProcess.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            // Note: This relies on `rc.exe` being in the PATH
            "rc.exe",
            switch (options.default_code_page) {
                .windows1252 => "/c1252",
                .utf8 => "/c65001",
            },
            // TODO: Make this configurable
            "/x", // ignore INCLUDE env var
            "/fo",
            output_path,
            input_path,
        },
        .cwd = options.cwd_path,
    });
    errdefer allocator.free(exec_result.stdout);
    errdefer allocator.free(exec_result.stderr);

    var result = Win32Result{ .exec = exec_result };
    if (exec_result.term == .Exited and exec_result.term.Exited == 0) {
        result.res = options.cwd.readFileAlloc(allocator, output_path, std.math.maxInt(usize)) catch |err| blk: {
            std.debug.print("expected file at {s} but got: {}", .{ output_path, err });
            break :blk null;
        };
    }
    return result;
}

pub fn randomNumberLiteral(allocator: Allocator, rand: std.rand.Random, comptime include_superscripts: bool) ![]const u8 {
    var buf = std.ArrayList(u8).init(allocator);
    errdefer buf.deinit();

    const Prefix = enum { none, minus, complement };
    const prefix = rand.enumValue(Prefix);

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
    const index = rand.uintLessThanBiased(u8, dicts.alpha.len);
    return dicts.alpha[index];
}

pub fn randomAlphanumeric(rand: std.rand.Random) u8 {
    const index = rand.uintLessThanBiased(u8, dicts.alphanumeric.len);
    return dicts.alphanumeric[index];
}

pub fn randomNumeric(rand: std.rand.Random) u8 {
    return rand.uintLessThanBiased(u8, 10) + '0';
}

/// Includes Windows-1252 encoded ¹ ² ³
pub fn randomNumericRC(rand: std.rand.Random) u8 {
    const dict = dicts.digits ++ [_]u8{ '\xb2', '\xb3', '\xb9' };
    const index = rand.uintLessThanBiased(u8, dict.len);
    return dict[index];
}

/// Includes Windows-1252 encoded ¹ ² ³
pub fn randomAlphanumericRC(rand: std.rand.Random) u8 {
    const dict = dicts.alphanumeric ++ [_]u8{ '\xb2', '\xb3', '\xb9' };
    const index = rand.uintLessThanBiased(u8, dict.len);
    return dict[index];
}

pub fn randomOperator(rand: std.rand.Random) u8 {
    const dict = [_]u8{ '-', '+', '|', '&' };
    const index = rand.uintLessThanBiased(u8, dict.len);
    return dict[index];
}

pub fn appendRandomStringLiteralSequence(rand: std.rand.Random, buf: *std.ArrayList(u8), is_last: bool, comptime string_type: resinator.literals.StringType) !void {
    const SequenceType = enum { byte, non_ascii_codepoint, octal, hex };
    const sequence_type = rand.enumValue(SequenceType);
    switch (sequence_type) {
        .byte => {
            while (true) {
                const byte = rand.int(u8);
                switch (byte) {
                    // these are currently invalid within string literals
                    0x00, 0x1A, 0x7F => continue,
                    // \r is a mess, so just change it to a space
                    // (clang's preprocessor converts \r to \n, rc skips them entirely)
                    '\r' => continue,
                    // \n within string literals are similarly fraught but they mostly expose
                    // bugs within the Windows RC compiler (where "\n\x01" causes a compile error,
                    // but "<anything besides newlines>\x01" doesn't).
                    // For sanity's sake, just don't put newlines in for now.
                    '\n' => continue,
                    // backslash at the very end of the string leads to \" which is
                    // currently disallowed, so avoid that.
                    '\\' => if (is_last) continue,
                    // Need to escape double quotes as "", but don't want to create a \"" sequence.
                    '"' => if (buf.items.len > 0 and buf.items[buf.items.len - 1] == '\\') continue,
                    else => {},
                }
                try buf.append(byte);
                // Escape double quotes by appending a second double quote
                if (byte == '"') try buf.append(byte);
                break;
            }
        },
        .non_ascii_codepoint => {
            while (true) {
                const codepoint = rand.intRangeAtMost(u21, 0x80, 0x10FFFF);
                if (!std.unicode.utf8ValidCodepoint(codepoint)) continue;
                switch (codepoint) {
                    // disallowed private use character
                    '\u{E000}' => continue,
                    // disallowed BOM
                    '\u{FEFF}' => continue,
                    else => {},
                }
                const codepoint_sequence_length = std.unicode.utf8CodepointSequenceLength(codepoint) catch unreachable;
                const start_index = buf.items.len;
                try buf.resize(buf.items.len + codepoint_sequence_length);
                _ = std.unicode.utf8Encode(codepoint, buf.items[start_index..]) catch unreachable;
                break;
            }
        },
        .octal => {
            const max_val = switch (string_type) {
                .ascii => 0o777,
                .wide => 0o7777777,
            };
            const max_digits = switch (string_type) {
                .ascii => 3,
                .wide => 7,
            };
            const val = rand.uintAtMost(u21, max_val);
            const width = rand.intRangeAtMost(u21, 1, max_digits);
            try buf.ensureUnusedCapacity(max_digits + 1);
            const unused_slice = buf.unusedCapacitySlice();
            const written = std.fmt.bufPrint(unused_slice, "\\{o:0>[1]}", .{ val, width }) catch unreachable;
            buf.items.len += written.len;
        },
        .hex => {
            const max_val = switch (string_type) {
                .ascii => 0xFF,
                .wide => 0xFFFF,
            };
            const max_digits = switch (string_type) {
                .ascii => 2,
                .wide => 4,
            };
            const val = rand.uintAtMost(u16, max_val);
            const width = rand.intRangeAtMost(u16, 1, max_digits);
            try buf.ensureUnusedCapacity(max_digits + 2);
            const unused_slice = buf.unusedCapacitySlice();
            const written = std.fmt.bufPrint(unused_slice, "\\x{x:0>[1]}", .{ val, width }) catch unreachable;
            buf.items.len += written.len;
        },
    }
}

pub fn randomStringLiteral(
    comptime string_type: resinator.literals.StringType,
    allocator: Allocator,
    rand: std.rand.Random,
    max_num_sequences: u16,
) ![]const u8 {
    const num_sequences = rand.uintAtMostBiased(u16, max_num_sequences);
    return randomStringLiteralExact(string_type, allocator, rand, num_sequences);
}

pub fn randomStringLiteralExact(
    comptime string_type: resinator.literals.StringType,
    allocator: Allocator,
    rand: std.rand.Random,
    num_sequences: u16,
) ![]const u8 {
    var buf = try std.ArrayList(u8).initCapacity(allocator, 2 + num_sequences * 4);
    errdefer buf.deinit();

    if (string_type == .wide) try buf.append('L');
    try buf.append('"');
    var i: usize = 0;
    while (i < num_sequences) : (i += 1) {
        try appendRandomStringLiteralSequence(rand, &buf, i == num_sequences - 1, string_type);
    }
    try buf.append('"');

    return try buf.toOwnedSlice();
}

/// Alphanumeric ASCII + any bytes >= 128
pub fn randomAlphanumExtendedBytes(allocator: Allocator, rand: std.rand.Random) ![]const u8 {
    const extended = extended: {
        var buf: [128]u8 = undefined;
        for (&buf, 0..) |*c, i| {
            c.* = @as(u8, @intCast(i)) + 128;
        }
        break :extended buf;
    };
    const dict = dicts.alphanumeric ++ extended;
    // at least 1 byte
    const slice_len = rand.uintAtMostBiased(u16, 512) + 1;
    const buf = try allocator.alloc(u8, slice_len);
    errdefer allocator.free(buf);

    for (buf) |*c| {
        const index = rand.uintLessThanBiased(u8, dict.len);
        var byte = dict[index];
        // swap out e/E to avoid 'expected exponent value' errors
        if (byte == 'e' or byte == 'E') byte += 1;
        c.* = byte;
    }

    return buf;
}

pub fn appendNumberStr(buf: *std.ArrayList(u8), num: anytype) !void {
    const num_digits = if (num == 0) 1 else std.math.log10_int(num) + 1;
    try buf.ensureUnusedCapacity(num_digits);
    const unused_slice = buf.unusedCapacitySlice();
    const written = std.fmt.bufPrint(unused_slice, "{}", .{num}) catch unreachable;
    std.debug.assert(written.len == num_digits);
    buf.items.len += num_digits;
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
            const pivot: usize = self.indexes[tail - 1];

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
