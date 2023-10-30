const std = @import("std");
const resinator = @import("resinator");
const Allocator = std.mem.Allocator;

pub fn expectSameResOutput(allocator: Allocator, source: []const u8, options: GetResultOptions) !void {
    const input_filepath = "test.rc";
    try options.cwd.writeFile(input_filepath, source);

    var resinator_result = try getResinatorResultFromFile(allocator, input_filepath, options);
    defer resinator_result.deinit(allocator);

    if (resinator_result.pre != null and resinator_result.pre.?.known_preprocessor_difference) {
        std.debug.print("known preprocessor difference, skipping\n", .{});
        return;
    }

    var win32_result = try getWin32ResultFromFile(allocator, input_filepath, options);
    defer win32_result.deinit(allocator);

    try compare(&win32_result, &resinator_result);
}

pub fn compare(win32_result: *Win32Result, resinator_result: *ResinatorResult) !void {
    // The preprocessor may reject things that the win32 compiler does not.
    // This is not something we can do anything about unless we write our own compliant
    // preprocessor, so instead we just treat it as okay and move on.
    if (resinator_result.didPreproccessorError()) {
        return;
    }
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
    /// Only populated if the preprocessor is not run
    rc_data: ?[]u8 = null,
    /// This is a slice of either pre.preprocessor.stdout or rc_data
    /// so it doesn't need to be freed separately
    processed_rc: ?[]const u8 = null,
    pre: ?PreprocessResult = null,

    pub const PreprocessResult = struct {
        preprocessor: std.ChildProcess.RunResult = undefined,
        known_preprocessor_difference: bool = false,
    };

    pub fn deinit(self: *ResinatorResult, allocator: Allocator) void {
        self.diagnostics.deinit();
        if (self.res) |res| {
            allocator.free(res);
        }
        if (self.pre) |pre| {
            if (!pre.known_preprocessor_difference) {
                allocator.free(pre.preprocessor.stdout);
                allocator.free(pre.preprocessor.stderr);
            }
        }
        if (self.rc_data) |rc_data| {
            allocator.free(rc_data);
        }
    }

    pub fn didPreproccessorError(self: *const ResinatorResult) bool {
        if (self.pre) |pre| {
            return pre.preprocessor.term != .Exited or pre.preprocessor.term.Exited != 0;
        }
        return false;
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
    run_preprocessor: bool,
    /// Only used in the Win32 version
    output_path: ?[]const u8 = null,
};

pub fn getResinatorResult(allocator: Allocator, source: []const u8, options: GetResultOptions) !ResinatorResult {
    // TODO: Bypass the intermediate file if options.run_preprocessor is false
    try options.cwd.writeFile("test.rc", source);
    return getResinatorResultFromFile(allocator, "test.rc", options);
}

pub fn getResinatorResultFromFile(allocator: Allocator, input_filepath: []const u8, options: GetResultOptions) !ResinatorResult {
    var result = ResinatorResult{
        .diagnostics = resinator.errors.Diagnostics.init(allocator),
    };
    errdefer result.deinit(allocator);

    result.pre = try runPreprocessor(allocator, input_filepath, options);
    if (result.pre) |pre| {
        if (pre.preprocessor.term != .Exited or pre.preprocessor.term.Exited != 0) {
            return result;
        }
    }

    // Use the preprocessor result if it exists, otherwise we need to read the file and use its contents
    // directly.
    if (!options.run_preprocessor) {
        result.rc_data = try options.cwd.readFileAlloc(allocator, input_filepath, std.math.maxInt(usize));
    }
    var data = result.rc_data orelse result.pre.?.preprocessor.stdout;

    var mapping_results = try resinator.source_mapping.parseAndRemoveLineCommands(
        allocator,
        data,
        data,
        .{ .initial_filename = input_filepath },
    );
    defer mapping_results.mappings.deinit(allocator);

    var final_input = resinator.comments.removeComments(mapping_results.result, mapping_results.result, &mapping_results.mappings);
    result.processed_rc = final_input;

    // TODO: Somehow make this re-usable between calls
    var output_buf = std.ArrayList(u8).init(allocator);
    defer output_buf.deinit();

    const compile_options = resinator.compile.CompileOptions{
        .cwd = options.cwd,
        .diagnostics = &result.diagnostics,
        .source_mappings = &mapping_results.mappings,
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

pub fn runPreprocessor(allocator: Allocator, input_filepath: []const u8, options: GetResultOptions) !?ResinatorResult.PreprocessResult {
    if (!options.run_preprocessor) return null;

    var result = ResinatorResult.PreprocessResult{};

    var data = try options.cwd.readFileAlloc(allocator, input_filepath, std.math.maxInt(usize));
    defer allocator.free(data);

    if (inputContainsKnownPreprocessorDifference(data)) {
        result.known_preprocessor_difference = true;
        return result;
    }

    var argv = std.ArrayList([]const u8).init(allocator);
    defer argv.deinit();

    try argv.appendSlice(&[_][]const u8{
        "clang",
        "-E", // preprocessor only
        "--comments",
        "-fuse-line-directives", // #line <num> instead of # <num>
        // TODO: could use --trace-includes to give info about what's included from where
        "-xc", // output c
        // TODO: Turn this off, check the warnings, and convert the spaces back to NUL
        "-Werror=null-character", // error on null characters instead of converting them to spaces
        // TODO: could remove -Werror=null-character and instead parse warnings looking for 'warning: null character ignored'
        //       since the only real problem is when clang doesn't preserve null characters
        //"-Werror=invalid-pp-token", // will error on unfinished string literals
        // TODO: could use -Werror instead
        // https://learn.microsoft.com/en-us/windows/win32/menurc/predefined-macros
        "-DRC_INVOKED",
    });
    try argv.append(input_filepath);

    result.preprocessor = try std.ChildProcess.run(.{
        .allocator = allocator,
        .argv = argv.items,
        .max_output_bytes = std.math.maxInt(u32),
        .cwd = options.cwd_path,
    });
    errdefer allocator.free(result.preprocessor.stdout);
    errdefer allocator.free(result.preprocessor.stderr);

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
    var exec_result = try std.ChildProcess.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            // Note: This relies on `rc.exe` being in the PATH
            "rc.exe",
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
        for (&buf, 0..) |*c, i| {
            c.* = @as(u8, @intCast(i)) + 128;
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
