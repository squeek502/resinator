const std = @import("std");
const resinator = @import("resinator");
const Allocator = std.mem.Allocator;

pub fn expectSameResOutput(allocator: Allocator, source: []const u8, options: GetResultOptions) !void {
    const input_filepath = "test.rc";
    try options.cwd.writeFile(.{ .sub_path = input_filepath, .data = source });

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
            .rc_would_miscompile_codepoint_whitespace,
            .rc_would_miscompile_codepoint_skip,
            .rc_would_miscompile_codepoint_bom,
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
    try options.cwd.writeFile(.{ .sub_path = "test.rc", .data = source });
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

    const default_code_page: resinator.code_pages.SupportedCodePage = switch (options.default_code_page) {
        .windows1252 => .windows1252,
        .utf8 => .utf8,
    };
    const has_disjoint_code_page = resinator.disjoint_code_page.hasDisjointCodePage(mapping_results.result, &mapping_results.mappings, default_code_page);

    const final_input = try resinator.comments.removeComments(mapping_results.result, mapping_results.result, &mapping_results.mappings);
    result.processed_rc = final_input;

    // TODO: Somehow make this re-usable between calls
    var output_buf = std.ArrayList(u8).init(allocator);
    defer output_buf.deinit();

    const compile_options = resinator.compile.CompileOptions{
        .cwd = options.cwd,
        .diagnostics = &result.diagnostics,
        .source_mappings = &mapping_results.mappings,
        .default_code_page = default_code_page,
        .disjoint_code_page = has_disjoint_code_page,
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
    exec: std.process.Child.RunResult,

    pub fn deinit(self: *Win32Result, allocator: Allocator) void {
        if (self.res) |res| {
            allocator.free(res);
        }
        allocator.free(self.exec.stdout);
        allocator.free(self.exec.stderr);
    }
};

pub fn getWin32Result(allocator: Allocator, source: []const u8, options: GetResultOptions) !Win32Result {
    try options.cwd.writeFile(.{ .sub_path = "test.rc", .data = source });
    return getWin32ResultFromFile(allocator, "test.rc", options);
}

pub fn getWin32ResultFromFile(allocator: Allocator, input_path: []const u8, options: GetResultOptions) !Win32Result {
    const output_path = options.output_path orelse "test_win32.res";
    const exec_result = try std.process.Child.run(.{
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

pub fn expectSameCvtResOutput(allocator: Allocator, res_source: []const u8, options: GetCvtResResultOptions) !void {
    var resinator_result = try getResinatorCvtResResult(allocator, res_source, options);
    defer resinator_result.deinit(allocator);

    var win32_result = try getWin32CvtResResult(allocator, res_source, options);
    defer win32_result.deinit(allocator);

    try compareCvtRes(&win32_result, &resinator_result);
}

pub fn compareCvtRes(win32_result: *Win32CvtResResult, resinator_result: *ResinatorCvtResResult) !void {
    // Both erroring is fine
    if (win32_result.obj == null and resinator_result.obj == null) {
        return;
    }

    if (win32_result.obj == null and resinator_result.obj != null) {
        std.debug.print("stdout:\n{s}\nstderr:\n{s}\n", .{ win32_result.exec.stdout, win32_result.exec.stderr });
        return error.ExpectedErrorButDidntGetOne;
    }
    if (win32_result.obj != null and resinator_result.obj == null) {
        if (resinator_result.parse_err) |err| std.debug.print("parse error: {s}\n", .{@errorName(err)});
        if (resinator_result.coff_err) |err| std.debug.print("coff error: {s}\n", .{@errorName(err)});
        return error.DidNotExpectErrorButGotOne;
    }

    std.testing.expectEqualSlices(u8, win32_result.obj.?, resinator_result.obj.?) catch |err| {
        return err;
    };
}

pub const GetCvtResResultOptions = struct {
    cwd: std.fs.Dir,
    cwd_path: []const u8,
    /// Only used in the Win32 version
    output_path: ?[]const u8 = null,
    target: std.coff.MachineType = .X64,
    read_only: bool = false,
    define_external_symbol: ?[]const u8 = null,
    fold_duplicate_data: bool = false,
};

pub const ResinatorCvtResResult = struct {
    obj: ?[]const u8 = null,
    parse_err: ?anyerror = null,
    coff_err: ?anyerror = null,

    pub fn deinit(self: *ResinatorCvtResResult, allocator: Allocator) void {
        if (self.obj) |obj| {
            allocator.free(obj);
        }
    }
};

pub fn getResinatorCvtResResult(allocator: Allocator, res_source: []const u8, options: GetCvtResResultOptions) !ResinatorCvtResResult {
    var fbs = std.io.fixedBufferStream(res_source);
    var resources = resinator.cvtres.parseRes(allocator, fbs.reader(), .{ .max_size = res_source.len }) catch |err| {
        return .{
            .parse_err = err,
        };
    };
    defer resources.deinit();

    var buf = try std.ArrayList(u8).initCapacity(allocator, 256);
    errdefer buf.deinit();

    resinator.cvtres.writeCoff(allocator, buf.writer(), resources.list.items, .{
        .target = options.target,
        .read_only = options.read_only,
        .define_external_symbol = options.define_external_symbol,
        .fold_duplicate_data = options.fold_duplicate_data,
    }, null) catch |err| {
        buf.deinit();
        return .{
            .coff_err = err,
        };
    };

    try std.fs.cwd().writeFile(.{ .sub_path = ".zig-cache/tmp/fuzzy_cvtres.resinator.obj", .data = buf.items });

    return .{
        .obj = try buf.toOwnedSlice(),
    };
}

pub const Win32CvtResResult = struct {
    obj: ?[]const u8 = null,
    exec: std.process.Child.RunResult,

    pub fn deinit(self: *Win32CvtResResult, allocator: Allocator) void {
        if (self.obj) |obj| {
            allocator.free(obj);
        }
        allocator.free(self.exec.stdout);
        allocator.free(self.exec.stderr);
    }
};

pub fn getWin32CvtResResult(allocator: Allocator, res_source: []const u8, options: GetCvtResResultOptions) !Win32CvtResResult {
    try options.cwd.writeFile(.{ .sub_path = "test.res", .data = res_source });
    return getWin32CvtResResultFromFile(allocator, "test.res", options);
}

pub fn getWin32CvtResResultFromFile(allocator: Allocator, input_path: []const u8, options: GetCvtResResultOptions) !Win32CvtResResult {
    var argv = try std.ArrayList([]const u8).initCapacity(allocator, 4);
    defer argv.deinit();

    // Note: This relies on `cvtres.exe` being in the PATH
    argv.appendAssumeCapacity("cvtres.exe");

    const target_name = switch (options.target) {
        .I386 => "X86",
        .ARMNT => "ARM",
        else => @tagName(options.target),
    };
    const machine_arg = try std.fmt.allocPrint(allocator, "/MACHINE:{s}", .{target_name});
    defer allocator.free(machine_arg);
    argv.appendAssumeCapacity(machine_arg);

    const output_path = options.output_path orelse "test_win32.obj";
    const out_arg = try std.fmt.allocPrint(allocator, "/OUT:{s}", .{output_path});
    defer allocator.free(out_arg);
    argv.appendAssumeCapacity(out_arg);

    if (options.read_only) {
        try argv.append("/READONLY");
    }
    if (options.fold_duplicate_data) {
        try argv.append("/FOLDDUPS");
    }
    const define_arg: ?[]const u8 = define_arg: {
        if (options.define_external_symbol) |symbol_name| {
            const define_arg = try std.fmt.allocPrint(allocator, "/DEFINE:{s}", .{symbol_name});
            errdefer allocator.free(define_arg);
            try argv.append(define_arg);
            break :define_arg define_arg;
        }
        break :define_arg null;
    };
    defer if (define_arg) |v| allocator.free(v);

    try argv.append(input_path);

    const exec_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = argv.items,
        .cwd = options.cwd_path,
    });
    errdefer allocator.free(exec_result.stdout);
    errdefer allocator.free(exec_result.stderr);

    var result = Win32CvtResResult{ .exec = exec_result };
    if (exec_result.term == .Exited and exec_result.term.Exited == 0) {
        var obj = options.cwd.readFileAlloc(allocator, output_path, std.math.maxInt(usize)) catch |err| blk: {
            std.debug.print("expected file at {s} but got: {}", .{ output_path, err });
            break :blk null;
        };
        errdefer if (obj) |v| allocator.free(v);

        if (obj) |obj_bytes| {
            var fbs = std.io.fixedBufferStream(obj.?);
            var stripped_buf = try std.ArrayList(u8).initCapacity(allocator, obj_bytes.len);
            errdefer stripped_buf.deinit();

            try stripAndFixupCoff(allocator, fbs.reader(), stripped_buf.writer(), .{});
            const stripped_obj = try stripped_buf.toOwnedSlice();
            allocator.free(obj_bytes);
            obj = stripped_obj;
        }

        result.obj = obj;
    }
    return result;
}

const CoffFixups = struct {
    clear_timestamp: bool = true,
    /// cvtres.exe writes 0 as the string table length, which goes against the spec:
    /// > At the beginning of the COFF string table are 4 bytes that contain the total size (in bytes)
    /// > of the rest of the string table. This size includes the size field itself, so that the value
    /// > in this location would be 4 if no strings were present.
    /// https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#coff-string-table
    correct_string_table_length: bool = true,
    /// cvtres.exe writes a non-zero value to the pointer to relocations field even if
    /// there are no relocations. The spec, however, says:
    /// > This is set to zero for executable images or if there are no relocations.
    /// https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#section-table-section-headers
    zero_pointer_to_relocations: bool = true,
    /// @comp.id is a Microsoft-specific symbol containing information about the compiler used
    /// See https://github.com/dishather/richprint for info
    strip_comp_id: bool = true,
};

// Not a general purpose implementation at all, only intended to work on the output of cvtres.exe
pub fn stripAndFixupCoff(allocator: Allocator, reader: anytype, writer: anytype, fixups: CoffFixups) !void {
    var counting_reader = std.io.countingReader(reader);
    const r = counting_reader.reader();

    var stripped_bytes: u32 = 0;
    var stripped_symbols: u32 = 0;
    // Assume @comp.id exists
    // TODO: Don't make this assumption
    if (fixups.strip_comp_id) {
        stripped_symbols += 1;
    }
    var stripped_sections: u16 = 0;
    var header = try r.readStruct(std.coff.CoffHeader);
    if (fixups.clear_timestamp) {
        header.time_date_stamp = 0;
    }
    const original_number_of_symbols = header.number_of_symbols;
    var sections = std.ArrayList(std.coff.SectionHeader).init(allocator);
    defer sections.deinit();
    for (0..header.number_of_sections) |_| {
        var section_header = try r.readStruct(std.coff.SectionHeader);
        if (std.mem.startsWith(u8, &section_header.name, ".rsrc")) {
            if (fixups.zero_pointer_to_relocations and section_header.number_of_relocations == 0) {
                section_header.pointer_to_relocations = 0;
            }
            try sections.append(section_header);
        } else {
            stripped_bytes += @sizeOf(std.coff.SectionHeader);
            stripped_bytes += section_header.size_of_raw_data;
            stripped_symbols += 2; // 1 for symbol, 1 for aux symbol of section
            stripped_sections += 1;
        }
    }
    header.number_of_symbols -= stripped_symbols;
    header.number_of_sections -= stripped_sections;
    header.pointer_to_symbol_table -= stripped_bytes;
    try writer.writeAll(std.mem.asBytes(&header));

    for (sections.items) |*section_header| {
        section_header.pointer_to_raw_data -|= stripped_bytes;
        section_header.pointer_to_relocations -|= stripped_bytes;
        try writer.writeAll(std.mem.asBytes(section_header));
    }

    const data_to_skip = stripped_bytes -| @sizeOf(std.coff.SectionHeader);
    try r.skipBytes(data_to_skip, .{});

    try pumpBytes(r, writer, sections.items[0].size_of_raw_data);
    for (0..sections.items[0].number_of_relocations) |_| {
        var relocation = std.coff.Relocation{
            .virtual_address = try r.readInt(u32, .little),
            .symbol_table_index = try r.readInt(u32, .little),
            .type = try r.readInt(u16, .little),
        };
        relocation.symbol_table_index -= stripped_symbols;
        try writer.writeInt(u32, relocation.virtual_address, .little);
        try writer.writeInt(u32, relocation.symbol_table_index, .little);
        try writer.writeInt(u16, relocation.type, .little);
    }
    try pumpBytes(r, writer, sections.items[1].size_of_raw_data);

    var symbol_i: usize = 0;
    while (symbol_i < original_number_of_symbols) : (symbol_i += 1) {
        var symbol_bytes = try r.readBytesNoEof(std.coff.Symbol.sizeOf());
        const name = symbol_bytes[0..8];
        const number_of_aux_symbols = symbol_bytes[17];
        const should_strip = std.mem.startsWith(u8, name, ".debug") or (fixups.strip_comp_id and std.mem.eql(u8, name, "@comp.id"));
        if (should_strip) {
            std.debug.assert(number_of_aux_symbols <= 1);
            if (number_of_aux_symbols == 1) {
                try r.skipBytes(std.coff.Symbol.sizeOf(), .{ .buf_size = std.coff.Symbol.sizeOf() });
            }
        } else {
            const section_number: std.coff.SectionNumber = @enumFromInt(std.mem.readInt(u16, symbol_bytes[12..14], .little));
            if (name[0] != '@' and section_number != .UNDEFINED and section_number != .ABSOLUTE and section_number != .DEBUG) {
                std.mem.writeInt(u16, symbol_bytes[12..14], @intFromEnum(section_number) - stripped_sections, .little);
            }
            try writer.writeAll(&symbol_bytes);
            std.debug.assert(number_of_aux_symbols <= 1);
            if (number_of_aux_symbols == 1) {
                symbol_bytes = try r.readBytesNoEof(std.coff.Symbol.sizeOf());
                try writer.writeAll(&symbol_bytes);
            }
        }
        symbol_i += number_of_aux_symbols;
    }

    var string_table_size = try r.readInt(u32, .little);
    if (fixups.correct_string_table_length) {
        string_table_size = @max(4, string_table_size);
    }
    try writer.writeInt(u32, string_table_size, .little);
    if (string_table_size > 4) {
        const remaining_data = string_table_size - 4;
        try pumpBytes(r, writer, remaining_data);
    }
}

fn pumpBytes(reader: anytype, writer: anytype, num_bytes: usize) !void {
    var limited_reader = std.io.limitedReader(reader, num_bytes);

    const FifoBuffer = std.fifo.LinearFifo(u8, .{ .Static = 256 });
    var fifo = FifoBuffer.init();
    try fifo.pump(limited_reader.reader(), writer);
}

pub fn randomNumberLiteral(allocator: Allocator, rand: std.Random, comptime include_superscripts: bool) ![]const u8 {
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

pub fn randomAlpha(rand: std.Random) u8 {
    const index = rand.uintLessThanBiased(u8, dicts.alpha.len);
    return dicts.alpha[index];
}

pub fn randomAlphanumeric(rand: std.Random) u8 {
    const index = rand.uintLessThanBiased(u8, dicts.alphanumeric.len);
    return dicts.alphanumeric[index];
}

pub fn randomNumeric(rand: std.Random) u8 {
    return rand.uintLessThanBiased(u8, 10) + '0';
}

/// Includes Windows-1252 encoded ¹ ² ³
pub fn randomNumericRC(rand: std.Random) u8 {
    const dict = dicts.digits ++ [_]u8{ '\xb2', '\xb3', '\xb9' };
    const index = rand.uintLessThanBiased(u8, dict.len);
    return dict[index];
}

/// Includes Windows-1252 encoded ¹ ² ³
pub fn randomAlphanumericRC(rand: std.Random) u8 {
    const dict = dicts.alphanumeric ++ [_]u8{ '\xb2', '\xb3', '\xb9' };
    const index = rand.uintLessThanBiased(u8, dict.len);
    return dict[index];
}

pub fn randomOperator(rand: std.Random) u8 {
    const dict = [_]u8{ '-', '+', '|', '&' };
    const index = rand.uintLessThanBiased(u8, dict.len);
    return dict[index];
}

pub fn appendRandomStringLiteralSequence(rand: std.Random, buf: *std.ArrayList(u8), is_last: bool, comptime string_type: resinator.literals.StringType) !void {
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
    rand: std.Random,
    max_num_sequences: u16,
) ![]const u8 {
    const num_sequences = rand.uintAtMostBiased(u16, max_num_sequences);
    return randomStringLiteralExact(string_type, allocator, rand, num_sequences);
}

pub fn randomStringLiteralExact(
    comptime string_type: resinator.literals.StringType,
    allocator: Allocator,
    rand: std.Random,
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
pub fn randomAlphanumExtendedBytes(allocator: Allocator, rand: std.Random) ![]const u8 {
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

pub fn writePreface(writer: anytype) !void {
    try writer.writeAll("\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00");
}

pub fn writeRandomPotentiallyInvalidResource(allocator: Allocator, rand: std.Random, writer: anytype) !void {
    const real_data_size = rand.uintAtMostBiased(u32, 150);
    const reported_data_size = switch (rand.uintLessThan(u8, 4)) {
        0 => real_data_size,
        1 => real_data_size + rand.uintAtMost(u32, 150),
        2 => real_data_size - rand.uintAtMost(u32, real_data_size),
        3 => rand.int(u32),
        else => unreachable,
    };
    const name_value = try getRandomNameOrOrdinal(allocator, rand, 32);
    defer name_value.deinit(allocator);
    const type_value = try getRandomNameOrOrdinal(allocator, rand, 32);
    defer type_value.deinit(allocator);

    const header = resinator.compile.Compiler.ResourceHeader{
        .name_value = name_value,
        .type_value = type_value,
        .language = @bitCast(rand.int(u16)),
        .memory_flags = @bitCast(rand.int(u16)),
        .data_size = reported_data_size,
        .version = rand.int(u32),
        .characteristics = rand.int(u32),
        .data_version = rand.int(u32),
    };
    // only possible error is a field overflowing a u32, which we know can't happen
    var size_info = header.calcSize() catch unreachable;
    size_info.bytes = switch (rand.uintLessThan(u8, 4)) {
        0 => size_info.bytes,
        1 => size_info.bytes + rand.uintAtMost(u32, size_info.bytes),
        2 => size_info.bytes - rand.uintAtMost(u32, size_info.bytes),
        3 => rand.int(u32),
        else => unreachable,
    };
    size_info.padding_after_name = switch (rand.uintLessThan(u8, 3)) {
        0 => size_info.padding_after_name,
        1 => size_info.padding_after_name +| rand.uintAtMost(u2, 3),
        2 => size_info.padding_after_name - rand.uintAtMost(u2, size_info.padding_after_name),
        else => unreachable,
    };
    try header.writeSizeInfo(writer, size_info);

    const data = try allocator.alloc(u8, real_data_size);
    defer allocator.free(data);

    rand.bytes(data);
    try writer.writeAll(data);
    var num_padding_bytes = resinator.compile.Compiler.numPaddingBytesNeeded(real_data_size);
    num_padding_bytes = switch (rand.uintLessThan(u8, 3)) {
        0 => num_padding_bytes,
        1 => num_padding_bytes +| rand.uintAtMost(u2, 3),
        2 => num_padding_bytes - rand.uintAtMost(u2, num_padding_bytes),
        else => unreachable,
    };
    try writer.writeByteNTimes(0, num_padding_bytes);
}

pub const RandomResourceOptions = struct {
    set_name: ?resinator.res.NameOrOrdinal = null,
    set_type: ?resinator.res.NameOrOrdinal = null,
    set_language: ?resinator.res.Language = null,
    set_data: ?[]const u8 = null,
};

/// Returns the data size of the resource that was written
pub fn writeRandomValidResource(allocator: Allocator, rand: std.Random, writer: anytype, options: RandomResourceOptions) !u32 {
    const data_size: u32 = if (options.set_data) |data| @intCast(data.len) else rand.uintAtMostBiased(u32, 150);
    const name_value = options.set_name orelse try getRandomNameOrOrdinal(allocator, rand, 32);
    defer if (options.set_name == null) name_value.deinit(allocator);
    const type_value = options.set_type orelse try getRandomNameOrOrdinal(allocator, rand, 32);
    defer if (options.set_type == null) type_value.deinit(allocator);

    const header = resinator.compile.Compiler.ResourceHeader{
        .name_value = name_value,
        .type_value = type_value,
        .language = options.set_language orelse @bitCast(rand.int(u16)),
        .memory_flags = @bitCast(rand.int(u16)),
        .data_size = data_size,
        .version = rand.int(u32),
        .characteristics = rand.int(u32),
        .data_version = rand.int(u32),
    };
    // only possible error is a field overflowing a u32, which we know can't happen
    const size_info = header.calcSize() catch unreachable;
    try header.writeSizeInfo(writer, size_info);

    if (options.set_data) |data| {
        try writer.writeAll(data);
    } else {
        const data = try allocator.alloc(u8, data_size);
        defer allocator.free(data);

        rand.bytes(data);
        try writer.writeAll(data);
    }
    const num_padding_bytes = resinator.compile.Compiler.numPaddingBytesNeeded(data_size);
    try writer.writeByteNTimes(0, num_padding_bytes);

    return data_size;
}

pub fn getRandomNameOrOrdinal(allocator: Allocator, rand: std.Random, max_name_len: usize) !resinator.res.NameOrOrdinal {
    return switch (rand.boolean()) {
        true => resinator.res.NameOrOrdinal{ .name = try getRandomName(allocator, rand, max_name_len) },
        false => resinator.res.NameOrOrdinal{ .ordinal = rand.int(u16) },
    };
}

pub fn getRandomName(allocator: Allocator, rand: std.Random, max_len: usize) ![:0]const u16 {
    const code_unit_len = rand.uintAtMost(usize, max_len);
    const buf = try allocator.allocSentinel(u16, code_unit_len, 0);
    errdefer allocator.free(buf);

    for (buf) |*code_unit| {
        if (rand.boolean()) {
            // random ASCII codepoint, except NUL
            code_unit.* = rand.intRangeAtMost(u7, 1, std.math.maxInt(u7));
        } else {
            // entirely random code unit, except NUL
            code_unit.* = rand.intRangeAtMost(u16, 1, std.math.maxInt(u16));
        }
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
