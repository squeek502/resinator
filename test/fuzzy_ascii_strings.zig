const std = @import("std");
const resinator = @import("resinator");
const utils = @import("utils.zig");

test "single chars" {
    const allocator = std.testing.allocator;
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var source_buf = "1 RCDATA { \"?\" }".*;
    var source: []u8 = &source_buf;
    const byte_index = std.mem.indexOfScalar(u8, source, '?').?;
    var byte: u8 = 1;
    while (true) : (byte += 1) {
        // \" is invalid within string literals (but accepted by the Windows RC compiler), skip it
        if (byte == '\\') continue;

        source[byte_index] = byte;

        const expected_res: ?[]const u8 = resinator.compile.getExpectedFromWindowsRC(allocator, source) catch null;
        defer if (expected_res != null) allocator.free(expected_res.?);

        var diagnostics = resinator.errors.Diagnostics.init(allocator);
        defer diagnostics.deinit();

        buffer.shrinkRetainingCapacity(0);
        resinator.compile.compile(allocator, source, buffer.writer(), std.fs.cwd(), &diagnostics) catch |err| switch (err) {
            error.ParseError, error.CompileError => {
                diagnostics.renderToStdErr(std.fs.cwd(), source, null);
                if (expected_res == null) {
                    if (byte == 255) break else continue;
                } else {
                    std.debug.print("\nSource:\n{s}\n\n--------------------------------\n\n", .{std.fmt.fmtSliceEscapeLower(source)});
                    return err;
                }
            },
            else => return err,
        };

        if (expected_res == null) {
            std.debug.print("\nSource:\n{s}\n\n--------------------------------\n\n", .{std.fmt.fmtSliceEscapeLower(source)});
            return error.ExpectedError;
        }

        resinator.utils.testing.expectEqualBytes(expected_res.?, buffer.items) catch |err| {
            std.debug.print("\nSource:\n{s}\n\n--------------------------------\n\n", .{std.fmt.fmtSliceEscapeLower(source)});
            return err;
        };

        if (byte == 255) break;
    }
}

test "single char escapes" {
    const allocator = std.testing.allocator;
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var source_buf = "1 RCDATA { \"\\?\" }".*;
    var source: []u8 = &source_buf;
    const escaped_byte_index = std.mem.indexOfScalar(u8, source, '?').?;
    var escaped_byte: u8 = 0;
    while (true) : (escaped_byte += 1) {
        // 'Substitute' leads to a false positive here, so skip it
        if (escaped_byte == '\x1A') continue;
        // \" is invalid within string literals (but accepted by the Windows RC compiler), skip it
        if (escaped_byte == '"') continue;

        source[escaped_byte_index] = escaped_byte;

        const expected_res: ?[]const u8 = resinator.compile.getExpectedFromWindowsRC(allocator, source) catch null;
        defer if (expected_res != null) allocator.free(expected_res.?);

        var diagnostics = resinator.errors.Diagnostics.init(allocator);
        defer diagnostics.deinit();

        buffer.shrinkRetainingCapacity(0);
        resinator.compile.compile(allocator, source, buffer.writer(), std.fs.cwd(), &diagnostics) catch |err| switch (err) {
            error.ParseError, error.CompileError => {
                diagnostics.renderToStdErr(std.fs.cwd(), source, null);
                if (expected_res == null) {
                    if (escaped_byte == 255) break else continue;
                } else {
                    std.debug.print("\nSource:\n{s}\n\n--------------------------------\n\n", .{std.fmt.fmtSliceEscapeLower(source)});
                    return err;
                }
            },
            else => return err,
        };

        if (expected_res == null) {
            std.debug.print("\nSource:\n{s}\n\n--------------------------------\n\n", .{std.fmt.fmtSliceEscapeLower(source)});
            return error.ExpectedError;
        }

        resinator.utils.testing.expectEqualBytes(expected_res.?, buffer.items) catch |err| {
            std.debug.print("\nSource:\n{s}\n\n--------------------------------\n\n", .{std.fmt.fmtSliceEscapeLower(source)});
            return err;
        };

        if (escaped_byte == 255) break;
    }
}

test "fuzz" {
    // Use a single tmp dir to avoid creating and cleaning up a dir for each RC invocation
    // Unfortunately there doesn't seem to be a way to avoid hitting the filesystem,
    // the Windows RC compiler doesn't seem to like named pipes for either input or output
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const allocator = std.testing.allocator;
    var random = std.rand.DefaultPrng.init(0);
    var rand = random.random();

    var source_buffer = std.ArrayList(u8).init(allocator);
    defer source_buffer.deinit();

    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    while (true) {
        source_buffer.shrinkRetainingCapacity(0);
        const literal = try utils.randomAsciiStringLiteral(allocator, rand);
        defer allocator.free(literal);
        var source_writer = source_buffer.writer();
        try source_writer.print("1 RCDATA {{ {s} }}", .{literal});

        const source = source_buffer.items;

        // write out the source file to disk for debugging
        try std.fs.cwd().writeFile("zig-cache/tmp/fuzzy_ascii_strings.rc", source);

        const expected_res: ?[]const u8 = resinator.compile.getExpectedFromWindowsRCWithDir(allocator, source, tmp.dir, "zig-cache/tmp/" ++ tmp.sub_path) catch null;
        defer if (expected_res != null) allocator.free(expected_res.?);

        var diagnostics = resinator.errors.Diagnostics.init(allocator);
        defer diagnostics.deinit();

        buffer.shrinkRetainingCapacity(0);
        resinator.compile.compile(allocator, source, buffer.writer(), tmp.dir, &diagnostics) catch |err| switch (err) {
            error.ParseError, error.CompileError => {
                diagnostics.renderToStdErr(std.fs.cwd(), source, null);
                if (expected_res != null) {
                    std.debug.print("\nSource:\n{s}\n\n--------------------------------\n\n", .{std.fmt.fmtSliceEscapeLower(source)});
                    return err;
                } else {
                    continue;
                }
            },
            else => return err,
        };

        if (expected_res == null) {
            std.debug.print("\nSource:\n{s}\n\n--------------------------------\n\n", .{std.fmt.fmtSliceEscapeLower(source)});
            std.debug.print("^^^^ expected error from resinator but successfully compiled instead\n\n\n", .{});
            return error.ExpectedError;
        }

        resinator.utils.testing.expectEqualBytes(expected_res.?, buffer.items) catch |e| {
            std.debug.print("\nSource:\n{s}\n\n--------------------------------\n\n", .{std.fmt.fmtSliceEscapeLower(source)});
            return e;
        };
    }
}
