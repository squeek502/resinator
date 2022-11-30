const std = @import("std");
const resinator = @import("resinator");

test "single char in raw data block" {
    var source_buf = "1 RCDATA { ? }".*;
    try testAllBytes(&source_buf);
}

test "number literal in raw data block" {
    var source_buf = "1 RCDATA { 1? }".*;
    try testAllBytes(&source_buf);
}

test "literal in raw data block" {
    var source_buf = "1 RCDATA { a? }".*;
    try testAllBytes(&source_buf);
}

fn testAllBytes(source: []u8) !void {
    const allocator = std.testing.allocator;
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    const byte_index = std.mem.indexOfScalar(u8, source, '?').?;
    var byte: u8 = 1;
    while (true) : (byte += 1) {
        // \x04 is a special case that we currently force to be an error
        if (byte == 4) continue;
        // TODO: Having a limited amount of trailing stuff after resource definitions is not actually an error.
        if (byte == '}') continue;
        // TODO: ¹ ² ³ (encoded as Windows-1252) are inexplicably valid in number literals
        // NOTE: This is also true when the encoding is UTF-8, in that case the bytes would
        //       be 0xC2 0xB2, 0xC2 0xB3, 0xC2 0xB9
        if (byte == '\xb2' or byte == '\xb3' or byte == '\xb9') continue;

        source[byte_index] = byte;
        std.debug.print("byte: 0x{X}\n", .{byte});

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
                    return error.DidNotExpectErrorButGotOne;
                }
            },
            else => return err,
        };

        if (expected_res == null) {
            std.debug.print("\nSource:\n{s}\n\n--------------------------------\n\n", .{std.fmt.fmtSliceEscapeLower(source)});
            return error.ExpectedErrorButDidntGetOne;
        }

        resinator.utils.testing.expectEqualBytes(expected_res.?, buffer.items) catch {
            std.debug.print("\nSource:\n{s}\n\n--------------------------------\n\n", .{std.fmt.fmtSliceEscapeLower(source)});
            continue;
        };

        if (byte == 255) break;
    }
}
