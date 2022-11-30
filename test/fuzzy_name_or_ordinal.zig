const std = @import("std");
const resinator = @import("resinator");
const utils = @import("utils.zig");

test {
    const allocator = std.testing.allocator;
    var random = std.rand.DefaultPrng.init(0);
    var rand = random.random();

    var source_buffer = std.ArrayList(u8).init(allocator);
    defer source_buffer.deinit();

    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    while (true) {
        source_buffer.shrinkRetainingCapacity(0);

        const number_literal = try utils.randomNumberLiteral(allocator, rand, false);
        defer allocator.free(number_literal);

        var source_writer = source_buffer.writer();
        try source_writer.print("{s} RCDATA {{}}", .{number_literal});

        const source = source_buffer.items;

        const expected_res: ?[]const u8 = resinator.compile.getExpectedFromWindowsRC(allocator, source) catch null;
        defer if (expected_res != null) allocator.free(expected_res.?);

        var diagnostics = resinator.errors.Diagnostics.init(allocator);
        defer diagnostics.deinit();

        buffer.shrinkRetainingCapacity(0);
        resinator.compile.compile(allocator, source, buffer.writer(), std.fs.cwd(), &diagnostics) catch |err| switch (err) {
            error.ParseError, error.CompileError => {
                diagnostics.renderToStdErr(std.fs.cwd(), source, null);
                if (expected_res == null) {
                    continue;
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

        resinator.utils.testing.expectEqualBytes(expected_res.?, buffer.items) catch |err| {
            std.debug.print("\nSource:\n{s}\n\n--------------------------------\n\n", .{std.fmt.fmtSliceEscapeLower(source)});
            return err;
        };
    }
}
