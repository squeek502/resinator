const std = @import("std");
const resinator = @import("resinator");

test "raw data" {
    const allocator = std.testing.allocator;
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var source_buffer = std.ArrayList(u8).init(allocator);
    defer source_buffer.deinit();

    var num: u8 = 0;
    while (num < 100) : (num += 1) {
        // RT_STRING as a number is a special case since it will always lead to invalid .res files
        if (num == 6) return;

        source_buffer.shrinkRetainingCapacity(0);

        const source_writer = source_buffer.writer();
        try source_writer.print("1 {d} {{ \"hello\" }}", .{num});

        const source = source_buffer.items;

        // TODO: Still try to compile and make sure we also fail the compilation
        const expected_res = resinator.compile.getExpectedFromWindowsRC(allocator, source) catch {
            std.debug.print("\n^^^^^^^^^^^^\nFound input that is rejected by the Windows RC compiler:\n\n{s}\n\n--------------------------------\n\n", .{std.fmt.fmtSliceEscapeLower(source)});
            continue;
        };
        defer allocator.free(expected_res);

        var diagnostics = resinator.errors.Diagnostics.init(allocator);
        defer diagnostics.deinit();

        buffer.shrinkRetainingCapacity(0);
        resinator.compile.compile(allocator, source, buffer.writer(), std.fs.cwd(), &diagnostics) catch |err| switch (err) {
            error.ParseError, error.CompileError => {
                diagnostics.renderToStdErr(std.fs.cwd(), source, null);
                return err;
            },
            else => return err,
        };

        resinator.utils.testing.expectEqualBytes(expected_res, buffer.items) catch |err| {
            std.debug.print("\nSource:\n{s}\n\n--------------------------------\n\n", .{std.fmt.fmtSliceEscapeLower(source)});
            return err;
        };
    }
}
