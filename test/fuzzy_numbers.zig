const std = @import("std");
const resinator = @import("resinator");
const utils = @import("utils.zig");
const fuzzy_options = @import("fuzzy_options");
const iterations = fuzzy_options.max_iterations;

test {
    const allocator = std.testing.allocator;
    var random = std.rand.DefaultPrng.init(0);
    var rand = random.random();

    var source_buffer = std.ArrayList(u8).init(allocator);
    defer source_buffer.deinit();

    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var i: u64 = 0;
    while (iterations == 0 or i < iterations) : (i += 1) {
        source_buffer.shrinkRetainingCapacity(0);
        const literal = try utils.randomNumberLiteral(allocator, rand, true);
        defer allocator.free(literal);
        var source_writer = source_buffer.writer();
        try source_writer.print("1 RCDATA {{ {s} }}", .{literal});

        const source = source_buffer.items;

        const expected_res = resinator.compile.getExpectedFromWindowsRC(allocator, source) catch {
            std.debug.print("\n^^^^^^^^^^^^\nFound input that is rejected by the Windows RC compiler:\n\n{s}\n\n--------------------------------\n\n", .{source});
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

        resinator.utils.testing.expectEqualBytes(expected_res, buffer.items) catch |e| {
            std.debug.print("\nSource:\n{s}\n", .{source});
            return e;
        };
    }
}
