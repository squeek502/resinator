const std = @import("std");
const resinator = @import("resinator");

test "single chars" {
    const allocator = std.testing.allocator;
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var source_buf = "1 RCDATA { \"?\" }".*;
    var source: []u8 = &source_buf;
    const byte_index = std.mem.indexOfScalar(u8, source, '?').?;
    var byte: u8 = 0;
    while (true) : (byte += 1) {
        // quotes are a special case that shouldn't be tested here
        if (byte == '"') continue;

        source[byte_index] = byte;

        const expected_res = resinator.compile.getExpectedFromWindowsRC(allocator, source) catch {
            std.debug.print("\n^^^^^^^^^^^^\nFound input that is rejected by the Windows RC compiler:\n\n{s}\n\n--------------------------------\n\n", .{std.fmt.fmtSliceEscapeLower(source)});
            if (byte != 255) continue else break;
        };
        defer allocator.free(expected_res);

        buffer.shrinkRetainingCapacity(0);
        try resinator.compile.compile(allocator, source, buffer.writer(), std.fs.cwd());

        std.testing.expectEqualSlices(u8, expected_res, buffer.items) catch {
            std.debug.print("\nSource:\n{s}\n\n--------------------------------\n\n", .{std.fmt.fmtSliceEscapeLower(source)});
            continue;
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
        // quotes are a special case that shouldn't be tested here
        if (escaped_byte == '"') continue;

        source[escaped_byte_index] = escaped_byte;

        const expected_res = resinator.compile.getExpectedFromWindowsRC(allocator, source) catch {
            std.debug.print("\n^^^^^^^^^^^^\nFound input that is rejected by the Windows RC compiler:\n\n{s}\n\n--------------------------------\n\n", .{std.fmt.fmtSliceEscapeLower(source)});
            if (escaped_byte != 255) continue else break;
        };
        defer allocator.free(expected_res);

        buffer.shrinkRetainingCapacity(0);
        try resinator.compile.compile(allocator, source, buffer.writer(), std.fs.cwd());

        std.testing.expectEqualSlices(u8, expected_res, buffer.items) catch {
            std.debug.print("\nSource:\n{s}\n\n--------------------------------\n\n", .{std.fmt.fmtSliceEscapeLower(source)});
            continue;
        };

        if (escaped_byte == 255) break;
    }
}

// test {
//     const allocator = std.testing.allocator;
//     var random = std.rand.DefaultPrng.init(0);
//     var rand = random.random();

//     var source_buffer = std.ArrayList(u8).init(allocator);
//     defer source_buffer.deinit();

//     var buffer = std.ArrayList(u8).init(allocator);
//     defer buffer.deinit();

//     while (true) {
//         source_buffer.shrinkRetainingCapacity(0);
//         const literal = try utils.randomAsciiStringLiteral(allocator, rand);
//         defer allocator.free(literal);
//         var source_writer = source_buffer.writer();
//         try source_writer.print("1 RCDATA {{ {s} }}", .{literal});

//         const source = source_buffer.items;

//         const expected_res = resinator.compile.getExpectedFromWindowsRC(allocator, source) catch {
//             std.debug.print("\n^^^^^^^^^^^^\nFound input that is rejected by the Windows RC compiler:\n\n{s}\n\n--------------------------------\n\n", .{source});
//             continue;
//         };
//         defer allocator.free(expected_res);

//         buffer.shrinkRetainingCapacity(0);

//         try resinator.compile.compile(allocator, source, buffer.writer(), std.fs.cwd());

//         std.testing.expectEqualSlices(u8, expected_res, buffer.items) catch |e| {
//             std.debug.print("\nSource:\n{s}\n", .{source});
//             return e;
//         };
//     }
// }
