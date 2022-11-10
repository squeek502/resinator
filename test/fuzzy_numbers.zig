const std = @import("std");
const resinator = @import("resinator");

// TODO: The actual fuzzing

test {
    const source = "1 RCDATA {}";
    const expected_res = try resinator.compile.getExpectedFromWindowsRC(std.testing.allocator, source);
    defer std.testing.allocator.free(expected_res);

    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();

    try resinator.compile.compile(std.testing.allocator, source, buffer.writer(), std.fs.cwd());

    try std.testing.expectEqualSlices(u8, expected_res, buffer.items);
}
