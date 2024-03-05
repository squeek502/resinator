const std = @import("std");
const utils = @import("utils.zig");

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

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);

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

        utils.expectSameResOutput(allocator, source, .{
            .cwd = tmp.dir,
            .cwd_path = tmp_path,
        }) catch |err| {
            std.debug.print("\nfailing byte: 0x{X}\n", .{byte});
            return err;
        };

        if (byte == 255) break;
    }
}
