const std = @import("std");
const utils = @import("utils.zig");
const fuzzy_options = @import("fuzzy_options");
const iterations = fuzzy_options.max_iterations;

test "octal escapes, ascii string literal" {
    const allocator = std.testing.allocator;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);

    var source_buf = "1 DLGINCLUDE \"\\???\"".*;
    var source: []u8 = &source_buf;
    const byte_index = std.mem.indexOfScalar(u8, source, '?').?;
    var value: u32 = 1;
    while (true) : (value += 1) {
        std.debug.print("{} (0o{o})\n", .{ value, value });
        _ = std.fmt.bufPrint(source[byte_index..], "{o:0>3}", .{value}) catch unreachable;

        utils.expectSameResOutput(allocator, source, .{
            .cwd = tmp.dir,
            .cwd_path = tmp_path,
            .run_preprocessor = false,
        }) catch {
            std.debug.print("difference found for {} (0o{o})\n\n", .{ value, value });
        };

        if (value == 0o777) break;
    }
}

test "octal escapes, wide string literal" {
    const allocator = std.testing.allocator;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);

    var source_buf = "1 DLGINCLUDE L\"\\???\"".*;
    var source: []u8 = &source_buf;
    const byte_index = std.mem.indexOfScalar(u8, source, '?').?;
    var value: u32 = 1;
    while (true) : (value += 1) {
        std.debug.print("{} (0o{o})\n", .{ value, value });
        _ = std.fmt.bufPrint(source[byte_index..], "{o:0>3}", .{value}) catch unreachable;

        utils.expectSameResOutput(allocator, source, .{
            .cwd = tmp.dir,
            .cwd_path = tmp_path,
            .run_preprocessor = false,
        }) catch {
            std.debug.print("difference found for {} (0o{o})\n\n", .{ value, value });
        };

        if (value == 0o777) break;
    }
}
