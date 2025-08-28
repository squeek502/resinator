const std = @import("std");
const resinator = @import("resinator");
const utils = @import("test_utils");
const fuzzy_options = @import("fuzzy_options");
const iterations = fuzzy_options.max_iterations;

test "fuzz" {
    const allocator = std.testing.allocator;
    var random = std.Random.DefaultPrng.init(0);
    const rand = random.random();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);

    var source_buffer: std.ArrayList(u8) = .empty;
    defer source_buffer.deinit(allocator);

    var cache_path_buffer = std.array_list.Managed(u8).init(allocator);
    defer cache_path_buffer.deinit();

    var i: u64 = 0;
    while (iterations == 0 or i < iterations) : (i += 1) {
        const num_sequences = rand.uintAtMostBiased(u16, 512);
        const string_type: resinator.literals.StringType = if (i % 2 == 0) .wide else .ascii;
        const literal = switch (string_type) {
            .ascii => try utils.randomStringLiteralExact(.ascii, allocator, rand, num_sequences),
            .wide => try utils.randomStringLiteralExact(.wide, allocator, rand, num_sequences),
        };
        defer allocator.free(literal);

        const Variation = enum {
            io_windows1252,
            in_1252_out_utf8,
            io_utf8,
            in_utf8_out_1252,
        };
        for (std.enums.values(Variation)) |variation| {
            source_buffer.clearRetainingCapacity();
            // Swap the input code page using #pragma code_page
            // (this only works if its the first #pragma code_page in the file)
            switch (variation) {
                .in_1252_out_utf8 => try source_buffer.appendSlice(allocator, "#pragma code_page(1252)\n"),
                .in_utf8_out_1252 => try source_buffer.appendSlice(allocator, "#pragma code_page(65001)\n"),
                else => {},
            }
            try source_buffer.print(allocator, "1 DLGINCLUDE {s}", .{literal});

            const source = source_buffer.items;

            utils.expectSameResOutput(allocator, source, .{
                .cwd = tmp.dir,
                .cwd_path = tmp_path,
                .default_code_page = switch (variation) {
                    // The default code page affects the output code page only if the first
                    // #pragma code_page changes the code page
                    .io_windows1252, .in_utf8_out_1252 => .windows1252,
                    .io_utf8, .in_1252_out_utf8 => .utf8,
                },
            }) catch |err| {
                cache_path_buffer.clearRetainingCapacity();
                try cache_path_buffer.appendSlice(".zig-cache/tmp/fuzzy_dlginclude_");
                try utils.appendNumberStr(&cache_path_buffer, i);
                try cache_path_buffer.append('_');
                try cache_path_buffer.appendSlice(@tagName(string_type));
                try cache_path_buffer.append('_');
                try cache_path_buffer.appendSlice(@tagName(variation));
                try cache_path_buffer.append('_');
                try utils.appendNumberStr(&cache_path_buffer, num_sequences);
                try cache_path_buffer.append('_');
                try cache_path_buffer.appendSlice(@errorName(err));
                try cache_path_buffer.appendSlice(".rc");

                // write out the source file to disk for debugging
                try std.fs.cwd().writeFile(.{ .sub_path = cache_path_buffer.items, .data = source });
            };
        }
    }
}

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
        _ = std.fmt.bufPrint(source[byte_index..], "{o:0>3}", .{value}) catch unreachable;

        utils.expectSameResOutput(allocator, source, .{
            .cwd = tmp.dir,
            .cwd_path = tmp_path,
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
        _ = std.fmt.bufPrint(source[byte_index..], "{o:0>3}", .{value}) catch unreachable;

        utils.expectSameResOutput(allocator, source, .{
            .cwd = tmp.dir,
            .cwd_path = tmp_path,
        }) catch {
            std.debug.print("difference found for {} (0o{o})\n\n", .{ value, value });
        };

        if (value == 0o777) break;
    }
}
