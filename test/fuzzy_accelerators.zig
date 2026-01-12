const std = @import("std");
const resinator = @import("resinator");
const utils = @import("test_utils");
const fuzzy_options = @import("fuzzy_options");
const iterations = fuzzy_options.max_iterations;

test "fuzz" {
    const io = std.testing.io;
    const allocator = std.testing.allocator;
    var random = std.Random.DefaultPrng.init(0);
    const rand = random.random();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(tmp_path);

    var source_buffer: std.ArrayList(u8) = .empty;
    defer source_buffer.deinit(allocator);

    var cache_path_buffer: std.array_list.Managed(u8) = .init(allocator);
    defer cache_path_buffer.deinit();

    var i: u64 = 0;
    while (iterations == 0 or i < iterations) : (i += 1) {
        const num_sequences = rand.uintAtMostBiased(u16, 3) + 1;
        const string_type: resinator.literals.StringType = if (i % 2 == 0) .wide else .ascii;
        const control = rand.boolean();
        const literal = literal: {
            const literal = switch (string_type) {
                .ascii => try utils.randomStringLiteralExact(.ascii, allocator, rand, num_sequences),
                .wide => try utils.randomStringLiteralExact(.wide, allocator, rand, num_sequences),
            };
            if (!control) break :literal literal;
            defer allocator.free(literal);
            const insert_i: usize = if (string_type == .ascii) 1 else 2;
            const control_literal = try allocator.alloc(u8, literal.len + 1);
            @memcpy(control_literal[0..insert_i], literal[0..insert_i]);
            control_literal[insert_i] = '^';
            @memcpy(control_literal[insert_i + 1 ..], literal[insert_i..]);
            break :literal control_literal;
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
            try source_buffer.print(allocator, "1 ACCELERATORS {{ {s}, 0x1 }}", .{literal});

            const source = source_buffer.items;

            utils.expectSameResOutput(allocator, io, source, .{
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
                try cache_path_buffer.appendSlice(".zig-cache/tmp/fuzzy_accelerators_");
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
                try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = cache_path_buffer.items, .data = source });
            };
        }
    }
}
