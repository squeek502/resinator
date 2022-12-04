const std = @import("std");

pub fn windows1252ToUtf8Stream(writer: anytype, reader: anytype) !usize {
    var bytes_written: usize = 0;
    var utf8_buf: [3]u8 = undefined;
    while (true) {
        const c = reader.readByte() catch |err| switch (err) {
            error.EndOfStream => return bytes_written,
            else => |e| return e,
        };
        const codepoint = toCodepoint(c);
        if (codepoint <= 0x7F) {
            try writer.writeByte(c);
            bytes_written += 1;
        } else {
            const utf8_len = std.unicode.utf8Encode(codepoint, &utf8_buf) catch unreachable;
            try writer.writeAll(utf8_buf[0..utf8_len]);
            bytes_written += utf8_len;
        }
    }
}

/// https://www.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WindowsBestFit/bestfit1252.txt
pub fn toCodepoint(c: u8) u16 {
    return switch (c) {
        0x80 => 0x20ac, // Euro Sign
        0x82 => 0x201a, // Single Low-9 Quotation Mark
        0x83 => 0x0192, // Latin Small Letter F With Hook
        0x84 => 0x201e, // Double Low-9 Quotation Mark
        0x85 => 0x2026, // Horizontal Ellipsis
        0x86 => 0x2020, // Dagger
        0x87 => 0x2021, // Double Dagger
        0x88 => 0x02c6, // Modifier Letter Circumflex Accent
        0x89 => 0x2030, // Per Mille Sign
        0x8a => 0x0160, // Latin Capital Letter S With Caron
        0x8b => 0x2039, // Single Left-Pointing Angle Quotation Mark
        0x8c => 0x0152, // Latin Capital Ligature Oe
        0x8e => 0x017d, // Latin Capital Letter Z With Caron
        0x91 => 0x2018, // Left Single Quotation Mark
        0x92 => 0x2019, // Right Single Quotation Mark
        0x93 => 0x201c, // Left Double Quotation Mark
        0x94 => 0x201d, // Right Double Quotation Mark
        0x95 => 0x2022, // Bullet
        0x96 => 0x2013, // En Dash
        0x97 => 0x2014, // Em Dash
        0x98 => 0x02dc, // Small Tilde
        0x99 => 0x2122, // Trade Mark Sign
        0x9a => 0x0161, // Latin Small Letter S With Caron
        0x9b => 0x203a, // Single Right-Pointing Angle Quotation Mark
        0x9c => 0x0153, // Latin Small Ligature Oe
        0x9e => 0x017e, // Latin Small Letter Z With Caron
        0x9f => 0x0178, // Latin Capital Letter Y With Diaeresis
        else => c,
    };
}

test "windows-1252 to utf8" {
    var buf = std.ArrayList(u8).init(std.testing.allocator);
    defer buf.deinit();

    const input_windows1252 = "\x81pqrstuvwxyz{|}~\x80\x82\x83\x84\x85\x86\x87\x88\x89\x8a\x8b\x8c\x8e\x91\x92\x93\x94\x95\x96\x97\x98\x99\x9a\x9b\x9c\x9e\x9f\xa1\xa2\xa3\xa4\xa5\xa6\xa7\xa8\xa9\xaa\xab\xac\xae\xaf\xb0\xb1\xb2\xb3\xb4\xb5\xb6\xb7\xb8\xb9\xba\xbb\xbc\xbd\xbe\xbf\xc0\xc1\xc2\xc3\xc4\xc5\xc6\xc7\xc8\xc9\xca\xcb\xcc\xcd\xce\xcf\xd0\xd1\xd2\xd3\xd4\xd5\xd6\xd7\xd8\xd9\xda\xdb\xdc\xdd\xde\xdf\xe0\xe1\xe2\xe3\xe4\xe5\xe6\xe7\xe8\xe9\xea\xeb\xec\xed\xee\xef\xf0\xf1\xf2\xf3\xf4\xf5\xf6\xf7\xf8\xf9\xfa\xfb\xfc\xfd\xfe\xff";
    const expected_utf8 = "\xc2\x81pqrstuvwxyz{|}~€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—˜™š›œžŸ¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ";

    var fbs = std.io.fixedBufferStream(input_windows1252);
    const bytes_written = try windows1252ToUtf8Stream(buf.writer(), fbs.reader());

    try std.testing.expectEqualStrings(expected_utf8, buf.items);
    try std.testing.expectEqual(expected_utf8.len, bytes_written);
}
