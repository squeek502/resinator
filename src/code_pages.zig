const std = @import("std");
const windows1252 = @import("windows1252.zig");

// ‰ representations for context:
// Win-1252   89
// UTF-8      E2 80 B0
// UTF-16     20 30
//
// With code page 65001:
//  ‰ RCDATA { "‰" L"‰" }
// File encoded as Windows-1252:
//  ‰ => <U+FFFD REPLACEMENT CHARACTER> as u16
//  "‰" => 0x3F ('?')
//  L"‰" => <U+FFFD REPLACEMENT CHARACTER> as u16
// File encoded as UTF-8:
//  ‰ => <U+2030 ‰> as u16
//  "‰" => 0x89 ('‰' encoded as Windows-1252)
//  L"‰" => <U+2030 ‰> as u16
//
// With code page 1252:
//  ‰ RCDATA { "‰" L"‰" }
// File encoded as Windows-1252:
//  ‰ => <U+2030 ‰> as u16
//  "‰" => 0x89 ('‰' encoded as Windows-1252)
//  L"‰" => <U+2030 ‰> as u16
// File encoded as UTF-8:
//  ‰ => 0xE2 as u16, 0x20AC as u16, 0xB0 as u16
//       ^ first byte of utf8 representation
//                    ^ second byte of UTF-8 representation (0x80), but interpretted as
//                      Windows-1252 ('€') and then converted to UTF-16 (<U+20AC>)
//                                   ^ third byte of utf8 representation
//  "‰" => 0xE2, 0x80, 0xB0 (the bytes of the UTF-8 representation)
//  L"‰" => 0xE2 as u16, 0x20AC as u16, 0xB0 as u16 (see '‰ =>' explanation)
//
// With code page 1252:
//  <0x90> RCDATA { "<0x90>" L"<0x90>" }
// File encoded as Windows-1252:
//  <0x90> => 0x90 as u16
//  "<0x90>" => 0x90
//  L"<0x90>" => 0x90 as u16
// File encoded as UTF-8:
//  <0x90> => 0xC2 as u16, 0x90 as u16
//  "<0x90>" => 0xC2, 0x90 (the bytes of the UTF-8 representation of <U+0090>)
//  L"<0x90>" => 0xC2 as u16, 0x90 as u16

pub const CodePage = enum(u16) {
    windows1252 = 1252,
    utf8 = 65001,

    pub fn codepointAt(code_page: CodePage, index: usize, bytes: []const u8) ?Codepoint {
        if (index >= bytes.len) return null;
        switch (code_page) {
            .windows1252 => {
                // All byte values have a representation, so just convert the byte
                return Codepoint{
                    .value = windows1252.toCodepoint(bytes[index]),
                    .byte_len = 1,
                };
            },
            .utf8 => {
                const len = std.unicode.utf8ByteSequenceLength(bytes[index]) catch {
                    // The first byte is invalid, return length of 1
                    return Codepoint{
                        .value = Codepoint.invalid,
                        .byte_len = 1,
                    };
                };

                if (index + len > bytes.len) {
                    // TODO: This might be the wrong way to handle this
                    return Codepoint{
                        .value = Codepoint.invalid,
                        .byte_len = 1,
                    };
                }

                const codepoint_slice = bytes[index .. index + len];
                const codepoint = std.unicode.utf8Decode(codepoint_slice) catch {
                    // TODO: This might be the wrong way to handle this
                    return Codepoint{
                        .value = Codepoint.invalid,
                        .byte_len = 1,
                    };
                };

                return Codepoint{
                    .value = codepoint,
                    .byte_len = len,
                };
            },
        }
    }
};

test "codepoint at utf8 encoded" {
    const utf8_encoded = "²";

    // with code page utf8
    try std.testing.expectEqual(Codepoint{
        .value = '²',
        .byte_len = 2,
    }, CodePage.utf8.codepointAt(0, utf8_encoded).?);
    try std.testing.expectEqual(@as(?Codepoint, null), CodePage.utf8.codepointAt(2, utf8_encoded));

    // with code page windows1252
    try std.testing.expectEqual(Codepoint{
        .value = '\xC2',
        .byte_len = 1,
    }, CodePage.windows1252.codepointAt(0, utf8_encoded).?);
    try std.testing.expectEqual(Codepoint{
        .value = '\xB2',
        .byte_len = 1,
    }, CodePage.windows1252.codepointAt(1, utf8_encoded).?);
    try std.testing.expectEqual(@as(?Codepoint, null), CodePage.windows1252.codepointAt(2, utf8_encoded));
}

test "codepoint at windows1252 encoded" {
    const windows1252_encoded = "\xB2";

    // with code page utf8
    try std.testing.expectEqual(Codepoint{
        .value = Codepoint.invalid,
        .byte_len = 1,
    }, CodePage.utf8.codepointAt(0, windows1252_encoded).?);
    try std.testing.expectEqual(@as(?Codepoint, null), CodePage.utf8.codepointAt(2, windows1252_encoded));

    // with code page windows1252
    try std.testing.expectEqual(Codepoint{
        .value = '\xB2',
        .byte_len = 1,
    }, CodePage.windows1252.codepointAt(0, windows1252_encoded).?);
    try std.testing.expectEqual(@as(?Codepoint, null), CodePage.windows1252.codepointAt(1, windows1252_encoded));
}

pub const Codepoint = struct {
    value: u21,
    byte_len: usize,

    pub const invalid: u21 = std.math.maxInt(u21);
};
