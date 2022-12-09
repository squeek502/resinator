const std = @import("std");
const windows1252 = @import("windows1252.zig");

// TODO: Parts of this comment block may be more relevant to string/NameOrOrdinal parsing
//       than it is to the stuff in this file.
//
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
//
// Within a raw data block, file encoded as Windows-1252 (Â is <0xC2>):
//  "Âa" L"Âa" "\xC2ad" L"\xC2AD"
// With code page 1252:
//  C2 61 C2 00 61 00 C2 61 64 AD C2
//  Â^ a^ Â~~~^ a~~~^ .^ a^ d^ ^~~~~\xC2AD
//              \xC2~`
// With code page 65001:
//  3F 61 FD FF 61 00 C2 61 64 AD C2
//  ^. a^ ^~~~. a~~~^ ^. a^ d^ ^~~~~\xC2AD
//    `.       `.       `~\xC2
//      `.       `.~<0xC2>a is not well-formed UTF-8 (0xC2 expects a continutation byte after it).
//        `.        Because 'a' is a valid first byte of a UTF-8 sequence, it is not included in the
//          `.      invalid sequence so only the <0xC2> gets converted to <U+FFFD>.
//            `~Same as ^ but converted to '?' instead.
//
// Within a raw data block, file encoded as Windows-1252 (ð is <0xF0>, € is <0x80>):
//  "ð€a" L"ð€a"
// With code page 1252:
//  F0 80 61 F0 00 AC 20 61 00
//  ð^ €^ a^ ð~~~^ €~~~^ a~~~^
// With code page 65001:
//  3F 61 FD FF 61 00
//  ^. a^ ^~~~. a~~~^
//    `.       `.
//      `.       `.~<0xF0><0x80> is not well-formed UTF-8, and <0x80> is not a valid first byte, so
//        `.        both bytes are considered an invalid sequence and get converted to '<U+FFFD>'
//          `~Same as ^ but converted to '?' instead.

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
                return Utf8.WellFormedDecoder.decode(bytes[index..]);
            },
        }
    }

    pub fn getByNumber(num: u16) !CodePage {
        return switch (num) {
            1252 => .windows1252,
            65001 => .utf8,
            // TODO: Separate out 'invalid' and 'unsupported'
            else => return error.InvalidCodePage,
        };
    }
};

pub const Utf8 = struct {
    /// Implements decoding with rejection of ill-formed UTF-8 sequences based on section
    /// D92 of Chapter 3 of the Unicode standard (Table 3-7 specifically).
    pub const WellFormedDecoder = struct {
        /// Like std.unicode.utf8ByteSequenceLength, but:
        /// - Rejects non-well-formed first bytes, i.e. C0-C1, F5-FF
        /// - Returns an optional value instead of an error union
        pub fn sequenceLength(first_byte: u8) ?u3 {
            return switch (first_byte) {
                0x00...0x7F => 1,
                0xC2...0xDF => 2,
                0xE0...0xEF => 3,
                0xF0...0xF4 => 4,
                else => null,
            };
        }

        pub fn decode(bytes: []const u8) Codepoint {
            std.debug.assert(bytes.len > 0);
            var first_byte = bytes[0];
            var expected_len = sequenceLength(first_byte) orelse {
                return .{ .value = Codepoint.invalid, .byte_len = 1 };
            };
            if (expected_len == 1) return .{ .value = first_byte, .byte_len = 1 };

            var value: u21 = first_byte & 0b00011111;
            var byte_index: u8 = 1;
            while (byte_index < expected_len) : (byte_index += 1) {
                const byte = bytes[byte_index];
                // See Table 3-7 of D92 in Chapter 3 of the Unicode Standard
                const valid: bool = switch (byte_index) {
                    1 => switch (first_byte) {
                        0xE0 => switch (byte) {
                            0xA0...0xBF => true,
                            else => false,
                        },
                        0xED => switch (byte) {
                            0x80...0x9F => true,
                            else => false,
                        },
                        0xF0 => switch (byte) {
                            0x90...0xBF => true,
                            else => false,
                        },
                        0xF4 => switch (byte) {
                            0x80...0x8F => true,
                            else => false,
                        },
                        else => switch (byte) {
                            0x80...0xBF => true,
                            else => false,
                        },
                    },
                    else => switch (byte) {
                        0x80...0xBF => true,
                        else => false,
                    },
                };

                if (!valid) {
                    // If the current byte is a valid first byte, then don't
                    // include it in the current invalid sequence.
                    var len = byte_index;
                    // Note: Using utf8ByteSequenceLength here means ignoring
                    // well-formedness, e.g. C0 is considered a valid first
                    // byte for this even though it is disallowed in well-formed
                    // UTF-8 byte sequences.
                    _ = std.unicode.utf8ByteSequenceLength(byte) catch {
                        len += 1;
                    };
                    return .{ .value = Codepoint.invalid, .byte_len = len };
                }

                value <<= 6;
                value |= byte & 0b00111111;
            }
            return .{ .value = value, .byte_len = expected_len };
        }
    };
};

test "Utf8.WellFormedDecoder" {
    const invalid_utf8 = "\xF0\x80";
    var decoded = Utf8.WellFormedDecoder.decode(invalid_utf8);
    try std.testing.expectEqual(Codepoint.invalid, decoded.value);
    try std.testing.expectEqual(@as(usize, 2), decoded.byte_len);
}

test "codepointAt invalid utf8" {
    {
        const invalid_utf8 = "\xf0\xf0\x80\x80\x80";
        try std.testing.expectEqual(Codepoint{
            .value = Codepoint.invalid,
            .byte_len = 1,
        }, CodePage.utf8.codepointAt(0, invalid_utf8).?);
        try std.testing.expectEqual(Codepoint{
            .value = Codepoint.invalid,
            .byte_len = 2,
        }, CodePage.utf8.codepointAt(1, invalid_utf8).?);
        try std.testing.expectEqual(Codepoint{
            .value = Codepoint.invalid,
            .byte_len = 1,
        }, CodePage.utf8.codepointAt(3, invalid_utf8).?);
        try std.testing.expectEqual(Codepoint{
            .value = Codepoint.invalid,
            .byte_len = 1,
        }, CodePage.utf8.codepointAt(4, invalid_utf8).?);
        try std.testing.expectEqual(@as(?Codepoint, null), CodePage.windows1252.codepointAt(5, invalid_utf8));
    }

    {
        const invalid_utf8 = "\xE1\xA0\xC0";
        try std.testing.expectEqual(Codepoint{
            .value = Codepoint.invalid,
            .byte_len = 2,
        }, CodePage.utf8.codepointAt(0, invalid_utf8).?);
        try std.testing.expectEqual(Codepoint{
            .value = Codepoint.invalid,
            .byte_len = 1,
        }, CodePage.utf8.codepointAt(2, invalid_utf8).?);
        try std.testing.expectEqual(@as(?Codepoint, null), CodePage.windows1252.codepointAt(3, invalid_utf8));
    }
}

test "codepointAt utf8 encoded" {
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

test "codepointAt windows1252 encoded" {
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
