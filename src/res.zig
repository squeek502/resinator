const std = @import("std");
const rc = @import("rc.zig");
const Resource = rc.Resource;
const CommonResourceAttributes = rc.CommonResourceAttributes;
const Allocator = std.mem.Allocator;
const windows1252 = @import("windows1252.zig");
const CodePage = @import("code_pages.zig").CodePage;
const SourceBytes = @import("literals.zig").SourceBytes;

/// https://learn.microsoft.com/en-us/windows/win32/menurc/resource-types
pub const RT = enum(u8) {
    ACCELERATOR = 9,
    ANICURSOR = 21,
    ANIICON = 22,
    BITMAP = 2,
    CURSOR = 1,
    DIALOG = 5,
    DLGINCLUDE = 17,
    FONT = 8,
    FONTDIR = 7,
    GROUP_CURSOR = 1 + 11, // CURSOR + 11
    GROUP_ICON = 3 + 11, // ICON + 11
    HTML = 23,
    ICON = 3,
    MANIFEST = 24,
    MENU = 4,
    MESSAGETABLE = 11,
    PLUGPLAY = 19,
    RCDATA = 10,
    STRING = 6,
    VERSION = 16,
    VXD = 20,
    _,

    /// Returns null if the resource type doesn't have a 1:1 mapping with an RT constant
    pub fn fromResource(resource: Resource) ?RT {
        return switch (resource) {
            .accelerators => .ACCELERATOR,
            .bitmap => .BITMAP,
            .cursor => .GROUP_CURSOR,
            .dialog => .DIALOG,
            .dialogex => null, // TODO: ?
            .font => .FONT,
            .html => .HTML,
            .icon => .GROUP_ICON,
            .menu => .MENU,
            .menuex => null, // TODO: ?
            .messagetable => .MESSAGETABLE,
            .popup => null,
            .plugplay => .PLUGPLAY,
            .rcdata => .RCDATA,
            .stringtable => null, // TODO: Maybe unreachable?
            .user_defined => null,
            .versioninfo => .VERSION,
            .vxd => .VXD,

            .cursor_num => .CURSOR,
            .icon_num => .ICON,
            .string_num => .STRING,
            .anicursor_num => .ANICURSOR,
            .aniicon_num => .ANIICON,
            .dlginclude_num => .DLGINCLUDE,
            .fontdir_num => .FONTDIR,
            .manifest_num => .MANIFEST,
        };
    }
};

/// https://learn.microsoft.com/en-us/windows/win32/menurc/common-resource-attributes
/// https://learn.microsoft.com/en-us/windows/win32/menurc/resourceheader
pub const MemoryFlags = packed struct(u16) {
    value: u16,

    pub const MOVEABLE: u16 = 0x10;
    // TODO: SHARED and PURE seem to be the same thing? Testing seems to confirm this but
    //       would like to find mention of it somewhere.
    pub const SHARED: u16 = 0x20;
    pub const PURE: u16 = 0x20;
    pub const PRELOAD: u16 = 0x40;
    pub const DISCARDABLE: u16 = 0x1000;

    /// Note: The defaults can have combinations that are not possible to specify within
    ///       an .rc file, as the .rc attributes imply other values (i.e. specifying
    ///       DISCARDABLE always implies MOVEABLE and PURE/SHARED, and yet RT_ICON
    ///       has a default of only MOVEABLE | DISCARDABLE).
    pub fn defaults(predefined_resource_type: ?RT) MemoryFlags {
        if (predefined_resource_type == null) {
            return MemoryFlags{ .value = MOVEABLE | SHARED };
        } else {
            return switch (predefined_resource_type.?) {
                .RCDATA, .BITMAP, .HTML, .MANIFEST, .ACCELERATOR => MemoryFlags{ .value = MOVEABLE | SHARED },
                .GROUP_ICON, .GROUP_CURSOR, .STRING, .FONT => MemoryFlags{ .value = MOVEABLE | SHARED | DISCARDABLE },
                .ICON, .CURSOR => MemoryFlags{ .value = MOVEABLE | DISCARDABLE },
                .FONTDIR => MemoryFlags{ .value = MOVEABLE | PRELOAD },
                else => {
                    std.debug.print("TODO: {}\n", .{predefined_resource_type.?});
                    @panic("TODO");
                },
            };
        }
    }

    pub fn set(self: *MemoryFlags, attribute: CommonResourceAttributes) void {
        switch (attribute) {
            .preload => self.value |= PRELOAD,
            .loadoncall => self.value &= ~PRELOAD,
            .moveable => self.value |= MOVEABLE,
            .fixed => self.value &= ~(MOVEABLE | DISCARDABLE),
            .shared => self.value |= SHARED,
            .nonshared => self.value &= ~(SHARED | DISCARDABLE),
            .pure => self.value |= PURE,
            .impure => self.value &= ~(PURE | DISCARDABLE),
            .discardable => self.value |= DISCARDABLE | MOVEABLE | PURE,
        }
    }
};

/// https://learn.microsoft.com/en-us/windows/win32/intl/language-identifiers
pub const Language = packed struct(u16) {
    // TODO: Are these defaults dependent on the system's language setting at the time
    //       that the RC compiler is run?
    primary_language_id: u10 = 0x09, // LANG_ENGLISH
    sublanguage_id: u6 = 0x01, // SUBLANG_ENGLISH_US (since primary is ENGLISH)
};

pub const NameOrOrdinal = union(enum) {
    name: [:0]const u16,
    ordinal: u16,

    pub fn deinit(self: NameOrOrdinal, allocator: Allocator) void {
        switch (self) {
            .name => |name| {
                allocator.free(name);
            },
            .ordinal => {},
        }
    }

    /// Returns the full length of the amount of bytes that would be written by `write`
    /// (e.g. for an ordinal it will return the length including the 0xFFFF indicator)
    pub fn byteLen(self: NameOrOrdinal) u32 {
        switch (self) {
            .name => |name| {
                // + 1 for 0-terminated, * 2 for bytes per u16
                return @intCast(u32, (name.len + 1) * 2);
            },
            .ordinal => return 4,
        }
    }

    pub fn write(self: NameOrOrdinal, writer: anytype) !void {
        switch (self) {
            .name => |name| {
                try writer.writeAll(std.mem.sliceAsBytes(name[0 .. name.len + 1]));
            },
            .ordinal => |ordinal| {
                try writer.writeIntLittle(u16, 0xffff);
                try writer.writeIntLittle(u16, ordinal);
            },
        }
    }

    pub fn fromString(allocator: Allocator, bytes: SourceBytes) !NameOrOrdinal {
        if (maybeOrdinalFromString(bytes)) |ordinal| {
            return ordinal;
        }
        return nameFromString(allocator, bytes);
    }

    pub fn nameFromString(allocator: Allocator, bytes: SourceBytes) !NameOrOrdinal {
        // TODO use bytes.code_page
        var as_utf16 = try std.unicode.utf8ToUtf16LeWithNull(allocator, bytes.slice);
        // Names have a limit of 256 UTF-16 code units + null terminator
        // Note: This can cut-off in the middle of a UTF-16, i.e. it can make the
        //       string end with an unpaired high surrogate
        if (as_utf16.len > 256) {
            var limited = allocator.shrink(as_utf16, 257);
            limited[256] = 0;
            as_utf16 = limited[0..256 :0];
        }
        // ASCII chars in names are always converted to uppercase
        for (as_utf16) |*char| {
            if (char.* < 128) {
                char.* = std.ascii.toUpper(@intCast(u8, char.*));
            }
        }
        return NameOrOrdinal{ .name = as_utf16 };
    }

    pub fn maybeOrdinalFromString(bytes: SourceBytes) ?NameOrOrdinal {
        var buf = bytes.slice;
        var radix: u8 = 10;
        if (buf.len > 2 and buf[0] == '0') {
            switch (buf[1]) {
                '0'...'9' => {},
                'x', 'X' => {
                    radix = 16;
                    buf = buf[2..];
                    // only the first 4 hex digits matter, anything else is ignored
                    // i.e. 0x12345 is treated as if it were 0x1234
                    buf.len = @min(buf.len, 4);
                },
                else => return null,
            }
        }

        var i: usize = 0;
        var result: u16 = 0;
        while (bytes.code_page.codepointAt(i, buf)) |codepoint| : (i += codepoint.byte_len) {
            const c = codepoint.value;
            const digit = switch (c) {
                // I have no idea why this is the case, but the Windows RC compiler
                // treats ¹, ², and ³ characters as valid digits when the radix is 10
                '¹', '²', '³' => if (radix != 10) break else @intCast(u8, c) - 0x30,
                0x00...0x7F => std.fmt.charToDigit(@intCast(u8, c), radix) catch switch (radix) {
                    10 => return null,
                    // non-hex-digits are treated as a terminator rather than invalidating
                    // the number (note: if there are no valid hex digits then the result
                    // will be zero which is not treated as a valid number)
                    16 => break,
                    else => unreachable,
                },
                else => break,
            };

            if (result != 0) {
                result *%= radix;
            }
            result +%= digit;
        }

        // Anything that resolves to zero is not interpretted as a number
        if (result == 0) return null;
        return NameOrOrdinal{ .ordinal = result };
    }

    pub fn predefinedResourceType(self: NameOrOrdinal) ?RT {
        switch (self) {
            .ordinal => |ordinal| {
                switch (@intToEnum(RT, ordinal)) {
                    .ACCELERATOR,
                    .ANICURSOR,
                    .ANIICON,
                    .BITMAP,
                    .CURSOR,
                    .DIALOG,
                    .DLGINCLUDE,
                    .FONT,
                    .FONTDIR,
                    .GROUP_CURSOR,
                    .GROUP_ICON,
                    .HTML,
                    .ICON,
                    .MANIFEST,
                    .MENU,
                    .MESSAGETABLE,
                    .PLUGPLAY,
                    .RCDATA,
                    .STRING,
                    .VERSION,
                    .VXD,
                    => |rt| return rt,
                    _ => return null,
                }
            },
            .name => return null,
        }
    }
};

fn expectNameOrOrdinal(expected: NameOrOrdinal, actual: NameOrOrdinal) !void {
    switch (expected) {
        .name => {
            if (actual != .name) return error.TestExpectedEqual;
            try std.testing.expectEqualSlices(u16, expected.name, actual.name);
        },
        .ordinal => {
            if (actual != .ordinal) return error.TestExpectedEqual;
            try std.testing.expectEqual(expected.ordinal, actual.ordinal);
        },
    }
}

test "NameOrOrdinal" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    // zero is treated as a string
    try expectNameOrOrdinal(
        NameOrOrdinal{ .name = std.unicode.utf8ToUtf16LeStringLiteral("0") },
        try NameOrOrdinal.fromString(allocator, .{ .slice = "0", .code_page = .windows1252 }),
    );
    // same with overflow that resolves to 0
    try expectNameOrOrdinal(
        NameOrOrdinal{ .name = std.unicode.utf8ToUtf16LeStringLiteral("65536") },
        try NameOrOrdinal.fromString(allocator, .{ .slice = "65536", .code_page = .windows1252 }),
    );
    // hex zero is also treated as a string
    try expectNameOrOrdinal(
        NameOrOrdinal{ .name = std.unicode.utf8ToUtf16LeStringLiteral("0X0") },
        try NameOrOrdinal.fromString(allocator, .{ .slice = "0x0", .code_page = .windows1252 }),
    );
    // hex numbers work
    try expectNameOrOrdinal(
        NameOrOrdinal{ .ordinal = 0x100 },
        try NameOrOrdinal.fromString(allocator, .{ .slice = "0x100", .code_page = .windows1252 }),
    );
    // only the first 4 hex digits matter
    try expectNameOrOrdinal(
        NameOrOrdinal{ .ordinal = 0x1234 },
        try NameOrOrdinal.fromString(allocator, .{ .slice = "0X12345", .code_page = .windows1252 }),
    );
    // octal is not supported so it gets treated as a string
    try expectNameOrOrdinal(
        NameOrOrdinal{ .name = std.unicode.utf8ToUtf16LeStringLiteral("0O1234") },
        try NameOrOrdinal.fromString(allocator, .{ .slice = "0o1234", .code_page = .windows1252 }),
    );
    // overflow wraps
    try expectNameOrOrdinal(
        NameOrOrdinal{ .ordinal = @truncate(u16, 65635) },
        try NameOrOrdinal.fromString(allocator, .{ .slice = "65635", .code_page = .windows1252 }),
    );
    // non-hex-digits in a hex literal are treated as a terminator
    try expectNameOrOrdinal(
        NameOrOrdinal{ .ordinal = 0x4 },
        try NameOrOrdinal.fromString(allocator, .{ .slice = "0x4n", .code_page = .windows1252 }),
    );
    try expectNameOrOrdinal(
        NameOrOrdinal{ .ordinal = 0xFA },
        try NameOrOrdinal.fromString(allocator, .{ .slice = "0xFAZ92348", .code_page = .windows1252 }),
    );
    // 0 at the start is allowed
    try expectNameOrOrdinal(
        NameOrOrdinal{ .ordinal = 50 },
        try NameOrOrdinal.fromString(allocator, .{ .slice = "050", .code_page = .windows1252 }),
    );
    // limit of 256 UTF-16 code units, can cut off between a surrogate pair
    {
        var expected = blk: {
            // the input before the 𐐷 character, but uppercased
            var expected_u8_bytes = "00614982008907933748980730280674788429543776231864944218790698304852300002973622122844631429099469274282385299397783838528QFFL7SHNSIETG0QKLR1UYPBTUV1PMFQRRA0VJDG354GQEDJMUPGPP1W1EXVNTZVEIZ6K3IPQM1AWGEYALMEODYVEZGOD3MFMGEY8FNR4JUETTB1PZDEWSNDRGZUA8SNXP3NGO";
            var buf: [256:0]u16 = undefined;
            for (expected_u8_bytes) |byte, i| {
                buf[i] = byte;
            }
            // surrogate pair that is now orphaned
            buf[255] = 0xD801;
            break :blk buf;
        };
        try expectNameOrOrdinal(
            NameOrOrdinal{ .name = &expected },
            try NameOrOrdinal.fromString(allocator, .{
                .slice = "00614982008907933748980730280674788429543776231864944218790698304852300002973622122844631429099469274282385299397783838528qffL7ShnSIETg0qkLr1UYpbtuv1PMFQRRa0VjDG354GQedJmUPgpp1w1ExVnTzVEiz6K3iPqM1AWGeYALmeODyvEZGOD3MfmGey8fnR4jUeTtB1PzdeWsNDrGzuA8Snxp3NGO𐐷",
                .code_page = .utf8,
            }),
        );
    }
}

/// https://learn.microsoft.com/en-us/windows/win32/api/winuser/ns-winuser-accel#members
/// https://devblogs.microsoft.com/oldnewthing/20070316-00/?p=27593
pub const AcceleratorModifiers = packed struct(u8) {
    value: u8 = 0,

    pub const ASCII = 0;
    pub const VIRTKEY = 1;
    pub const NOINVERT = 1 << 1;
    pub const SHIFT = 1 << 2;
    pub const CONTROL = 1 << 3;
    pub const ALT = 1 << 4;
    /// Marker for the last accelerator in an accelerator table
    pub const last_accelerator_in_table = 1 << 7;

    pub fn apply(self: *AcceleratorModifiers, modifier: rc.AcceleratorTypeAndOptions) void {
        self.value |= modifierValue(modifier);
    }

    pub fn isSet(self: AcceleratorModifiers, modifier: rc.AcceleratorTypeAndOptions) bool {
        // ASCII is set whenever VIRTKEY is not
        if (modifier == .ascii) return self.value & modifierValue(.virtkey) == 0;
        return self.value & modifierValue(modifier) != 0;
    }

    fn modifierValue(modifier: rc.AcceleratorTypeAndOptions) u8 {
        return switch (modifier) {
            .ascii => ASCII,
            .virtkey => VIRTKEY,
            .noinvert => NOINVERT,
            .shift => SHIFT,
            .control => CONTROL,
            .alt => ALT,
        };
    }

    pub fn markLast(self: *AcceleratorModifiers) void {
        self.value |= last_accelerator_in_table;
    }
};

/// Expects the input to be a parsed string literal
/// TODO: Codepage-aware handling of certain characters
pub fn parseAcceleratorKeyString(str: []const u8, is_virt: bool) !u16 {
    if (str.len == 0 or str.len > 2) {
        return error.InvalidAccelerator;
    }

    if (str[0] == '^') {
        if (str.len == 1) return error.InvalidControlCharacter;
        // TODO: This should be handled differently if the code page is not 1252
        const c = str[1];
        const codepoint = windows1252.toCodepoint(c);
        switch (codepoint) {
            '^' => return '^', // special case
            'a'...'z', 'A'...'Z' => return std.ascii.toLower(c) - 'a' + 1,
            // For some reason the Windows-1252 characters that have codepoints less
            // than 0x0200 but greater than 0xFF get converted like so when used in a
            // ^<char> sequence.
            // TODO: Understand this better?
            'ƒ', 'Š', 'Œ', 'Ž', 'š', 'œ', 'ž', 'Ÿ' => return codepoint - 0x40,
            else => return error.ControlCharacterOutOfRange,
        }
    }

    // TODO: This should be handled differently if the code page is not 1252 (see test case)
    var result: u16 = windows1252.toCodepoint(str[0]);
    if (str.len == 2) {
        result <<= 8;
        result += windows1252.toCodepoint(str[1]);
    } else if (is_virt) {
        switch (result) {
            'a'...'z' => result -= 0x20, // toUpper
            else => {},
        }
    }
    return result;
}

test "accelerator keys" {
    try std.testing.expectEqual(@as(u16, 1), try parseAcceleratorKeyString("^a", false));
    try std.testing.expectEqual(@as(u16, 1), try parseAcceleratorKeyString("^A", false));
    try std.testing.expectEqual(@as(u16, 26), try parseAcceleratorKeyString("^Z", false));
    try std.testing.expectEqual(@as(u16, '^'), try parseAcceleratorKeyString("^^", false));

    try std.testing.expectEqual(@as(u16, 'a'), try parseAcceleratorKeyString("a", false));
    try std.testing.expectEqual(@as(u16, 0x6162), try parseAcceleratorKeyString("ab", false));

    try std.testing.expectEqual(@as(u16, 'C'), try parseAcceleratorKeyString("c", true));
    try std.testing.expectEqual(@as(u16, 0x6363), try parseAcceleratorKeyString("cc", true));

    // \x80 is € in Windows-1252, which is Unicode codepoint 20AC
    // This depends on the code page, though, with codepage 65001, \x80 is parsed as 0x80
    try std.testing.expectEqual(@as(u16, 0x20AC), try parseAcceleratorKeyString("\x80", false));
    try std.testing.expectEqual(@as(u16, 0xCCAC), try parseAcceleratorKeyString("\x80\x80", false));

    // \x83 is ƒ in Windows-1252, which is Unicode codepoint 0192; 0x0192 - 0x40 = 0x0152
    try std.testing.expectEqual(@as(u16, 0x0152), try parseAcceleratorKeyString("^\x83", false));

    try std.testing.expectError(error.ControlCharacterOutOfRange, parseAcceleratorKeyString("^1", false));
    try std.testing.expectError(error.InvalidControlCharacter, parseAcceleratorKeyString("^", false));
    try std.testing.expectError(error.InvalidAccelerator, parseAcceleratorKeyString("", false));
    try std.testing.expectError(error.InvalidAccelerator, parseAcceleratorKeyString("hello", false));
    try std.testing.expectError(error.ControlCharacterOutOfRange, parseAcceleratorKeyString("^\x80", false));
}
