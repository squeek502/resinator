const std = @import("std");
const rc = @import("rc.zig");
const Resource = rc.Resource;
const CommonResourceAttributes = rc.CommonResourceAttributes;
const Allocator = std.mem.Allocator;

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
                .RCDATA, .BITMAP, .HTML, .MANIFEST => MemoryFlags{ .value = MOVEABLE | SHARED },
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

    pub fn fromString(allocator: Allocator, str: []const u8) !NameOrOrdinal {
        if (maybeOrdinalFromString(str)) |ordinal| {
            return ordinal;
        }
        return nameFromString(allocator, str);
    }

    pub fn nameFromString(allocator: Allocator, str: []const u8) !NameOrOrdinal {
        var as_utf16 = try std.unicode.utf8ToUtf16LeWithNull(allocator, str);
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

    pub fn maybeOrdinalFromString(str: []const u8) ?NameOrOrdinal {
        var buf = str;
        var radix: u8 = 10;
        if (buf.len > 2 and buf[0] == '0') {
            switch (str[1]) {
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

        var result: u16 = 0;
        for (buf) |c| {
            const digit = std.fmt.charToDigit(c, radix) catch switch (radix) {
                10 => return null,
                // If this is hex, then non-hex-digits are treated as a terminator rather
                // than an invalid number
                16 => break,
                else => unreachable,
            };

            if (result != 0) {
                if (@mulWithOverflow(u16, result, radix, &result)) return null;
            }
            if (@addWithOverflow(u16, result, digit, &result)) return null;
        }

        // Zero is not interpretted as a number
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
