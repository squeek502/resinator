const rc = @import("rc.zig");
const Resource = rc.Resource;
const CommonResourceAttributes = rc.CommonResourceAttributes;

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
            .rcdata => .RCDATA,
            .stringtable => .STRING,
            .versioninfo => .VERSION,
            .user_defined => null,
        };
    }
};

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
            return MemoryFlags{ .value = MOVEABLE | PURE };
        } else {
            return switch (predefined_resource_type.?) {
                .RCDATA, .BITMAP => MemoryFlags{ .value = MOVEABLE | PURE },
                .GROUP_ICON, .GROUP_CURSOR => MemoryFlags{ .value = MOVEABLE | PURE | DISCARDABLE },
                .ICON, .CURSOR => MemoryFlags{ .value = MOVEABLE | DISCARDABLE },
                else => @panic("TODO"),
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
