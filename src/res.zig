const Resource = @import("rc.zig").Resource;

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
            .cursor => .CURSOR,
            .dialog => .DIALOG,
            .dialogex => null, // TODO: ?
            .font => .FONT,
            .html => .HTML,
            .icon => .ICON,
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
