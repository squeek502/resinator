const std = @import("std");

pub const Resource = enum {
    accelerators,
    bitmap,
    cursor,
    dialog,
    dialogex,
    font,
    html,
    icon,
    menu,
    menuex,
    messagetable,
    popup,
    //plugplay, // Obsolete
    rcdata,
    stringtable,
    //textinclude, // A special resource that is interpreted by Visual C++.
    //typelib, // A special resource that is used with the /TLBID and /TLBOUT linker options
    user_defined,
    versioninfo,
    //vxd, // Obsolete

    const map = std.ComptimeStringMap(Resource, .{
        .{ "ACCELERATORS", .accelerators },
        .{ "BITMAP", .bitmap },
        .{ "CURSOR", .cursor },
        .{ "DIALOG", .dialog },
        .{ "DIALOGEX", .dialogex },
        .{ "FONT", .font },
        .{ "HTML", .html },
        .{ "ICON", .icon },
        .{ "MENU", .menu },
        .{ "MENUEX", .menuex },
        .{ "MESSAGETABLE", .messagetable },
        .{ "POPUP", .popup },
        .{ "RCDATA", .rcdata },
        .{ "STRINGTABLE", .stringtable },
        .{ "VERSIONINFO", .versioninfo },
    });

    pub fn fromString(str: []const u8) Resource {
        return map.get(str) orelse .user_defined;
    }
};
