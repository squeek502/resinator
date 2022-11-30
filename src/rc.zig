const std = @import("std");
const utils = @import("utils.zig");

// https://learn.microsoft.com/en-us/windows/win32/menurc/about-resource-files

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

    const map = utils.ComptimeCaseInsensitiveStringMap(Resource, .{
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

/// https://learn.microsoft.com/en-us/windows/win32/menurc/stringtable-resource#parameters
pub const OptionalStatements = enum {
    characteristics,
    language,
    version,

    pub const map = utils.ComptimeCaseInsensitiveStringMap(OptionalStatements, .{
        .{ "CHARACTERISTICS", .characteristics },
        .{ "LANGUAGE", .language },
        .{ "VERSION", .version },
    });
};

/// Keywords that are be the first token in a statement and (if so) dictate how the rest
/// of the statement is parsed.
pub const TopLevelKeywords = enum {
    language,
    stringtable,

    pub const map = utils.ComptimeCaseInsensitiveStringMap(TopLevelKeywords, .{
        .{ "LANGUAGE", .language },
        .{ "STRINGTABLE", .stringtable },
    });
};

pub const CommonResourceAttributes = enum {
    preload,
    loadoncall,
    fixed,
    moveable,
    discardable,
    pure,
    impure,
    shared,
    nonshared,

    pub const map = utils.ComptimeCaseInsensitiveStringMap(CommonResourceAttributes, .{
        .{ "PRELOAD", .preload },
        .{ "LOADONCALL", .loadoncall },
        .{ "FIXED", .fixed },
        .{ "MOVEABLE", .moveable },
        .{ "DISCARDABLE", .discardable },
        .{ "PURE", .pure },
        .{ "IMPURE", .impure },
        .{ "SHARED", .shared },
        .{ "NONSHARED", .nonshared },
    });
};
