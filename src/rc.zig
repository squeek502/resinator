const std = @import("std");
const utils = @import("utils.zig");
const res = @import("res.zig");

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
    plugplay, // Obsolete
    rcdata,
    stringtable,
    //textinclude, // A special resource that is interpreted by Visual C++.
    //typelib, // A special resource that is used with the /TLBID and /TLBOUT linker options
    user_defined,
    versioninfo,
    vxd, // Obsolete

    // Types that can only be specified by numbers, they don't have keywords
    cursor_num,
    icon_num,
    string_num,
    anicursor_num,
    aniicon_num,
    dlginclude_num,
    fontdir_num,
    manifest_num,

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
        .{ "PLUGPLAY", .plugplay },
        .{ "RCDATA", .rcdata },
        .{ "STRINGTABLE", .stringtable },
        .{ "VERSIONINFO", .versioninfo },
        .{ "VXD", .vxd },
    });

    pub fn fromString(str: []const u8) Resource {
        const maybe_ordinal = res.NameOrOrdinal.maybeOrdinalFromString(str);
        if (maybe_ordinal) |ordinal| {
            const rt = @intToEnum(res.RT, ordinal.ordinal);
            return fromRT(rt);
        }
        return map.get(str) orelse .user_defined;
    }

    // TODO: Some comptime validation that RT <-> Resource conversion is synced?
    pub fn fromRT(rt: res.RT) Resource {
        return switch (rt) {
            .ACCELERATOR => .accelerators,
            .ANICURSOR => .anicursor_num,
            .ANIICON => .aniicon_num,
            .BITMAP => .bitmap,
            .CURSOR => .cursor_num,
            .DIALOG => .dialog,
            .DLGINCLUDE => .dlginclude_num,
            .FONT => .font,
            .FONTDIR => .fontdir_num,
            .GROUP_CURSOR => .cursor,
            .GROUP_ICON => .icon,
            .HTML => .html,
            .ICON => .icon_num,
            .MANIFEST => .manifest_num,
            .MENU => .menu,
            .MESSAGETABLE => .messagetable,
            .PLUGPLAY => .plugplay,
            .RCDATA => .rcdata,
            .STRING => .string_num,
            .VERSION => .versioninfo,
            .VXD => .vxd,
            _ => .user_defined,
        };
    }

    pub fn canUseRawData(resource: Resource) bool {
        return switch (resource) {
            .user_defined,
            .html,
            .plugplay, // Obsolete
            .rcdata,
            .vxd, // Obsolete
            .manifest_num,
            => true,
            else => false,
        };
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

pub const AcceleratorTypeAndOptions = enum {
    virtkey,
    ascii,
    noinvert,
    alt,
    shift,
    control,

    pub const map = utils.ComptimeCaseInsensitiveStringMap(AcceleratorTypeAndOptions, .{
        .{ "VIRTKEY", .virtkey },
        .{ "ASCII", .ascii },
        .{ "NOINVERT", .noinvert },
        .{ "ALT", .alt },
        .{ "SHIFT", .shift },
        .{ "CONTROL", .control },
    });
};
