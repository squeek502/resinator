const std = @import("std");
const utils = @import("utils.zig");
const res = @import("res.zig");
const SourceBytes = @import("literals.zig").SourceBytes;

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

    pub fn fromString(bytes: SourceBytes) Resource {
        const maybe_ordinal = res.NameOrOrdinal.maybeOrdinalFromString(bytes);
        if (maybe_ordinal) |ordinal| {
            const rt = @intToEnum(res.RT, ordinal.ordinal);
            return fromRT(rt);
        }
        return map.get(bytes.slice) orelse .user_defined;
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
/// https://learn.microsoft.com/en-us/windows/win32/menurc/dialog-resource#parameters
/// https://learn.microsoft.com/en-us/windows/win32/menurc/dialogex-resource#parameters
pub const OptionalStatements = enum {
    characteristics,
    language,
    version,

    // DIALOG
    caption,
    class,
    exstyle,
    font,
    menu,
    style,

    pub const map = utils.ComptimeCaseInsensitiveStringMap(OptionalStatements, .{
        .{ "CHARACTERISTICS", .characteristics },
        .{ "LANGUAGE", .language },
        .{ "VERSION", .version },
    });

    pub const dialog_map = utils.ComptimeCaseInsensitiveStringMap(OptionalStatements, .{
        .{ "CAPTION", .caption },
        .{ "CLASS", .class },
        .{ "EXSTYLE", .exstyle },
        .{ "FONT", .font },
        .{ "MENU", .menu },
        .{ "STYLE", .style },
    });
};

pub const Control = enum {
    auto3state,
    autocheckbox,
    autoradiobutton,
    checkbox,
    combobox,
    control,
    ctext,
    defpushbutton,
    edittext,
    hedit,
    iedit,
    groupbox,
    icon,
    listbox,
    ltext,
    pushbox,
    pushbutton,
    radiobutton,
    rtext,
    scrollbar,
    state3,
    userbutton,

    pub const map = utils.ComptimeCaseInsensitiveStringMap(Control, .{
        .{ "AUTO3STATE", .auto3state },
        .{ "AUTOCHECKBOX", .autocheckbox },
        .{ "AUTORADIOBUTTON", .autoradiobutton },
        .{ "CHECKBOX", .checkbox },
        .{ "COMBOBOX", .combobox },
        .{ "CONTROL", .control },
        .{ "CTEXT", .ctext },
        .{ "DEFPUSHBUTTON", .defpushbutton },
        .{ "EDITTEXT", .edittext },
        .{ "HEDIT", .hedit },
        .{ "IEDIT", .iedit },
        .{ "GROUPBOX", .groupbox },
        .{ "ICON", .icon },
        .{ "LISTBOX", .listbox },
        .{ "LTEXT", .ltext },
        .{ "PUSHBOX", .pushbox },
        .{ "PUSHBUTTON", .pushbutton },
        .{ "RADIOBUTTON", .radiobutton },
        .{ "RTEXT", .rtext },
        .{ "SCROLLBAR", .scrollbar },
        .{ "STATE3", .state3 },
        .{ "USERBUTTON", .userbutton },
    });

    pub fn hasTextParam(control: Control) bool {
        switch (control) {
            .scrollbar, .listbox, .iedit, .hedit, .edittext, .combobox => return false,
            else => return true,
        }
    }
};

pub const ControlClass = struct {
    pub const map = utils.ComptimeCaseInsensitiveStringMap(res.ControlClass, .{
        .{ "BUTTON", .button },
        .{ "EDIT", .edit },
        .{ "STATIC", .static },
        .{ "LISTBOX", .listbox },
        .{ "SCROLLBAR", .scrollbar },
        .{ "COMBOBOX", .combobox },
    });

    /// Like `map.get` but works on WTF16 strings, for use with parsed
    /// string literals ("BUTTON", or even "\x42UTTON")
    pub fn fromWideString(str: []const u16) ?res.ControlClass {
        const utf16Literal = std.unicode.utf8ToUtf16LeStringLiteral;
        return if (std.os.windows.eqlIgnoreCaseWTF16(str, utf16Literal("BUTTON")))
            .button
        else if (std.os.windows.eqlIgnoreCaseWTF16(str, utf16Literal("EDIT")))
            .edit
        else if (std.os.windows.eqlIgnoreCaseWTF16(str, utf16Literal("STATIC")))
            .static
        else if (std.os.windows.eqlIgnoreCaseWTF16(str, utf16Literal("LISTBOX")))
            .listbox
        else if (std.os.windows.eqlIgnoreCaseWTF16(str, utf16Literal("SCROLLBAR")))
            .scrollbar
        else if (std.os.windows.eqlIgnoreCaseWTF16(str, utf16Literal("COMBOBOX")))
            .combobox
        else
            null;
    }
};

pub const MenuItem = enum {
    menuitem,
    popup,

    pub const map = utils.ComptimeCaseInsensitiveStringMap(MenuItem, .{
        .{ "MENUITEM", .menuitem },
        .{ "POPUP", .popup },
    });

    pub fn isSeparator(bytes: []const u8) bool {
        return std.ascii.eqlIgnoreCase(bytes, "SEPARATOR");
    }

    pub const Option = enum {
        checked,
        grayed,
        help,
        inactive,
        menubarbreak,
        menubreak,

        pub const map = utils.ComptimeCaseInsensitiveStringMap(Option, .{
            .{ "CHECKED", .checked },
            .{ "GRAYED", .grayed },
            .{ "HELP", .help },
            .{ "INACTIVE", .inactive },
            .{ "MENUBARBREAK", .menubarbreak },
            .{ "MENUBREAK", .menubreak },
        });
    };
};

pub const VersionInfo = enum {
    file_version,
    product_version,
    file_flags_mask,
    file_flags,
    file_os,
    file_type,
    file_subtype,

    pub const map = utils.ComptimeCaseInsensitiveStringMap(VersionInfo, .{
        .{ "FILEVERSION", .file_version },
        .{ "PRODUCTVERSION", .product_version },
        .{ "FILEFLAGSMASK", .file_flags_mask },
        .{ "FILEFLAGS", .file_flags },
        .{ "FILEOS", .file_os },
        .{ "FILETYPE", .file_type },
        .{ "FILESUBTYPE", .file_subtype },
    });
};

pub const VersionBlock = enum {
    block,
    value,

    pub const map = utils.ComptimeCaseInsensitiveStringMap(VersionBlock, .{
        .{ "BLOCK", .block },
        .{ "VALUE", .value },
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
