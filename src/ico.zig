//! https://devblogs.microsoft.com/oldnewthing/20120720-00/?p=7083
//! https://learn.microsoft.com/en-us/previous-versions/ms997538(v=msdn.10)
//! https://learn.microsoft.com/en-us/windows/win32/menurc/newheader
//! https://learn.microsoft.com/en-us/windows/win32/menurc/resdir
//! https://learn.microsoft.com/en-us/windows/win32/menurc/localheader

const std = @import("std");

pub fn read(allocator: std.mem.Allocator, reader: anytype) !IconDir {
    const reserved = try reader.readIntLittle(u16);
    if (reserved != 0) {
        return error.InvalidHeader;
    }

    const image_type = reader.readEnum(ImageType, .Little) catch |err| switch (err) {
        error.InvalidValue => return error.InvalidImageType,
        else => |e| return e,
    };

    const num_images = try reader.readIntLittle(u16);

    // To avoid over-allocation in the case of a file that says it has way more
    // entries than it actually does, we use an ArrayList with a conservatively
    // limited initial capacity instead of allocating the entire slice at once.
    const initial_capacity = @min(num_images, 8);
    var entries = try std.ArrayList(Entry).initCapacity(allocator, initial_capacity);
    errdefer entries.deinit();

    var i: usize = 0;
    while (i < num_images) : (i += 1) {
        var entry: Entry = undefined;
        entry.width = try reader.readByte();
        entry.height = try reader.readByte();
        entry.num_colors = try reader.readByte();
        _ = try reader.readByte(); // reserved
        switch (image_type) {
            .icon => {
                entry.type_specific_data = .{ .icon = .{
                    .color_planes = try reader.readIntLittle(u16),
                    .bits_per_pixel = try reader.readIntLittle(u16),
                } };
            },
            .cursor => {
                entry.type_specific_data = .{ .cursor = .{
                    .hotspot_x = try reader.readIntLittle(u16),
                    .hotspot_y = try reader.readIntLittle(u16),
                } };
            },
        }
        entry.data_size_in_bytes = try reader.readIntLittle(u32);
        entry.data_offset_from_start_of_file = try reader.readIntLittle(u32);
        try entries.append(entry);
    }

    return .{
        .image_type = image_type,
        .entries = try entries.toOwnedSlice(),
        .allocator = allocator,
    };
}

pub const ImageType = enum(u16) {
    icon = 1,
    cursor = 2,
};

pub const IconDir = struct {
    image_type: ImageType,
    entries: []Entry,
    allocator: std.mem.Allocator,

    pub fn deinit(self: IconDir) void {
        self.allocator.free(self.entries);
    }

    pub fn writeResData(self: IconDir, writer: anytype, first_image_id: u16) !void {
        try writer.writeIntLittle(u16, 0);
        try writer.writeIntLittle(u16, @enumToInt(self.image_type));
        try writer.writeIntLittle(u16, @intCast(u16, self.entries.len));

        var image_id = first_image_id;
        for (self.entries) |entry| {
            try entry.writeResData(writer, image_id);
            image_id += 1;
        }
    }
};

pub const Entry = struct {
    // Icons are limited to u8 sizes, cursors can have u16,
    // so we store as u16 and truncate when needed.
    width: u16,
    height: u16,
    num_colors: u8,
    type_specific_data: union(ImageType) {
        icon: struct {
            color_planes: u16,
            bits_per_pixel: u16,
        },
        cursor: struct {
            hotspot_x: u16,
            hotspot_y: u16,
        },
    },
    data_size_in_bytes: u32,
    data_offset_from_start_of_file: u32,

    pub fn writeResData(self: Entry, writer: anytype, id: u16) !void {
        switch (self.type_specific_data) {
            .icon => |icon_data| {
                try writer.writeIntLittle(u8, @truncate(u8, self.width));
                try writer.writeIntLittle(u8, @truncate(u8, self.height));
                try writer.writeIntLittle(u8, self.num_colors);
                try writer.writeIntLittle(u8, 0); // reserved
                try writer.writeIntLittle(u16, icon_data.color_planes);
                try writer.writeIntLittle(u16, icon_data.bits_per_pixel);
                try writer.writeIntLittle(u32, self.data_size_in_bytes);
            },
            .cursor => |cursor_data| {
                try writer.writeIntLittle(u16, self.width);
                try writer.writeIntLittle(u16, self.height);
                try writer.writeIntLittle(u16, cursor_data.hotspot_x);
                try writer.writeIntLittle(u16, cursor_data.hotspot_y);
                try writer.writeIntLittle(u32, self.data_size_in_bytes + 4);
            },
        }
        try writer.writeIntLittle(u16, id);
    }
};

test "icon" {
    var fbs = std.io.fixedBufferStream("\x00\x00\x01\x00\x03\x00\x10\x10\x00\x00\x01\x00\x20\x00\x68\x04\x00\x00\x36\x00\x00\x00\x20\x20\x00\x00\x01\x00\x20\x00\xA8\x10\x00\x00\x9E\x04\x00\x00\x30\x30\x00\x00\x01\x00\x20\x00\xA8\x25\x00\x00\x46\x15\x00\x00");
    const icon = try read(std.testing.allocator, fbs.reader());
    defer icon.deinit();

    try std.testing.expectEqual(ImageType.icon, icon.image_type);
    try std.testing.expectEqual(@as(usize, 3), icon.entries.len);
}

/// From WinGDI.h
pub const BITMAPCOREHEADER = extern struct {
    bcSize: u32,
    bcWidth: i32,
    bcHeight: i32,
    bcPlanes: u16,
    bcBitCount: u16,

    pub fn isPng(self: *const BITMAPCOREHEADER) bool {
        const png_signature = "\x89PNG\r\n\x1a\n";
        return std.mem.startsWith(u8, std.mem.asBytes(self), png_signature);
    }

    pub fn isRiff(self: *const BITMAPCOREHEADER) bool {
        return std.mem.startsWith(u8, std.mem.asBytes(self), "RIFF");
    }
};
