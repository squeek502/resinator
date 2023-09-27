//! https://learn.microsoft.com/en-us/windows/win32/menurc/fontdirentry
//! https://web.archive.org/web/20041114153219/http://www.csn.ul.ie/~caolan/publink/winresdump/winresdump/doc/resfmt.txt
//! http://www.vsoft.nl/software/utils/win/fontedit/
//! https://www.os2museum.com/files/docs/win10sdk/windows-1.03-sdk-prgref-1986.pdf
//! https://web.archive.org/web/20080115184921/http://support.microsoft.com/kb/65123
//! https://www.betaarchive.com/wiki/index.php?title=Microsoft_KB_Archive/65123
//! http://justsolve.archiveteam.org/wiki/FNT_(Windows_Font)
//! http://justsolve.archiveteam.org/wiki/FON

// TODO: This is currently entirely unused. A possible use would be to add
//       something like a --fon-compat option which would switch to writing
//       the FONTDIR as if it were the 16-bit rc.exe to potentially be able to emit
//       a byte-for-byte identical (or close to it) .FON file but on a modern
//       system.
//
//       Note, however, that this byte-for-byte identical-ness would be purely
//       cosmetic in nature, since Windows doesn't seem to care about the FONTDIR
//       resource at all anymore.

const std = @import("std");

pub const ReadError = std.mem.Allocator.Error || error{ InvalidHeader, ImpossibleNameOffset, OverlappingNames, ImpossibleDataSize, UnexpectedEOF, ReadError };

pub fn read(allocator: std.mem.Allocator, reader: anytype, max_size: u64) ReadError!Font {
    // Some Reader implementations have an empty ReadError error set which would
    // cause 'unreachable else' if we tried to use an else in the switch, so we
    // need to detect this case and not try to translate to ReadError
    const empty_reader_errorset = @typeInfo(@TypeOf(reader).Error).ErrorSet == null or @typeInfo(@TypeOf(reader).Error).ErrorSet.?.len == 0;
    if (empty_reader_errorset) {
        return readAnyError(allocator, reader, max_size) catch |err| switch (err) {
            error.EndOfStream, error.StreamTooLong => error.UnexpectedEOF,
            else => |e| return e,
        };
    } else {
        return readAnyError(allocator, reader, max_size) catch |err| switch (err) {
            error.OutOfMemory,
            error.InvalidHeader,
            error.ImpossibleNameOffset,
            error.ImpossibleDataSize,
            error.OverlappingNames,
            => |e| return e,
            error.EndOfStream, error.StreamTooLong => error.UnexpectedEOF,
            // The remaining errors are dependent on the `reader`, so
            // we just translate them all to generic ReadError
            else => error.ReadError,
        };
    }
}

// TODO: This seems like a somewhat strange pattern, could be a better way
//       to do this. Maybe it makes more sense to handle the translation
//       at the call site instead of having a helper function here.
pub fn readAnyError(allocator: std.mem.Allocator, reader: anytype, max_size: u64) !Font {
    var font = Font{
        .entry = try FontDirEntry.read(reader),
        .allocator = allocator,
    };
    errdefer font.deinit();

    var cur_offset: usize = FontDirEntry.len;

    // This is not technically a requirement, but all known specified version fields
    // will have a NUL first byte (0x0100, 0x0200, 0x0300 are the known possible versions).
    // Anything with a non-NUL first byte is almost certainly not a valid .FNT file.
    if (font.entry.version & 255 != 0) return error.InvalidHeader;
    if (font.entry.size > max_size) return error.ImpossibleDataSize;
    // It's seemingly allowed to have these offsets be within the .FNT header, but
    // it's unlikely that would be intentional, and it would make this particular
    // implementation a bit more complicated (since we'd have to backtrack and
    // read part of the .FNT header again to interpret it as a NUL-terminated
    // string) so it's treated as an error condition here.
    if (font.entry.device_offset >= max_size or (font.entry.device_offset != 0 and font.entry.device_offset < cur_offset)) return error.ImpossibleNameOffset;
    if (font.entry.face_offset >= max_size or (font.entry.face_offset != 0 and font.entry.face_offset < cur_offset)) return error.ImpossibleNameOffset;

    const first_offset = @min(font.entry.device_offset, font.entry.face_offset);
    if (first_offset != 0) {
        const offset = first_offset - cur_offset;
        if (font.entry.device_offset <= font.entry.face_offset) {
            font.device_name = try readName(allocator, reader, offset, max_size - first_offset);
        } else {
            font.face_name = try readName(allocator, reader, offset, max_size - first_offset);
        }
        cur_offset += offset + font.device_name.len + 1;
    }
    const second_offset = @max(font.entry.device_offset, font.entry.face_offset);
    if (second_offset != 0) {
        // Similar to the ImpossibleNameOffset error above, this is mostly an error just
        // because it would make the implementation a bit more complicated. Overlapped
        // name strings are extremely unlikely to be present in a valid .FNT file, so it
        // doesn't seem worth it to try to handle them.
        if (second_offset < cur_offset) return error.OverlappingNames;
        const offset = second_offset - cur_offset;
        if (font.entry.device_offset <= font.entry.face_offset) {
            font.face_name = try readName(allocator, reader, offset, max_size - second_offset);
        } else {
            font.device_name = try readName(allocator, reader, offset, max_size - second_offset);
        }
        cur_offset += offset + font.device_name.len + 1;
    }

    return font;
}

fn readName(allocator: std.mem.Allocator, reader: anytype, offset: u64, max_size: u64) ![]const u8 {
    // TODO: Use file cursor moving instead of skipBytes here
    try reader.skipBytes(offset, .{});
    return reader.readUntilDelimiterAlloc(allocator, 0, @truncate(max_size));
}

pub const Font = struct {
    entry: FontDirEntry,
    device_name: []const u8 = &.{},
    face_name: []const u8 = &.{},
    allocator: std.mem.Allocator,

    pub fn deinit(self: Font) void {
        self.allocator.free(self.device_name);
        self.allocator.free(self.face_name);
    }

    /// Write this font's data as a FONTDIRENTRY for use in a FONTDIR resource.
    ///
    /// This implementation is based on the behavior of the 16-bit version of
    /// RC.EXE, which better matches the available documentation for
    /// FONTDIRENTRY and the .FNT format's 'device name' and 'face name'
    /// values.
    pub fn writeResData(self: *const Font, writer: anytype) !void {
        try self.entry.write(writer);
        try writer.writeAll(self.device_name);
        try writer.writeByte(0);
        try writer.writeAll(self.face_name);
        try writer.writeByte(0);
    }
};

/// This is FONTDIRENTRY from https://learn.microsoft.com/en-us/windows/win32/menurc/fontdirentry
/// but without the `szDeviceName` and `szFaceName` fields.
///
/// Note: This corresponds with the .FNT format up to and including `dfBitsPointer`
/// (the final `reserved` field is the same size/location as `dfBitsPointer`).
///
/// In .FNT files and in FONTDIR resources, these fields all have zero bytes of padding
/// between them. Use `FontDirEntry.len` for the byte length without padding.
pub const FontDirEntry = struct {
    version: u16,
    size: u32,
    copyright: [60]u8,
    type: u16,
    points: u16,
    vert_res: u16,
    horiz_res: u16,
    ascent: u16,
    interal_leading: u16,
    external_leading: u16,
    italic: u8,
    underline: u8,
    strike_out: u8,
    weight: u16,
    char_set: u8,
    pix_width: u16,
    pix_height: u16,
    pitch_and_family: u8,
    avg_width: u16,
    max_width: u16,
    first_char: u8,
    last_char: u8,
    default_char: u8,
    break_char: u8,
    width_bytes: u16,
    /// Offset from the start of the .FNT to the location of the NUL-terminated device name.
    /// If zero, then there is no device name.
    device_offset: u32,
    /// Offset from the start of the .FNT to the location of the NUL-terminated face name.
    /// If zero, then the face name is at offset zero (but is likely indicative of an invalid
    /// .FNT file if the first byte of the file is not 0).
    face_offset: u32,
    reserved: u32,

    pub const len = len: {
        var val: comptime_int = 0;
        inline for (@typeInfo(FontDirEntry).Struct.fields) |field| {
            val += @sizeOf(field.type);
        }
        break :len val;
    };

    pub fn read(reader: anytype) !FontDirEntry {
        var entry: FontDirEntry = undefined;
        inline for (@typeInfo(FontDirEntry).Struct.fields) |field| {
            switch (field.type) {
                u8 => @field(entry, field.name) = try reader.readByte(),
                u16, u32 => @field(entry, field.name) = try reader.readIntLittle(field.type),
                [60]u8 => @field(entry, field.name) = try reader.readBytesNoEof(60),
                else => @compileError("unknown field in FontDirEntry"),
            }
        }
        return entry;
    }

    pub fn write(entry: FontDirEntry, writer: anytype) !void {
        inline for (@typeInfo(FontDirEntry).Struct.fields) |field| {
            switch (field.type) {
                u8 => try writer.writeByte(@field(entry, field.name)),
                u16, u32 => try writer.writeIntLittle(field.type, @field(entry, field.name)),
                [60]u8 => try writer.writeAll(&@field(entry, field.name)),
                else => @compileError("unknown field in FontDirEntry"),
            }
        }
    }
};

fn testFont(buf: []u8, expected_entry: FontDirEntry, expected_device_name: []const u8, expected_face_name: []const u8) !void {
    var buf_fbs = std.io.fixedBufferStream(buf);
    try expected_entry.write(buf_fbs.writer());

    var fbs = std.io.fixedBufferStream(buf);

    const font = try read(std.testing.allocator, fbs.reader(), buf.len);
    defer font.deinit();

    try std.testing.expectEqualDeep(expected_entry, font.entry);
    try std.testing.expectEqualStrings(expected_device_name, font.device_name);
    try std.testing.expectEqualStrings(expected_face_name, font.face_name);
}

const TestData = struct {
    // Write in a device name and face name somewhere in buf outside of the first FontDirEntry.len bytes
    const device_offset = 130;
    // Note: must be var due to a Zig bug, see https://github.com/ziglang/zig/issues/15944
    var device_name = "abcdefg";
    const face_offset = 167;
    // Note: must be var due to a Zig bug, see https://github.com/ziglang/zig/issues/15944
    var face_name = "font face name";

    pub fn init() [256]u8 {
        var buf = [_]u8{0xFF} ** 256;
        (buf[device_offset..][0 .. device_name.len + 1]).* = device_name[0 .. device_name.len + 1].*;
        (buf[face_offset..][0 .. face_name.len + 1]).* = face_name[0 .. face_name.len + 1].*;
        return buf;
    }
};

test "basic" {
    var buf = TestData.init();
    var entry = std.mem.zeroes(FontDirEntry);
    entry.version = 0x300;

    // Device and face offset are zero, so the names should be zero-length.
    try testFont(&buf, entry, "", "");

    // Now the face and device names should be found.
    entry.face_offset = TestData.face_offset;
    entry.device_offset = TestData.device_offset;
    try testFont(&buf, entry, TestData.device_name, TestData.face_name);
}

test "name errors" {
    var buf = TestData.init();
    var entry = std.mem.zeroes(FontDirEntry);
    entry.version = 0x300;

    // Face/device offset within the FontDirEntry is not allowed
    entry.face_offset = 13;
    try std.testing.expectError(
        error.ImpossibleNameOffset,
        testFont(&buf, entry, "", ""),
    );

    // Face/device offset >= file size is not allowed
    entry.face_offset = buf.len;
    try std.testing.expectError(
        error.ImpossibleNameOffset,
        testFont(&buf, entry, "", ""),
    );

    // No terminator before hitting EOF when reading a name is an error
    entry.face_offset = buf.len - 5;
    try std.testing.expectError(
        error.UnexpectedEOF,
        testFont(&buf, entry, "", ""),
    );
}

test "too short of a file" {
    var buf = [_]u8{0} ** 100;
    var fbs = std.io.fixedBufferStream(&buf);

    try std.testing.expectError(
        error.UnexpectedEOF,
        read(std.testing.allocator, fbs.reader(), buf.len),
    );
}

test "invalid header" {
    var buf = TestData.init();
    var entry = std.mem.zeroes(FontDirEntry);

    // Version must have a 0x00 first byte
    entry.version = 0x1234;
    try std.testing.expectError(
        error.InvalidHeader,
        testFont(&buf, entry, "", ""),
    );
}
