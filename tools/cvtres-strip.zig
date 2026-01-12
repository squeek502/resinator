const std = @import("std");
const utils = @import("utils");

pub fn main(init: std.process.Init) !void {
    const gpa = init.gpa;
    const io = init.io;
    const arena = init.arena.allocator();

    const args = try init.minimal.args.toSlice(arena);

    if (args.len <= 1) {
        std.debug.print("usage: {s} <filepath>\n", .{args[0]});
    }

    const filepath = args[1];

    var file = try std.Io.Dir.cwd().openFile(io, filepath, .{ .mode = .read_write });
    defer file.close(io);

    var buf: std.Io.Writer.Allocating = try .initCapacity(gpa, try file.length(io));
    defer buf.deinit();

    var reader_buf: [1024]u8 = undefined;
    var file_reader = file.reader(io, &reader_buf);

    try utils.stripAndFixupCoff(gpa, &file_reader.interface, &buf.writer, .{});

    try file.writePositionalAll(io, buf.written(), 0);
    try file.setLength(io, buf.written().len);
}
