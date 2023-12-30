const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;

const compressed_mingw_includes = @import("compressed_mingw_includes").data;
const include_ver = 2;

pub fn extractMingwIncludes(allocator: Allocator, maybe_progress: ?*std.Progress) ![]const u8 {
    const resinator_cache_path = try getCachePath(allocator, "resinator") orelse return error.CannotResolveCachePath;
    defer allocator.free(resinator_cache_path);

    var resinator_cache_dir = try std.fs.cwd().makeOpenPath(resinator_cache_path, .{});
    defer resinator_cache_dir.close();

    existing_ver: {
        var manifest_buffer: [32]u8 = undefined;
        if (resinator_cache_dir.readFile("include_ver", &manifest_buffer)) |ver_bytes| {
            var it = std.mem.splitAny(u8, ver_bytes, " \t\r\n");
            const existing_ver = std.fmt.parseInt(u16, it.first(), 10) catch break :existing_ver;
            if (existing_ver >= include_ver) return std.fs.path.join(allocator, &.{ resinator_cache_path, "include" });
        } else |_| {}
    }

    const extract_node: ?*std.Progress.Node = if (maybe_progress) |progress|
        progress.start("extracting MinGW includes into cache [one-time setup]", 0)
    else
        null;
    defer if (extract_node) |node| node.end();
    if (maybe_progress) |progress| progress.refresh();

    // delete any previously existing include dir since it's either out-of-date
    // or in a weird state and we should just start over.
    resinator_cache_dir.deleteTree("include") catch {};

    var in_stream = std.io.fixedBufferStream(compressed_mingw_includes);
    var zstd_stream = std.compress.zstd.decompressStream(allocator, in_stream.reader());
    defer zstd_stream.deinit();

    const uncompressed_tar = try zstd_stream.reader().readAllAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(uncompressed_tar);
    var tar_stream = std.io.fixedBufferStream(uncompressed_tar);

    try std.tar.pipeToFileSystem(resinator_cache_dir, tar_stream.reader(), .{ .mode_mode = .ignore });
    const include_ver_contents = std.fmt.comptimePrint("{}", .{include_ver});
    try resinator_cache_dir.writeFile("include_ver", include_ver_contents);

    return std.fs.path.join(allocator, &.{ resinator_cache_path, "include" });
}

pub fn getCachePath(allocator: Allocator, appname: []const u8) !?[]const u8 {
    switch (builtin.os.tag) {
        .windows => {
            const local_app_data = std.process.getEnvVarOwned(allocator, "LOCALAPPDATA") orelse return null;
            defer allocator.free(local_app_data);
            return try std.fs.path.join(allocator, &.{ "Temp", "resinator" });
        },
        .macos => {
            const home = std.os.getenv("HOME") orelse return null;
            return try std.fs.path.join(allocator, &.{ home, "Library/Caches", appname });
        },
        else => {
            if (std.os.getenv("XDG_CACHE_HOME")) |cache| {
                return try std.fs.path.join(allocator, &.{ cache, appname });
            }
            const home = std.os.getenv("HOME") orelse return null;
            return try std.fs.path.join(allocator, &.{ home, ".cache", appname });
        },
    }
}
