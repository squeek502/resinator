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

const LatestMsvcDir = struct {
    /// Intended to be equivalent to ISetupConfiguration.EnumInstances:
    /// https://learn.microsoft.com/en-us/dotnet/api/microsoft.visualstudio.setup.configuration
    /// but without the use of COM in order to avoid a dependency on ole32.dll
    ///
    /// The logic in this function is intended to match what ISetupConfiguration does
    /// under-the-hood, as verified using Procmon.
    fn find(allocator: std.mem.Allocator) error{ OutOfMemory, PathNotFound }![]const u8 {
        // Typically `%PROGRAMDATA%\Microsoft\VisualStudio\Packages\_Instances`
        // This will contain directories with names of instance IDs like 80a758ca,
        // which will contain `state.json` files that have the version and
        // installation directory.
        var instances_dir = try findInstancesDir(allocator);
        defer instances_dir.close();

        var state_subpath_buf: [std.fs.MAX_NAME_BYTES + 32]u8 = undefined;
        var latest_version_dir = std.ArrayListUnmanaged(u8){};
        errdefer latest_version_dir.deinit(allocator);

        var latest_version: u64 = 0;
        var instances_dir_it = instances_dir.iterateAssumeFirstIteration();
        while (instances_dir_it.next() catch return error.PathNotFound) |entry| {
            if (entry.kind != .directory) continue;

            var fbs = std.io.fixedBufferStream(&state_subpath_buf);
            const writer = fbs.writer();

            writer.writeAll(entry.name) catch unreachable;
            writer.writeByte(std.fs.path.sep) catch unreachable;
            writer.writeAll("state.json") catch unreachable;

            const json_contents = instances_dir.readFileAlloc(allocator, fbs.getWritten(), std.math.maxInt(usize)) catch continue;
            defer allocator.free(json_contents);

            var parsed = std.json.parseFromSlice(std.json.Value, allocator, json_contents, .{}) catch continue;
            defer parsed.deinit();

            if (parsed.value != .object) continue;
            const catalog_info = parsed.value.object.get("catalogInfo") orelse continue;
            if (catalog_info != .object) continue;
            const product_version_value = catalog_info.object.get("buildVersion") orelse continue;
            if (product_version_value != .string) continue;
            const product_version_text = product_version_value.string;
            const parsed_version = parseVersionQuad(product_version_text) catch continue;

            // We want to end up with the most recent version installed
            if (parsed_version <= latest_version) continue;

            const installation_path = parsed.value.object.get("installationPath") orelse continue;
            if (installation_path != .string) continue;

            latest_version_dir.clearRetainingCapacity();
            try latest_version_dir.appendSlice(allocator, installation_path);
            latest_version = parsed_version;
        }

        if (latest_version_dir.items.len == 0) return error.PathNotFound;
        return latest_version_dir.toOwnedSlice(allocator);
    }

    fn findInstancesDirViaCLSID(allocator: std.mem.Allocator) error{ OutOfMemory, PathNotFound }!std.fs.Dir {
        const setup_configuration_clsid = "{177f0c4a-1cd3-4de7-a32c-71dbbb9fa36d}";
        const setup_config_key = RegistryUtf8.openKey(windows.HKEY_CLASSES_ROOT, "CLSID\\" ++ setup_configuration_clsid) catch |err| switch (err) {
            error.KeyNotFound => return error.PathNotFound,
        };
        defer setup_config_key.closeKey();

        const dll_path = setup_config_key.getString(allocator, "InprocServer32", "") catch |err| switch (err) {
            error.NotAString,
            error.ValueNameNotFound,
            error.StringNotFound,
            => return error.PathNotFound,

            error.OutOfMemory => return error.OutOfMemory,
        };
        defer allocator.free(dll_path);

        var path_it = std.fs.path.componentIterator(dll_path) catch return error.PathNotFound;
        // the .dll filename
        _ = path_it.last();
        const root_path = while (path_it.previous()) |dir_component| {
            if (std.ascii.eqlIgnoreCase(dir_component.name, "VisualStudio")) {
                break dir_component.path;
            }
        } else {
            return error.PathNotFound;
        };

        const instances_path = try std.fs.path.join(allocator, &.{ root_path, "Packages", "_Instances" });
        defer allocator.free(instances_path);

        return std.fs.openDirAbsolute(instances_path, .{ .iterate = true }) catch return error.PathNotFound;
    }

    fn findInstancesDir(allocator: std.mem.Allocator) error{ OutOfMemory, PathNotFound }!std.fs.Dir {
        // First try to get the path from the .dll that would have been
        // loaded via COM for SetupConfiguration.
        return findInstancesDirViaCLSID(allocator) catch |orig_err| {
            // If that can't be found, fall back to manually appending
            // `Microsoft\VisualStudio\Packages\_Instances` to %PROGRAMDATA%
            const program_data = std.process.getEnvVarOwned(allocator, "PROGRAMDATA") catch |err| switch (err) {
                error.OutOfMemory => |e| return e,
                else => return orig_err,
            };
            defer allocator.free(program_data);

            const instances_path = try std.fs.path.join(allocator, &.{ program_data, "Microsoft", "VisualStudio", "Packages", "_Instances" });
            defer allocator.free(instances_path);

            return std.fs.openDirAbsolute(instances_path, .{ .iterate = true }) catch return orig_err;
        };
    }

    /// Intended to be equivalent to `ISetupHelper.ParseVersion`
    /// Example: 17.4.33205.214 -> 0x0011000481b500d6
    fn parseVersionQuad(version: []const u8) error{InvalidVersion}!u64 {
        var it = std.mem.splitScalar(u8, version, '.');
        const a = it.next() orelse return error.InvalidVersion;
        const b = it.next() orelse return error.InvalidVersion;
        const c = it.next() orelse return error.InvalidVersion;
        const d = it.next() orelse return error.InvalidVersion;
        if (it.next()) |_| return error.InvalidVersion;
        var result: u64 = undefined;
        var result_bytes = std.mem.asBytes(&result);

        std.mem.writeInt(
            u16,
            result_bytes[0..2],
            std.fmt.parseUnsigned(u16, d, 10) catch return error.InvalidVersion,
            .little,
        );
        std.mem.writeInt(
            u16,
            result_bytes[2..4],
            std.fmt.parseUnsigned(u16, c, 10) catch return error.InvalidVersion,
            .little,
        );
        std.mem.writeInt(
            u16,
            result_bytes[4..6],
            std.fmt.parseUnsigned(u16, b, 10) catch return error.InvalidVersion,
            .little,
        );
        std.mem.writeInt(
            u16,
            result_bytes[6..8],
            std.fmt.parseUnsigned(u16, a, 10) catch return error.InvalidVersion,
            .little,
        );

        return result;
    }
};
