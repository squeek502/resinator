const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const windows = std.os.windows;
const L = std.unicode.utf8ToUtf16LeStringLiteral;

const compressed_mingw_includes = @import("compressed_mingw_includes").data;
const include_ver = 2;

pub fn extractMingwIncludes(allocator: Allocator, maybe_progress: ?std.Progress.Node) ![]const u8 {
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

    const extract_node: ?std.Progress.Node = if (maybe_progress) |progress|
        progress.start("extracting MinGW includes into cache", 0)
    else
        null;
    defer if (extract_node) |node| node.end();

    // delete any previously existing include dir since it's either out-of-date
    // or in a weird state and we should just start over.
    resinator_cache_dir.deleteTree("include") catch {};

    const window_len = std.compress.zstd.default_window_len;
    const window_buffer = try allocator.create([window_len]u8);
    defer allocator.destroy(window_buffer);
    var in: std.io.Reader = .fixed(compressed_mingw_includes);
    var decompress: std.compress.zstd.Decompress = .init(&in, window_buffer, .{
        .window_len = window_len,
        .verify_checksum = false,
    });

    try std.tar.pipeToFileSystem(resinator_cache_dir, &decompress.reader, .{ .mode_mode = .ignore });
    const include_ver_contents = std.fmt.comptimePrint("{}", .{include_ver});
    try resinator_cache_dir.writeFile(.{ .sub_path = "include_ver", .data = include_ver_contents });

    return std.fs.path.join(allocator, &.{ resinator_cache_path, "include" });
}

pub fn getCachePath(allocator: Allocator, appname: []const u8) error{OutOfMemory}!?[]const u8 {
    switch (builtin.os.tag) {
        .windows => {
            const local_app_data = std.process.getEnvVarOwned(allocator, "LOCALAPPDATA") catch |err| switch (err) {
                error.EnvironmentVariableNotFound => return null,
                error.InvalidWtf8 => unreachable,
                else => |e| return e,
            };
            defer allocator.free(local_app_data);
            return try std.fs.path.join(allocator, &.{ local_app_data, "Temp", appname });
        },
        .macos => {
            if (std.posix.getenv("XDG_CACHE_HOME")) |cache| {
                return try std.fs.path.join(allocator, &.{ cache, appname });
            }
            const home = std.posix.getenv("HOME") orelse return null;
            return try std.fs.path.join(allocator, &.{ home, "Library/Caches", appname });
        },
        else => {
            if (std.posix.getenv("XDG_CACHE_HOME")) |cache| {
                return try std.fs.path.join(allocator, &.{ cache, appname });
            }
            const home = std.posix.getenv("HOME") orelse return null;
            return try std.fs.path.join(allocator, &.{ home, ".cache", appname });
        },
    }
}

pub fn getMsvcIncludePaths(allocator: Allocator) error{ OutOfMemory, MsvcIncludesNotFound }![]const []const u8 {
    var list = try std.ArrayList([]const u8).initCapacity(allocator, 5);
    errdefer {
        for (list.items) |path| allocator.free(path);
        list.deinit();
    }

    const msvc_tools_path = LatestMsvcToolsDir.find(allocator) catch |err| switch (err) {
        error.PathNotFound => return error.MsvcIncludesNotFound,
        else => |e| return e,
    };
    defer allocator.free(msvc_tools_path);

    const msvc_tools_include = try std.fs.path.join(allocator, &.{ msvc_tools_path, "include" });
    list.appendAssumeCapacity(msvc_tools_include);

    const atlmfc_include = try std.fs.path.join(allocator, &.{ msvc_tools_path, "atlmfc", "include" });
    list.appendAssumeCapacity(atlmfc_include);

    const sdk_include_path = LatestSdkIncludeDir.find(allocator) catch |err| switch (err) {
        error.PathNotFound => return error.MsvcIncludesNotFound,
        else => |e| return e,
    };
    defer allocator.free(sdk_include_path);

    const sdk_ucrt = try std.fs.path.join(allocator, &.{ sdk_include_path, "ucrt" });
    list.appendAssumeCapacity(sdk_ucrt);

    const sdk_um = try std.fs.path.join(allocator, &.{ sdk_include_path, "um" });
    list.appendAssumeCapacity(sdk_um);

    const sdk_shared = try std.fs.path.join(allocator, &.{ sdk_include_path, "shared" });
    list.appendAssumeCapacity(sdk_shared);

    return list.toOwnedSlice();
}

// TODO: Remove once this constant gets added to std.os.windows
const HKEY_CURRENT_USER: windows.HKEY = @ptrFromInt(0x80000001);

/// Based on Common7\Tools\vsdevcmd\core\winsdk.bat of a MSVC installation
pub const LatestSdkIncludeDir = struct {
    pub fn find(allocator: Allocator) error{ OutOfMemory, PathNotFound }![]const u8 {
        return find10(allocator) catch |err| switch (err) {
            error.OutOfMemory => |e| return e,
            error.PathNotFound => find8_1(allocator),
        };
    }

    pub fn find8_1(allocator: Allocator) error{ OutOfMemory, PathNotFound }![]const u8 {
        var dir = try findInstallationIncludeDir("v8.1");
        defer dir.close();

        // SDK v8.1 doesn't have subdirs with different SDK versions in its Include dir,
        // so we can just return the path to the Include dir.
        // This is a roundabout way of doing this since we build the path, open the dir,
        // then get the path from the dir, but oh well.

        var fd_path_buf: [std.fs.max_path_bytes]u8 = undefined;
        const dir_path = std.os.getFdPath(dir.fd, &fd_path_buf) catch return error.PathNotFound;
        return allocator.dupe(u8, dir_path);
    }

    pub fn find10(allocator: Allocator) error{ OutOfMemory, PathNotFound }![]const u8 {
        var dir = try findInstallationIncludeDir("v10.0");
        defer dir.close();

        var subpath_buf: [std.fs.MAX_NAME_BYTES]u8 = undefined;
        var latest_version_dir = std.ArrayListUnmanaged(u8).initBuffer(&subpath_buf);

        var latest_version: u64 = 0;
        var dir_it = dir.iterateAssumeFirstIteration();
        while (dir_it.next() catch return error.PathNotFound) |entry| {
            if (entry.kind != .directory) continue;

            const parsed_version = parseVersionQuad(entry.name) catch continue;

            // We want to end up with the most recent version installed
            if (parsed_version <= latest_version) continue;

            // Verify that the SDK has at least some of the expected files
            if (!verifySdk(dir, entry.name)) continue;

            latest_version_dir.clearRetainingCapacity();
            latest_version_dir.appendSliceAssumeCapacity(entry.name);
            latest_version = parsed_version;
        }

        if (latest_version == 0) return error.PathNotFound;

        var fd_path_buf: [std.fs.max_path_bytes]u8 = undefined;
        const dir_path = std.os.getFdPath(dir.fd, &fd_path_buf) catch return error.PathNotFound;
        return std.fs.path.join(allocator, &.{ dir_path, latest_version_dir.items });
    }

    fn verifySdk(installation_dir: std.fs.Dir, sdk_ver: []const u8) bool {
        var dir = installation_dir.openDir(sdk_ver, .{}) catch return false;
        defer dir.close();

        // winsdk.bat checks for winsdkver.h by default, or Windows.h if the app platform
        // is set to UWP. We just check for the existence of either one and call that good enough.
        winsdkver_h: {
            const stat = dir.statFile("um/winsdkver.h") catch break :winsdkver_h;
            if (stat.kind == .file) return true;
        }
        windows_h: {
            const stat = dir.statFile("um/Windows.h") catch break :windows_h;
            if (stat.kind == .file) return true;
        }

        return false;
    }

    /// Returns a Dir with iterate permissions.
    pub fn findInstallationIncludeDir(comptime version: []const u8) error{PathNotFound}!std.fs.Dir {
        const KeyAndOptions = struct { root_key: windows.HKEY, options: OpenKeyOptions };
        const variants = [_]KeyAndOptions{
            .{ .root_key = windows.HKEY_LOCAL_MACHINE, .options = .{ .wow64_32 = true } },
            .{ .root_key = HKEY_CURRENT_USER, .options = .{ .wow64_32 = true } },
            .{ .root_key = windows.HKEY_LOCAL_MACHINE, .options = .{} },
            .{ .root_key = HKEY_CURRENT_USER, .options = .{} },
        };
        for (&variants) |variant| {
            const installation_path = findInstallationIncludePath(version, variant.root_key, variant.options) catch continue;
            return std.fs.openDirAbsoluteW(installation_path.span(), .{ .iterate = true }) catch continue;
        }
        return error.PathNotFound;
    }

    /// Returns an NT-prefixed absolute path if the installation path was found.
    pub fn findInstallationIncludePath(comptime version: []const u8, root_key: windows.HKEY, options: OpenKeyOptions) error{PathNotFound}!windows.PathSpace {
        const key = openKey(root_key, L("SOFTWARE\\Microsoft\\Microsoft SDKs\\Windows\\" ++ version), options) catch return error.PathNotFound;
        defer _ = std.os.windows.advapi32.RegCloseKey(key);

        var installation_path = windows.PathSpace{ .data = undefined, .len = 0 };

        // Pre-populate the NT prefix. This avoids putting a redundant PathSpace on the stack
        // since we avoid the need to call windows.wToPrefixedFileW.
        const nt_prefix = L("\\??\\");
        @memcpy(installation_path.data[0..nt_prefix.len], nt_prefix);
        installation_path.len += nt_prefix.len;

        // Read into path_buf, but after the NT prefix.
        const written_slice = getRegistryStringValue(installation_path.data[installation_path.len..], key, L("InstallationFolder")) catch return error.PathNotFound;
        installation_path.len += written_slice.len;

        // Append Include since that's the subdir we want to iterate to find the latest version
        // (e.g. `<...>\Windows Kits\10\Include\10.0.22621.0` where `<...>\Windows Kits\10` is
        // the installation path, and `10.0.22621.0` is an installed SDK version)
        if (!std.fs.path.PathType.windows.isSep(u16, installation_path.data[installation_path.len - 1])) {
            installation_path.data[installation_path.len] = std.fs.path.sep;
            installation_path.len += 1;
        }
        const includes_subpath = L("Include");
        // copy over the subpath including the NUL terminator
        @memcpy(installation_path.data[installation_path.len..][0 .. includes_subpath.len + 1], includes_subpath[0 .. includes_subpath.len + 1]);
        installation_path.len += includes_subpath.len;

        return installation_path;
    }
};

pub const LatestMsvcToolsDir = struct {
    /// Intended to be equivalent to ISetupConfiguration.EnumInstances:
    /// https://learn.microsoft.com/en-us/dotnet/api/microsoft.visualstudio.setup.configuration
    /// but without the use of COM in order to avoid a dependency on ole32.dll
    ///
    /// The logic in this function is intended to match what ISetupConfiguration does
    /// under-the-hood, as verified using Procmon.
    pub fn find(allocator: Allocator) error{ OutOfMemory, PathNotFound }![]const u8 {
        // Typically `%PROGRAMDATA%\Microsoft\VisualStudio\Packages\_Instances`
        // This will contain directories with names of instance IDs like 80a758ca,
        // which will contain `state.json` files that have the version and
        // installation directory.
        var instances_dir = try findInstancesDir();
        defer instances_dir.close();

        var state_subpath_buf: [std.fs.MAX_NAME_BYTES + 32]u8 = undefined;
        var latest_version_dir: std.ArrayListUnmanaged(u8) = .empty;
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

            const tools_dir_path = toolsDirFromInstallationPath(allocator, installation_path.string) catch |err| switch (err) {
                error.OutOfMemory => |e| return e,
                error.PathNotFound => continue,
            };
            defer allocator.free(tools_dir_path);

            latest_version_dir.clearRetainingCapacity();
            try latest_version_dir.appendSlice(allocator, tools_dir_path);
            latest_version = parsed_version;
        }

        if (latest_version_dir.items.len == 0) return error.PathNotFound;
        return latest_version_dir.toOwnedSlice(allocator);
    }

    fn toolsDirFromInstallationPath(allocator: Allocator, installation_path: []const u8) error{ OutOfMemory, PathNotFound }![]const u8 {
        var lib_dir_buf = try std.ArrayList(u8).initCapacity(allocator, installation_path.len + 64);
        errdefer lib_dir_buf.deinit();

        lib_dir_buf.appendSliceAssumeCapacity(installation_path);

        if (!std.fs.path.isSep(lib_dir_buf.getLast())) {
            lib_dir_buf.appendAssumeCapacity('\\');
        }
        const installation_path_with_trailing_sep_len = lib_dir_buf.items.len;

        lib_dir_buf.appendSliceAssumeCapacity("VC\\Auxiliary\\Build\\Microsoft.VCToolsVersion.default.txt");
        var default_tools_version_buf: [512]u8 = undefined;
        const default_tools_version_contents = std.fs.cwd().readFile(lib_dir_buf.items, &default_tools_version_buf) catch {
            return error.PathNotFound;
        };
        var tokenizer = std.mem.tokenizeAny(u8, default_tools_version_contents, " \r\n");
        const default_tools_version = tokenizer.next() orelse return error.PathNotFound;

        lib_dir_buf.shrinkRetainingCapacity(installation_path_with_trailing_sep_len);
        lib_dir_buf.appendSliceAssumeCapacity("VC\\Tools\\MSVC\\");
        try lib_dir_buf.appendSlice(default_tools_version);

        return lib_dir_buf.toOwnedSlice();
    }

    fn findInstancesDirViaSetup() error{PathNotFound}!std.fs.Dir {
        const vs_setup_key_path = L("SOFTWARE\\Microsoft\\VisualStudio\\Setup");
        const vs_setup_key = openKey(windows.HKEY_LOCAL_MACHINE, vs_setup_key_path, .{}) catch |err| switch (err) {
            error.KeyNotFound => return error.PathNotFound,
        };
        defer _ = std.os.windows.advapi32.RegCloseKey(vs_setup_key);

        var path_buf = windows.PathSpace{ .data = undefined, .len = 0 };
        // Pre-populate the NT prefix. This avoids putting a redundant PathSpace on the stack
        // since we avoid the need to call windows.wToPrefixedFileW.
        const nt_prefix = L("\\??\\");
        @memcpy(path_buf.data[0..nt_prefix.len], nt_prefix);
        path_buf.len += nt_prefix.len;

        // Read into path_buf, but after the NT prefix.
        const packages_path = getRegistryStringValue(path_buf.data[path_buf.len..], vs_setup_key, L("CachePath")) catch return error.PathNotFound;
        path_buf.len += packages_path.len;

        if (!std.fs.path.isAbsoluteWindowsWTF16(packages_path)) return error.PathNotFound;

        const instances_subpath_with_preceding_sep = L("\\_Instances");
        const has_trailing_sep = packages_path[packages_path.len - 1] == '\\' or packages_path[packages_path.len - 1] == '/';
        const instances_subpath = if (has_trailing_sep) instances_subpath_with_preceding_sep[1.. :0] else instances_subpath_with_preceding_sep;
        // copy over the subpath including the NUL terminator
        @memcpy(path_buf.data[path_buf.len..][0 .. instances_subpath.len + 1], instances_subpath[0 .. instances_subpath.len + 1]);
        path_buf.len += instances_subpath.len;

        return std.fs.openDirAbsoluteW(path_buf.span(), .{ .iterate = true }) catch return error.PathNotFound;
    }

    fn findInstancesDirViaCLSID() error{PathNotFound}!std.fs.Dir {
        const setup_configuration_clsid = "{177f0c4a-1cd3-4de7-a32c-71dbbb9fa36d}";
        const key_path = L("CLSID\\" ++ setup_configuration_clsid ++ "\\InprocServer32");
        const setup_config_key = openKey(windows.HKEY_CLASSES_ROOT, key_path, .{}) catch return error.PathNotFound;
        defer _ = std.os.windows.advapi32.RegCloseKey(setup_config_key);

        var path_buf = windows.PathSpace{ .data = undefined, .len = 0 };
        // Pre-populate the NT prefix. This avoids putting a redundant PathSpace on the stack
        // since we avoid the need to call windows.wToPrefixedFileW.
        const nt_prefix = L("\\??\\");
        @memcpy(path_buf.data[0..nt_prefix.len], nt_prefix);
        path_buf.len += nt_prefix.len;

        // Read into path_buf, but after the NT prefix.
        // We want to get the default / unnamed value of InprocServer32, so the name is an empty string.
        const dll_path = getRegistryStringValue(path_buf.data[path_buf.len..], setup_config_key, L("")) catch return error.PathNotFound;

        // dll_path will be something like `C:\ProgramData\Microsoft\VisualStudio\Setup\x64\Microsoft.VisualStudio.Setup.Configuration.Native.dll`
        // but we just want the portion up to and including `VisualStudio`
        var path_it = std.fs.path.ComponentIterator(.windows, u16).init(dll_path) catch return error.PathNotFound;
        // Path must be absolute
        if (path_it.root() == null) return error.PathNotFound;
        // the .dll filename
        _ = path_it.last();
        const root_path = while (path_it.previous()) |dir_component| {
            if (windows.eqlIgnoreCaseWTF16(dir_component.name, L("VisualStudio"))) {
                break dir_component.path;
            }
        } else {
            return error.PathNotFound;
        };
        path_buf.len += root_path.len;

        // Always add a separator since ComponentIterator guarantees that there will
        // be no trailing separator.
        const instances_subpath = L("\\Packages\\_Instances");
        // copy over the subpath including the NUL terminator
        @memcpy(path_buf.data[path_buf.len..][0 .. instances_subpath.len + 1], instances_subpath[0 .. instances_subpath.len + 1]);
        path_buf.len += instances_subpath.len;

        return std.fs.openDirAbsoluteW(path_buf.span(), .{ .iterate = true }) catch return error.PathNotFound;
    }

    fn findInstancesDir() error{PathNotFound}!std.fs.Dir {
        // First, try getting the packages cache path from the registry.
        // This only seems to exist when the path is different from the default.
        method1: {
            return findInstancesDirViaSetup() catch |err| switch (err) {
                error.PathNotFound => break :method1,
            };
        }
        // Otherwise, try to get the path from the .dll that would have been
        // loaded via COM for SetupConfiguration.
        method2: {
            return findInstancesDirViaCLSID() catch |err| switch (err) {
                error.PathNotFound => break :method2,
            };
        }
        // If that can't be found, fall back to manually appending
        // `Microsoft\VisualStudio\Packages\_Instances` to %PROGRAMDATA%
        method3: {
            const program_data = std.process.getenvW(L("PROGRAMDATA")) orelse break :method3;
            // Must be an absolute path
            if (!std.fs.path.isAbsoluteWindowsWTF16(program_data)) break :method3;

            var instances_path = windows.PathSpace{ .data = undefined, .len = 0 };
            // Pre-populate the NT prefix. This avoids putting a redundant PathSpace on the stack
            // since we avoid the need to call windows.wToPrefixedFileW.
            const nt_prefix = L("\\??\\");
            @memcpy(instances_path.data[0..nt_prefix.len], nt_prefix);
            instances_path.len += nt_prefix.len;

            @memcpy(instances_path.data[instances_path.len..][0..program_data.len], program_data);
            instances_path.len += program_data.len;
            if (program_data.len > 0 and !std.fs.path.PathType.windows.isSep(u16, program_data[program_data.len - 1])) {
                instances_path.data[instances_path.len] = std.fs.path.sep;
                instances_path.len += 1;
            }
            const instances_subpath = L("Microsoft\\VisualStudio\\Packages\\_Instances");
            // copy over the subpath including the NUL terminator
            @memcpy(instances_path.data[instances_path.len..][0 .. instances_subpath.len + 1], instances_subpath[0 .. instances_subpath.len + 1]);
            instances_path.len += instances_subpath.len;

            return std.fs.openDirAbsoluteW(instances_path.span(), .{ .iterate = true }) catch break :method3;
        }
        return error.PathNotFound;
    }
};

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

const OpenKeyOptions = struct {
    wow64_32: bool = false,
};

fn openKey(root_key: windows.HKEY, key_path: [:0]const u16, options: OpenKeyOptions) error{KeyNotFound}!windows.HKEY {
    var key: windows.HKEY = undefined;
    var access_attributes: windows.REGSAM = windows.KEY_QUERY_VALUE;
    if (options.wow64_32) access_attributes |= windows.KEY_WOW64_32KEY;
    const open_result = std.os.windows.advapi32.RegOpenKeyExW(
        root_key,
        key_path,
        0,
        access_attributes,
        &key,
    );
    switch (@as(windows.Win32Error, @enumFromInt(open_result))) {
        .SUCCESS => return key,
        else => return error.KeyNotFound,
    }
}

/// Returns error.NotAString if the value if not of type REG_SZ/REG_EXPAND_SZ
fn getRegistryStringValue(
    value_buf: []u16,
    key: windows.HKEY,
    value_name: [:0]const u16,
) error{ NotAString, FileNotFound, BufferTooSmall, Unexpected }![:0]u16 {
    var value_type: windows.DWORD = undefined;
    var value_size: windows.DWORD = @intCast(value_buf.len * 2);
    const buf = std.mem.sliceAsBytes(value_buf);
    const query_result = windows.advapi32.RegQueryValueExW(
        key,
        value_name,
        null,
        &value_type,
        @ptrCast(buf.ptr),
        &value_size,
    );
    switch (@as(windows.Win32Error, @enumFromInt(query_result))) {
        .SUCCESS => {
            switch (value_type) {
                windows.REG.SZ, windows.REG.EXPAND_SZ => {},
                else => return error.NotAString,
            }
            const code_unit_len = value_size / 2 - 1;
            // RegQueryValueExW does not guarantee that the values
            // returned are NUL-terminated, so force NUL-termination.
            value_buf[code_unit_len] = 0;
            return value_buf[0..code_unit_len :0];
        },
        .FILE_NOT_FOUND => return error.FileNotFound,
        .MORE_DATA => return error.BufferTooSmall,
        else => |err| return windows.unexpectedError(err),
    }
}
