const std = @import("std");
const Allocator = std.mem.Allocator;
const Node = @import("ast.zig").Node;
const Lexer = @import("lex.zig").Lexer;
const Parser = @import("parse.zig").Parser;
const Resource = @import("rc.zig").Resource;
const parseQuotedAsciiString = @import("literals.zig").parseQuotedAsciiString;
const parseQuotedWideStringAlloc = @import("literals.zig").parseQuotedWideStringAlloc;
const res = @import("res.zig");
const WORD = std.os.windows.WORD;
const DWORD = std.os.windows.DWORD;

pub fn compile(allocator: Allocator, source: []const u8, writer: anytype, cwd: std.fs.Dir) !void {
    var lexer = Lexer.init(source);
    var parser = Parser.init(&lexer);
    var tree = try parser.parse(allocator);
    defer tree.deinit();

    var arena_allocator = std.heap.ArenaAllocator.init(allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    var compiler = Compiler{
        .source = source,
        .arena = arena,
        .allocator = allocator,
        .cwd = cwd,
    };

    try compiler.writeRoot(tree.root(), writer);
}

pub const Compiler = struct {
    source: []const u8,
    arena: Allocator,
    allocator: Allocator,
    cwd: std.fs.Dir,

    pub const State = struct {};

    pub fn writeRoot(self: *Compiler, root: *Node.Root, writer: anytype) !void {
        try writeEmptyResource(writer);
        for (root.body) |node| {
            try self.writeNode(node, writer);
        }
    }

    pub fn writeNode(self: *Compiler, node: *Node, writer: anytype) !void {
        switch (node.id) {
            .root => unreachable, // writeRoot should be called directly instead
            .resource_external => try self.writeResourceExternal(@fieldParentPtr(Node.ResourceExternal, "base", node), writer),
            .resource_raw_data => try self.writeResourceRawData(@fieldParentPtr(Node.ResourceRawData, "base", node), writer),
        }
    }

    pub const NameOrOrdinal = union(enum) {
        name: [:0]const u16,
        ordinal: u16,

        pub fn deinit(self: NameOrOrdinal, allocator: Allocator) void {
            switch (self) {
                .name => |name| {
                    allocator.free(name);
                },
                .ordinal => {},
            }
        }

        /// Returns the full length of the amount of bytes that would be written by `write`
        /// (e.g. for an ordinal it will return the length including the 0xFFFF indicator)
        pub fn byteLen(self: NameOrOrdinal) u32 {
            switch (self) {
                .name => |name| {
                    // + 1 for 0-terminated, * 2 for bytes per u16
                    return @intCast(u32, (name.len + 1) * 2);
                },
                .ordinal => return 4,
            }
        }

        pub fn write(self: NameOrOrdinal, writer: anytype) !void {
            switch (self) {
                .name => |name| {
                    try writer.writeAll(std.mem.sliceAsBytes(name[0 .. name.len + 1]));
                },
                .ordinal => |ordinal| {
                    try writer.writeIntLittle(WORD, 0xffff);
                    try writer.writeIntLittle(WORD, ordinal);
                },
            }
        }

        pub fn fromString(allocator: Allocator, str: []const u8) !NameOrOrdinal {
            if (maybeOrdinalFromString(str)) |ordinal| {
                return ordinal;
            }
            return nameFromString(allocator, str);
        }

        pub fn nameFromString(allocator: Allocator, str: []const u8) !NameOrOrdinal {
            const as_utf16 = try std.unicode.utf8ToUtf16LeWithNull(allocator, str);
            return NameOrOrdinal{ .name = as_utf16 };
        }

        pub fn maybeOrdinalFromString(str: []const u8) ?NameOrOrdinal {
            // TODO: Needs to match the `rc` ordinal parsing
            const ordinal = std.fmt.parseUnsigned(WORD, str, 0) catch return null;
            return NameOrOrdinal{ .ordinal = ordinal };
        }
    };

    pub fn writeResourceExternal(self: *Compiler, node: *Node.ResourceExternal, writer: anytype) !void {
        const filename = node.filename.slice(self.source);
        // TODO: emit error on file not found
        const contents: []const u8 = self.cwd.readFileAlloc(self.allocator, filename, std.math.maxInt(u32)) catch "";
        defer self.allocator.free(contents);

        const default_language = 0x409;
        const default_memory_flags = 0x30;

        const type_value = type: {
            const resource_type = Resource.fromString(node.type.slice(self.source));
            if (resource_type != .user_defined) {
                if (res.RT.fromResource(resource_type)) |rt_constant| {
                    break :type NameOrOrdinal{ .ordinal = @enumToInt(rt_constant) };
                } else {
                    @panic("TODO: unhandled resource -> RT constant conversion");
                }
            } else {
                break :type try NameOrOrdinal.fromString(self.allocator, node.type.slice(self.source));
            }
        };
        defer type_value.deinit(self.allocator);

        // TODO: The type can change how the resource is written to the file
        //       (i.e. if it's an ICON then the contents should be parsed as a .ico, among
        //       other things)

        const name_value = try NameOrOrdinal.fromString(self.allocator, node.id.slice(self.source));
        defer name_value.deinit(self.allocator);

        const byte_length_up_to_name: u32 = 8 + name_value.byteLen() + type_value.byteLen();
        const padding_after_name = std.mem.alignForward(byte_length_up_to_name, 4) - byte_length_up_to_name;
        const header_size: u32 = byte_length_up_to_name + @intCast(u32, padding_after_name) + 16;

        try writer.writeIntLittle(DWORD, @intCast(u32, contents.len)); // DataSize
        try writer.writeIntLittle(DWORD, header_size); // HeaderSize
        try type_value.write(writer); // TYPE
        try name_value.write(writer); // NAME
        try writer.writeByteNTimes(0, padding_after_name);

        try writer.writeIntLittle(DWORD, 0); // DataVersion
        try writer.writeIntLittle(WORD, default_memory_flags); // MemoryFlags
        try writer.writeIntLittle(WORD, default_language); // LanguageId
        try writer.writeIntLittle(DWORD, 0); // Version
        try writer.writeIntLittle(DWORD, 0); // Characteristics

        try writer.writeAll(contents);

        const padding_after_data = std.mem.alignForward(contents.len, 4) - contents.len;
        try writer.writeByteNTimes(0, padding_after_data);
    }

    pub fn writeResourceRawData(self: *Compiler, node: *Node.ResourceRawData, writer: anytype) !void {
        var data_buffer = std.ArrayList(u8).init(self.allocator);
        defer data_buffer.deinit();

        for (node.raw_data) |token| {
            switch (token.id) {
                // Any literal here must be a number
                .literal => {
                    // TODO: On overflow, the number should wrap; unsure how big can the literal itself can be before things start breaking more though
                    const number = std.fmt.parseUnsigned(WORD, token.slice(self.source), 0) catch unreachable;
                    var data_writer = data_buffer.writer();
                    try data_writer.writeIntLittle(WORD, number);
                },
                .quoted_ascii_string => {
                    const slice = token.slice(self.source);
                    try data_buffer.appendSlice(parseQuotedAsciiString(slice));
                },
                .quoted_wide_string => {
                    const slice = token.slice(self.source);
                    const parsed_string = try parseQuotedWideStringAlloc(self.allocator, slice);
                    defer self.allocator.free(parsed_string);
                    try data_buffer.appendSlice(std.mem.sliceAsBytes(parsed_string));
                },
                else => unreachable,
            }
        }

        const default_language = 0x409;
        const default_memory_flags = 0x30;

        const type_value = type: {
            const resource_type = Resource.fromString(node.type.slice(self.source));
            if (resource_type != .user_defined) {
                if (res.RT.fromResource(resource_type)) |rt_constant| {
                    break :type NameOrOrdinal{ .ordinal = @enumToInt(rt_constant) };
                } else {
                    @panic("TODO: unhandled resource -> RT constant conversion");
                }
            } else {
                break :type try NameOrOrdinal.fromString(self.allocator, node.type.slice(self.source));
            }
        };
        defer type_value.deinit(self.allocator);

        // TODO: The type can change how the resource is written to the file
        //       (i.e. if it's an ICON then the contents should be parsed as a .ico, among
        //       other things)

        const name_value = try NameOrOrdinal.fromString(self.allocator, node.id.slice(self.source));
        defer name_value.deinit(self.allocator);

        const byte_length_up_to_name: u32 = 8 + name_value.byteLen() + type_value.byteLen();
        const padding_after_name = std.mem.alignForward(byte_length_up_to_name, 4) - byte_length_up_to_name;
        const header_size: u32 = byte_length_up_to_name + @intCast(u32, padding_after_name) + 16;

        try writer.writeIntLittle(DWORD, @intCast(u32, data_buffer.items.len)); // DataSize
        try writer.writeIntLittle(DWORD, header_size); // HeaderSize
        try type_value.write(writer); // TYPE
        try name_value.write(writer); // NAME
        try writer.writeByteNTimes(0, padding_after_name);

        try writer.writeIntLittle(DWORD, 0); // DataVersion
        try writer.writeIntLittle(WORD, default_memory_flags); // MemoryFlags
        try writer.writeIntLittle(WORD, default_language); // LanguageId
        try writer.writeIntLittle(DWORD, 0); // Version
        try writer.writeIntLittle(DWORD, 0); // Characteristics

        try writer.writeAll(data_buffer.items);

        const padding_after_data = std.mem.alignForward(data_buffer.items.len, 4) - data_buffer.items.len;
        try writer.writeByteNTimes(0, padding_after_data);
    }

    pub fn writeEmptyResource(writer: anytype) !void {
        try writer.writeIntLittle(DWORD, 0); // DataSize
        try writer.writeIntLittle(DWORD, 32); // HeaderSize
        try writer.writeIntLittle(WORD, 0xffff);
        try writer.writeIntLittle(WORD, 0x0000); // TYPE
        try writer.writeIntLittle(WORD, 0xffff);
        try writer.writeIntLittle(WORD, 0x0000); // NAME
        try writer.writeIntLittle(DWORD, 0); // DataVersion
        try writer.writeIntLittle(WORD, 0); // MemoryFlags
        try writer.writeIntLittle(WORD, 0); // LanguageId
        try writer.writeIntLittle(DWORD, 0); // Version
        try writer.writeIntLittle(DWORD, 0); // Characteristics
    }
};

fn testCompile(source: []const u8, cwd: std.fs.Dir) !void {
    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();

    try compile(std.testing.allocator, source, buffer.writer(), cwd);

    const expected_res = try getExpectedFromWindres(std.testing.allocator, source);
    defer std.testing.allocator.free(expected_res);

    try std.testing.expectEqualSlices(u8, expected_res, buffer.items);
}

fn testCompileWithOutput(source: []const u8, expected_output: []const u8, cwd: std.fs.Dir) !void {
    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();

    try compile(std.testing.allocator, source, buffer.writer(), cwd);

    std.testing.expectEqualSlices(u8, expected_output, buffer.items) catch |e| {
        std.debug.print("got:\n{}\n", .{std.zig.fmtEscapes(buffer.items)});
        std.debug.print("expected:\n{}\n", .{std.zig.fmtEscapes(expected_output)});
        return e;
    };
}

fn getExpectedFromWindres(allocator: Allocator, source: []const u8) ![]const u8 {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile("test.rc", source);

    var result = try std.ChildProcess.exec(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            // TODO: Don't hardcode this
            "C:\\Program Files (x86)\\Windows Kits\\10\\bin\\10.0.19041.0\\x86\\rc.exe",
            "test.rc",
        },
        .cwd = ("zig-cache/tmp/" ++ tmp.sub_path),
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    switch (result.term) {
        .Exited => |code| {
            if (code != 0) {
                std.debug.print("exit code: {}\n", .{result.term});
                std.debug.print("stdout: {s}\n", .{result.stdout});
                std.debug.print("stderr: {s}\n", .{result.stderr});
                return error.ExitCodeFailure;
            }
        },
        .Signal, .Stopped, .Unknown => {
            return error.ProcessTerminated;
        },
    }

    return tmp.dir.readFileAlloc(allocator, "test.res", std.math.maxInt(usize));
}

test "empty rc" {
    try testCompileWithOutput(
        "",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
}

test "basic rcdata" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile("file.bin", "hello world");

    try testCompileWithOutput(
        "1 RCDATA file.bin",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00hello world\x00",
        tmp_dir.dir,
    );
}

test "basic rcdata with empty raw data" {
    try testCompileWithOutput(
        "1 RCDATA {}",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
}

test "basic rcdata with raw data" {
    try testCompileWithOutput(
        "1 RCDATA { 1, \"2\", L\"3\" }",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x01\x0023\x00\x00\x00\x00",
        std.fs.cwd(),
    );
}

test "basic but with tricky type" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile("file.bin", "hello world");

    try testCompileWithOutput(
        "1 \"RCDATA\" file.bin",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x000\x00\x00\x00\"\x00R\x00C\x00D\x00A\x00T\x00A\x00\"\x00\x00\x00\xff\xff\x01\x00\x00\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00hello world\x00",
        tmp_dir.dir,
    );
}
