const std = @import("std");
const Allocator = std.mem.Allocator;
const Node = @import("ast.zig").Node;
const Lexer = @import("lex.zig").Lexer;
const Parser = @import("parse.zig").Parser;
const WORD = std.os.windows.WORD;
const DWORD = std.os.windows.DWORD;

pub fn compile(allocator: Allocator, source: []const u8, writer: anytype) !void {
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
    };

    try compiler.writeRoot(tree.root(), writer);
}

pub const Compiler = struct {
    source: []const u8,
    arena: Allocator,
    allocator: Allocator,

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
        }
    }

    pub fn writeResourceExternal(self: *Compiler, node: *Node.ResourceExternal, writer: anytype) !void {
        const filename = node.filename.slice(self.source);
        // TODO: emit error on file not found
        const contents: []const u8 = std.fs.cwd().readFileAlloc(self.allocator, filename, std.math.maxInt(u32)) catch "";
        defer self.allocator.free(contents);

        const default_language = 0x409;
        const default_memory_flags = 0x30;

        // TODO: Support for more than just user-defined (string) types
        const type_byte_len = (node.type.slice(self.source).len + 1) * 2; // + 1 for 0-terminated, * 2 for UTF-16
        const type_as_utf16 = try std.unicode.utf8ToUtf16LeWithNull(self.allocator, node.type.slice(self.source));
        defer self.allocator.free(type_as_utf16);

        // TODO: Support for more than just int IDs
        const name_byte_len: u32 = 4;
        const name_int_id = std.fmt.parseUnsigned(u16, node.id.slice(self.source), 0) catch {
            @panic("TODO handle non-int names");
        };

        const byte_length_up_to_name: u32 = 8 + name_byte_len + @intCast(u32, type_byte_len);
        const padding_after_name = std.mem.alignForward(byte_length_up_to_name, 4) - byte_length_up_to_name;
        const header_size: u32 = byte_length_up_to_name + @intCast(u32, padding_after_name) + 16;

        try writer.writeIntLittle(DWORD, @intCast(u32, contents.len)); // DataSize
        try writer.writeIntLittle(DWORD, header_size); // HeaderSize
        try writer.writeAll(std.mem.sliceAsBytes(type_as_utf16[0 .. type_as_utf16.len + 1])); // TYPE
        try writer.writeIntLittle(WORD, 0xffff);
        try writer.writeIntLittle(WORD, name_int_id); // NAME
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

fn testCompile(source: []const u8) !void {
    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();

    try compile(std.testing.allocator, source, buffer.writer());

    const expected_res = try getExpectedFromWindres(std.testing.allocator, source);
    defer std.testing.allocator.free(expected_res);

    try std.testing.expectEqualSlices(u8, expected_res, buffer.items);
}

fn testCompileWithOutput(source: []const u8, expected_output: []const u8) !void {
    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();

    try compile(std.testing.allocator, source, buffer.writer());

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
    );
}

test "basic but with tricky type" {
    try testCompileWithOutput(
        "1 \"RCDATA\" file.bin",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x000\x00\x00\x00\"\x00R\x00C\x00D\x00A\x00T\x00A\x00\"\x00\x00\x00\xff\xff\x01\x00\x00\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00hello world\x00",
    );
}
