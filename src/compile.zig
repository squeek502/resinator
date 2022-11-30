const std = @import("std");
const Allocator = std.mem.Allocator;
const Node = @import("ast.zig").Node;
const Lexer = @import("lex.zig").Lexer;
const Parser = @import("parse.zig").Parser;
const Resource = @import("rc.zig").Resource;
const Token = @import("lex.zig").Token;
const Number = @import("literals.zig").Number;
const parseNumberLiteral = @import("literals.zig").parseNumberLiteral;
const parseQuotedAsciiString = @import("literals.zig").parseQuotedAsciiString;
const parseQuotedWideStringAlloc = @import("literals.zig").parseQuotedWideStringAlloc;
const Diagnostics = @import("errors.zig").Diagnostics;
const ErrorDetails = @import("errors.zig").ErrorDetails;
const MemoryFlags = @import("res.zig").MemoryFlags;
const rc = @import("rc.zig");
const res = @import("res.zig");
const ico = @import("ico.zig");
const WORD = std.os.windows.WORD;
const DWORD = std.os.windows.DWORD;

pub fn compile(allocator: Allocator, source: []const u8, writer: anytype, cwd: std.fs.Dir, diagnostics: *Diagnostics) !void {
    var lexer = Lexer.init(source);
    var parser = Parser.init(&lexer);
    var tree = try parser.parse(allocator, diagnostics);
    defer tree.deinit();

    var arena_allocator = std.heap.ArenaAllocator.init(allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    var compiler = Compiler{
        .source = source,
        .arena = arena,
        .allocator = allocator,
        .cwd = cwd,
        .diagnostics = diagnostics,
    };

    try compiler.writeRoot(tree.root(), writer);
}

pub const Compiler = struct {
    source: []const u8,
    arena: Allocator,
    allocator: Allocator,
    cwd: std.fs.Dir,
    state: State = .{},
    diagnostics: *Diagnostics,

    pub const State = struct {
        icon_id: u16 = 1,
        string_table: StringTable = .{},
    };

    pub fn writeRoot(self: *Compiler, root: *Node.Root, writer: anytype) !void {
        try writeEmptyResource(writer);
        for (root.body) |node| {
            try self.writeNode(node, writer);
        }

        // once we've written every else out, we can write out the finalized STRINGTABLE resources
        var string_table_it = self.state.string_table.blocks.iterator();
        while (string_table_it.next()) |entry| {
            try entry.value_ptr.writeResData(self, entry.key_ptr.*, writer);
        }
    }

    pub fn writeNode(self: *Compiler, node: *Node, writer: anytype) !void {
        switch (node.id) {
            .root => unreachable, // writeRoot should be called directly instead
            .resource_external => try self.writeResourceExternal(@fieldParentPtr(Node.ResourceExternal, "base", node), writer),
            .resource_raw_data => try self.writeResourceRawData(@fieldParentPtr(Node.ResourceRawData, "base", node), writer),
            .literal => unreachable, // this is context dependent and should be handled by its parent
            .binary_expression => @panic("TODO"),
            .grouped_expression => @panic("TODO"),
            .invalid => @panic("TODO"),
            .string_table => try self.writeStringTable(@fieldParentPtr(Node.StringTable, "base", node)),
            .string_table_string => unreachable, // handled by writeStringTable
            .language_statement => @panic("TODO"),
            .simple_statement => @panic("TODO"),
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
            var as_utf16 = try std.unicode.utf8ToUtf16LeWithNull(allocator, str);
            // Names have a limit of 256 UTF-16 code units + null terminator
            // Note: This can cut-off in the middle of a UTF-16, i.e. it can make the
            //       string end with an unpaired high surrogate
            if (as_utf16.len > 256) {
                var limited = allocator.shrink(as_utf16, 257);
                limited[256] = 0;
                as_utf16 = limited[0..256 :0];
            }
            // ASCII chars in names are always converted to uppercase
            for (as_utf16) |*char| {
                if (char.* < 128) {
                    char.* = std.ascii.toUpper(@intCast(u8, char.*));
                }
            }
            return NameOrOrdinal{ .name = as_utf16 };
        }

        pub fn maybeOrdinalFromString(str: []const u8) ?NameOrOrdinal {
            var buf = str;
            var radix: u8 = 10;
            if (buf.len > 2 and buf[0] == '0') {
                switch (str[1]) {
                    '0'...'9' => {},
                    'x', 'X' => {
                        radix = 16;
                        buf = buf[2..];
                        // only the first 4 hex digits matter, anything else is ignored
                        // i.e. 0x12345 is treated as if it were 0x1234
                        buf.len = @min(buf.len, 4);
                    },
                    else => return null,
                }
            }

            var result: u16 = 0;
            for (buf) |c| {
                const digit = std.fmt.charToDigit(c, radix) catch switch (radix) {
                    10 => return null,
                    // If this is hex, then non-hex-digits are treated as a terminator rather
                    // than an invalid number
                    16 => break,
                    else => unreachable,
                };

                if (result != 0) {
                    result *%= radix;
                }
                result +%= digit;
            }

            // Zero is not interpretted as a number
            if (result == 0) return null;
            return NameOrOrdinal{ .ordinal = result };
        }

        pub fn predefinedResourceType(self: NameOrOrdinal) ?res.RT {
            switch (self) {
                .ordinal => |ordinal| {
                    switch (@intToEnum(res.RT, ordinal)) {
                        .ACCELERATOR,
                        .ANICURSOR,
                        .ANIICON,
                        .BITMAP,
                        .CURSOR,
                        .DIALOG,
                        .DLGINCLUDE,
                        .FONT,
                        .FONTDIR,
                        .GROUP_CURSOR,
                        .GROUP_ICON,
                        .HTML,
                        .ICON,
                        .MANIFEST,
                        .MENU,
                        .MESSAGETABLE,
                        .PLUGPLAY,
                        .RCDATA,
                        .STRING,
                        .VERSION,
                        .VXD,
                        => |rt| return rt,
                        _ => return null,
                    }
                },
                .name => return null,
            }
        }
    };

    const Filename = struct {
        utf8: []const u8,
        needs_free: bool = false,

        pub fn deinit(self: Filename, allocator: Allocator) void {
            if (self.needs_free) {
                allocator.free(self.utf8);
            }
        }
    };

    pub fn evaluateFilenameExpression(self: *Compiler, expression_node: *Node) !Filename {
        switch (expression_node.id) {
            .literal => {
                const literal_node = expression_node.cast(.literal).?;
                switch (literal_node.token.id) {
                    .literal, .number => {
                        const literal_as_string = literal_node.token.slice(self.source);
                        return .{ .utf8 = literal_as_string };
                    },
                    .quoted_ascii_string => {
                        const slice = literal_node.token.slice(self.source);
                        const column = literal_node.token.calculateColumn(self.source, 8, null);
                        const parsed = try parseQuotedAsciiString(self.allocator, slice, column);
                        return .{ .utf8 = parsed, .needs_free = true };
                    },
                    .quoted_wide_string => {
                        // TODO: No need to parse this to UTF-16 and then back to UTF-8
                        // if it's already UTF-8. Should have a function that parses wide
                        // strings directly to UTF-8.
                        const slice = literal_node.token.slice(self.source);
                        const column = literal_node.token.calculateColumn(self.source, 8, null);
                        const parsed_string = try parseQuotedWideStringAlloc(self.allocator, slice, column);
                        defer self.allocator.free(parsed_string);
                        const parsed_as_utf8 = try std.unicode.utf16leToUtf8Alloc(self.allocator, parsed_string);
                        return .{ .utf8 = parsed_as_utf8, .needs_free = true };
                    },
                    else => unreachable, // no other token types should be in a filename literal node
                }
            },
            .binary_expression => {
                const binary_expression_node = expression_node.cast(.binary_expression).?;
                return self.evaluateFilenameExpression(binary_expression_node.right);
            },
            .grouped_expression => {
                const grouped_expression_node = expression_node.cast(.grouped_expression).?;
                return self.evaluateFilenameExpression(grouped_expression_node.expression);
            },
            else => unreachable,
        }
    }

    pub fn writeResourceExternal(self: *Compiler, node: *Node.ResourceExternal, writer: anytype) !void {
        const filename = try self.evaluateFilenameExpression(node.filename);
        defer filename.deinit(self.allocator);

        // TODO: emit error on file not found
        const file = self.cwd.openFile(filename.utf8, .{}) catch @panic("TODO openFile error handling");
        defer file.close();

        // Init header with data size zero for now, will need to fill it in later
        var header = ResourceHeader.init(self.allocator, node.id.slice(self.source), node.type.slice(self.source), 0) catch |err| switch (err) {
            error.StringResourceAsNumericType => {
                return self.failDetails(.{
                    .err = .string_resource_as_numeric_type,
                    .token = node.type,
                });
            },
            else => |e| return e,
        };
        defer header.deinit(self.allocator);

        if (header.predefinedResourceType()) |predefined_type| {
            switch (predefined_type) {
                .GROUP_ICON, .GROUP_CURSOR => {
                    const icon_dir = try ico.read(self.allocator, file.reader());
                    defer icon_dir.deinit();

                    // TODO: Could emit a warning if `icon_dir.image_type` doesn't match the predefined type.
                    //       The Windows RC compiler doesn't, but it seems like it'd be helpful.

                    // Memory flags only affect the RT_ICON, not the RT_GROUP_ICON
                    var icon_memory_flags = MemoryFlags.defaults(res.RT.ICON);
                    applyToMemoryFlags(&icon_memory_flags, node.common_resource_attributes, self.source);

                    const first_icon_id = self.state.icon_id;
                    const entry_type = if (predefined_type == .GROUP_ICON) @enumToInt(res.RT.ICON) else @enumToInt(res.RT.CURSOR);
                    for (icon_dir.entries) |entry| {
                        const image_header = ResourceHeader{
                            .type_value = .{ .ordinal = entry_type },
                            .name_value = .{ .ordinal = self.state.icon_id },
                            .data_size = entry.data_size_in_bytes,
                            .memory_flags = icon_memory_flags,
                        };
                        try image_header.write(writer);
                        try file.seekTo(entry.data_offset_from_start_of_file);
                        try writeResourceData(writer, file.reader(), image_header.data_size);
                        self.state.icon_id += 1;
                    }

                    // TODO: Move this in a function somewhere that makes sense
                    header.data_size = @intCast(u32, 6 + 14 * icon_dir.entries.len);

                    try header.write(writer);
                    try icon_dir.writeResData(writer, first_icon_id);
                    return;
                },
                .RCDATA, .HTML => {
                    header.applyMemoryFlags(node.common_resource_attributes, self.source);
                },
                .BITMAP => {
                    header.applyMemoryFlags(node.common_resource_attributes, self.source);
                    const file_size = try file.getEndPos();
                    const bmp_header_size = 14;
                    // TODO: error on invalid bmps
                    header.data_size = @intCast(u32, file_size) - bmp_header_size;
                    try header.write(writer);
                    try file.seekTo(bmp_header_size);
                    try writeResourceData(writer, file.reader(), header.data_size);
                    return;
                },
                else => {
                    std.debug.print("Type: {}\n", .{predefined_type});
                    @panic("TODO writeResourceExternal");
                },
            }
        } else {
            header.applyMemoryFlags(node.common_resource_attributes, self.source);
        }

        // Fallback to just writing out the entire contents of the file
        // TODO: How does the Windows RC compiler handle file sizes over u32 max?
        header.data_size = @intCast(u32, try file.getEndPos());
        try header.write(writer);
        try writeResourceData(writer, file.reader(), header.data_size);
    }

    pub const DataType = enum {
        number,
        ascii_string,
        wide_string,
    };

    pub const Data = union(DataType) {
        number: Number,
        ascii_string: []const u8,
        wide_string: []const u16,

        pub fn deinit(self: Data, allocator: Allocator) void {
            switch (self) {
                .wide_string => |wide_string| {
                    allocator.free(wide_string);
                },
                .ascii_string => |ascii_string| {
                    allocator.free(ascii_string);
                },
                else => {},
            }
        }

        pub fn write(self: Data, writer: anytype) !void {
            switch (self) {
                .number => |number| switch (number.is_long) {
                    false => try writer.writeIntLittle(WORD, number.asWord()),
                    true => try writer.writeIntLittle(DWORD, number.value),
                },
                .ascii_string => |ascii_string| {
                    try writer.writeAll(ascii_string);
                },
                .wide_string => |wide_string| {
                    try writer.writeAll(std.mem.sliceAsBytes(wide_string));
                },
            }
        }

        pub fn evaluateOperator(operator_char: u8, lhs: Data, rhs: Data) Data {
            std.debug.assert(lhs == .number);
            std.debug.assert(rhs == .number);
            const result = switch (operator_char) {
                '-' => lhs.number.value -% rhs.number.value,
                '+' => lhs.number.value +% rhs.number.value,
                '|' => lhs.number.value | rhs.number.value,
                '&' => lhs.number.value & rhs.number.value,
                else => unreachable, // invalid operator, this would be a lexer/parser bug
            };
            return .{ .number = .{
                .value = result,
                .is_long = lhs.number.is_long or rhs.number.is_long,
            } };
        }
    };

    pub fn evaluateDataExpression(self: *Compiler, expression_node: *Node) !Data {
        switch (expression_node.id) {
            .literal => {
                const literal_node = expression_node.cast(.literal).?;
                switch (literal_node.token.id) {
                    .number => {
                        const number = parseNumberLiteral(literal_node.token.slice(self.source));
                        return .{ .number = number };
                    },
                    .quoted_ascii_string => {
                        const slice = literal_node.token.slice(self.source);
                        const column = literal_node.token.calculateColumn(self.source, 8, null);
                        const parsed = try parseQuotedAsciiString(self.allocator, slice, column);
                        errdefer self.allocator.free(parsed);
                        return .{ .ascii_string = parsed };
                    },
                    .quoted_wide_string => {
                        const slice = literal_node.token.slice(self.source);
                        const column = literal_node.token.calculateColumn(self.source, 8, null);
                        const parsed_string = try parseQuotedWideStringAlloc(self.allocator, slice, column);
                        errdefer self.allocator.free(parsed_string);
                        return .{ .wide_string = parsed_string };
                    },
                    .close_paren => {
                        // A close paren as a data expression is a special case that is essentially
                        // skipped (i.e. it contributes no actual data value).
                        return .{ .ascii_string = "" };
                    },
                    else => {
                        std.debug.print("unexpected token in literal node: {}\n", .{literal_node.token});
                        unreachable; // no other token types should be in a data literal node
                    },
                }
            },
            .binary_expression => {
                const binary_expression_node = expression_node.cast(.binary_expression).?;
                const lhs = try self.evaluateDataExpression(binary_expression_node.left);
                defer lhs.deinit(self.allocator);
                const rhs = try self.evaluateDataExpression(binary_expression_node.right);
                defer rhs.deinit(self.allocator);
                const operator_char = binary_expression_node.operator.slice(self.source)[0];
                return Data.evaluateOperator(operator_char, lhs, rhs);
            },
            else => {
                std.debug.print("{}\n", .{expression_node.id});
                @panic("TODO: evaluateDataExpression");
            },
        }
    }

    pub fn writeResourceRawData(self: *Compiler, node: *Node.ResourceRawData, writer: anytype) !void {
        var data_buffer = std.ArrayList(u8).init(self.allocator);
        defer data_buffer.deinit();
        const data_writer = data_buffer.writer();

        for (node.raw_data) |expression| {
            const data = try self.evaluateDataExpression(expression);
            defer data.deinit(self.allocator);
            try data.write(data_writer);
        }

        try self.writeResourceHeader(writer, node.id, node.type, @intCast(u32, data_buffer.items.len), node.common_resource_attributes);

        var data_fbs = std.io.fixedBufferStream(data_buffer.items);
        try writeResourceData(writer, data_fbs.reader(), @intCast(u32, data_buffer.items.len));
    }

    pub fn writeResourceHeader(self: *Compiler, writer: anytype, id_token: Token, type_token: Token, data_size: u32, common_resource_attributes: []Token) !void {
        var header = ResourceHeader.init(self.allocator, id_token.slice(self.source), type_token.slice(self.source), data_size) catch |err| switch (err) {
            error.StringResourceAsNumericType => {
                return self.failDetails(.{
                    .err = .string_resource_as_numeric_type,
                    .token = type_token,
                });
            },
            else => |e| return e,
        };
        defer header.deinit(self.allocator);

        header.applyMemoryFlags(common_resource_attributes, self.source);

        try header.write(writer);
    }

    pub fn writeResourceData(writer: anytype, data_reader: anytype, data_size: u32) !void {
        var limited_reader = std.io.limitedReader(data_reader, data_size);

        const FifoBuffer = std.fifo.LinearFifo(u8, .{ .Static = 4096 });
        var fifo = FifoBuffer.init();
        try fifo.pump(limited_reader.reader(), writer);

        const padding_after_data = std.mem.alignForward(data_size, 4) - data_size;
        try writer.writeByteNTimes(0, padding_after_data);
    }

    pub fn writeStringTable(self: *Compiler, node: *Node.StringTable) !void {
        for (node.strings) |string_node| {
            const string = @fieldParentPtr(Node.StringTableString, "base", string_node);
            const string_id_data = try self.evaluateDataExpression(string.id);
            const string_id = string_id_data.number.asWord();

            self.state.string_table.set(self.arena, string_id, string.string, node.common_resource_attributes, self.source) catch |err| switch (err) {
                error.StringAlreadyDefined => {
                    try self.warnDetails(ErrorDetails{
                        .err = .string_already_defined,
                        .token = string.string, // TODO: point to id instead?
                        .extra = .{ .number = string_id },
                    });
                    const existing_definition = self.state.string_table.get(string_id).?;
                    return self.failDetails(ErrorDetails{
                        .err = .string_already_defined,
                        .type = .note,
                        .token = existing_definition, // TODO: point to id instead?
                        .extra = .{ .number = string_id },
                    });
                },
                error.OutOfMemory => |e| return e,
            };
        }
    }

    pub const ResourceHeader = struct {
        name_value: NameOrOrdinal,
        type_value: NameOrOrdinal,
        language: WORD = 0x409, // TODO
        memory_flags: MemoryFlags,
        data_size: DWORD,

        pub fn init(allocator: Allocator, id_slice: []const u8, type_slice: []const u8, data_size: DWORD) !ResourceHeader {
            const type_value = type: {
                const resource_type = Resource.fromString(type_slice);
                if (resource_type != .user_defined) {
                    if (res.RT.fromResource(resource_type)) |rt_constant| {
                        break :type NameOrOrdinal{ .ordinal = @enumToInt(rt_constant) };
                    } else {
                        @panic("TODO: unhandled resource -> RT constant conversion");
                    }
                } else {
                    break :type try NameOrOrdinal.fromString(allocator, type_slice);
                }
            };
            errdefer type_value.deinit(allocator);

            if (type_value == .ordinal and type_value.ordinal == @enumToInt(res.RT.STRING)) {
                return error.StringResourceAsNumericType;
            }

            const name_value = try NameOrOrdinal.fromString(allocator, id_slice);
            errdefer name_value.deinit(allocator);

            const predefined_resource_type = type_value.predefinedResourceType();

            return ResourceHeader{
                .name_value = name_value,
                .type_value = type_value,
                .data_size = data_size,
                .memory_flags = MemoryFlags.defaults(predefined_resource_type),
            };
        }

        pub fn deinit(self: ResourceHeader, allocator: Allocator) void {
            self.name_value.deinit(allocator);
            self.type_value.deinit(allocator);
        }

        pub fn write(self: ResourceHeader, writer: anytype) !void {
            const byte_length_up_to_name: u32 = 8 + self.name_value.byteLen() + self.type_value.byteLen();
            const padding_after_name = std.mem.alignForward(byte_length_up_to_name, 4) - byte_length_up_to_name;
            const header_size: u32 = byte_length_up_to_name + @intCast(u32, padding_after_name) + 16;

            try writer.writeIntLittle(DWORD, self.data_size); // DataSize
            try writer.writeIntLittle(DWORD, header_size); // HeaderSize
            try self.type_value.write(writer); // TYPE
            try self.name_value.write(writer); // NAME
            try writer.writeByteNTimes(0, padding_after_name);

            try writer.writeIntLittle(DWORD, 0); // DataVersion
            try writer.writeIntLittle(WORD, self.memory_flags.value); // MemoryFlags
            try writer.writeIntLittle(WORD, self.language); // LanguageId
            try writer.writeIntLittle(DWORD, 0); // Version
            try writer.writeIntLittle(DWORD, 0); // Characteristics
        }

        pub fn predefinedResourceType(self: ResourceHeader) ?res.RT {
            return self.type_value.predefinedResourceType();
        }

        pub fn applyMemoryFlags(self: *ResourceHeader, tokens: []Token, source: []const u8) void {
            applyToMemoryFlags(&self.memory_flags, tokens, source);
        }
    };

    fn applyToMemoryFlags(flags: *MemoryFlags, tokens: []Token, source: []const u8) void {
        for (tokens) |token| {
            const attribute = rc.CommonResourceAttributes.map.get(token.slice(source)).?;
            flags.set(attribute);
        }
    }

    pub fn writeEmptyResource(writer: anytype) !void {
        const header = ResourceHeader{
            .name_value = .{ .ordinal = 0 },
            .type_value = .{ .ordinal = 0 },
            .language = 0,
            .memory_flags = .{ .value = 0 },
            .data_size = 0,
        };
        try header.write(writer);
    }

    fn warnDetails(self: *Compiler, details: ErrorDetails) Allocator.Error!void {
        try self.diagnostics.append(details);
    }

    fn failDetails(self: *Compiler, details: ErrorDetails) error{ CompileError, OutOfMemory } {
        try self.warnDetails(details);
        return error.CompileError;
    }
};

pub const StringTable = struct {
    /// Blocks are written to the .res file in order depending on when the first string
    /// was added to the block (i.e. `STRINGTABLE { 16 "b" 0 "a" }` would then get written
    /// with block ID 2 (the one with "b") first and block ID 1 (the one with "a") second).
    /// Using an ArrayHashMap here gives us this property for free.
    blocks: std.AutoArrayHashMapUnmanaged(u16, Block) = .{},

    pub const Block = struct {
        string_tokens: std.ArrayListUnmanaged(Token) = .{},
        set_indexes: std.bit_set.IntegerBitSet(16) = .{ .mask = 0 },
        memory_flags: MemoryFlags = MemoryFlags.defaults(res.RT.STRING),

        /// Returns the index to insert the string into the `string_tokens` list.
        /// Returns null if the string should be appended.
        fn getInsertionIndex(self: *Block, index: u8) ?u8 {
            std.debug.assert(!self.set_indexes.isSet(index));

            const first_set = self.set_indexes.findFirstSet() orelse return null;
            if (first_set > index) return 0;

            const last_set = 15 - @clz(self.set_indexes.mask);
            if (index > last_set) return null;

            var bit = first_set + 1;
            var insertion_index: u8 = 1;
            while (bit != index) : (bit += 1) {
                if (self.set_indexes.isSet(bit)) insertion_index += 1;
            }
            return insertion_index;
        }

        fn getTokenIndex(self: *Block, string_index: u8) ?u8 {
            const count = self.string_tokens.items.len;
            if (count == 0) return null;
            if (count == 1) return 0;

            const first_set = self.set_indexes.findFirstSet() orelse unreachable;
            if (first_set == string_index) return 0;
            const last_set = 15 - @clz(self.set_indexes.mask);
            if (last_set == string_index) return @intCast(u8, count - 1);

            if (first_set == last_set) return null;

            var bit = first_set + 1;
            var token_index: u8 = 1;
            while (bit < last_set) : (bit += 1) {
                if (!self.set_indexes.isSet(bit)) continue;
                if (bit == string_index) return token_index;
                token_index += 1;
            }
            return null;
        }

        fn dump(self: *Block) void {
            var bit_it = self.set_indexes.iterator(.{});
            var string_index: usize = 0;
            while (bit_it.next()) |bit_index| {
                const token = self.string_tokens.items[string_index];
                std.debug.print("{}: [{}] {any}\n", .{ bit_index, string_index, token });
                string_index += 1;
            }
        }

        pub fn writeResData(self: *Block, compiler: *Compiler, block_id: u16, writer: anytype) !void {
            var data_buffer = std.ArrayList(u8).init(compiler.allocator);
            defer data_buffer.deinit();
            const data_writer = data_buffer.writer();

            var i: u8 = 0;
            var string_i: u8 = 0;
            while (true) : (i += 1) {
                if (!self.set_indexes.isSet(i)) {
                    try data_writer.writeIntLittle(u16, 0);
                    if (i == 15) break else continue;
                }

                const string_token = self.string_tokens.items[string_i];
                const slice = string_token.slice(compiler.source);
                const column = string_token.calculateColumn(compiler.source, 8, null);
                const utf16_string = utf16: {
                    switch (string_token.id) {
                        .quoted_ascii_string => {
                            const parsed = try parseQuotedAsciiString(compiler.allocator, slice, column);
                            defer compiler.allocator.free(parsed);
                            break :utf16 try std.unicode.utf8ToUtf16LeWithNull(compiler.allocator, parsed);
                        },
                        .quoted_wide_string => break :utf16 try parseQuotedWideStringAlloc(compiler.allocator, slice, column),
                        else => unreachable,
                    }
                };
                defer compiler.allocator.free(utf16_string);

                try data_writer.writeIntLittle(u16, @intCast(u16, utf16_string.len));
                for (utf16_string) |wc| {
                    try data_writer.writeIntLittle(u16, wc);
                }

                if (i == 15) break;
                string_i += 1;
            }

            const header = Compiler.ResourceHeader{
                .name_value = .{ .ordinal = block_id },
                .type_value = .{ .ordinal = @enumToInt(res.RT.STRING) },
                .memory_flags = self.memory_flags,
                .data_size = @intCast(u32, data_buffer.items.len),
            };
            try header.write(writer);

            var data_fbs = std.io.fixedBufferStream(data_buffer.items);
            try Compiler.writeResourceData(writer, data_fbs.reader(), @intCast(u32, data_buffer.items.len));
        }
    };

    pub fn deinit(self: *StringTable, allocator: Allocator) void {
        var it = self.blocks.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.string_tokens.deinit(allocator);
        }
        self.blocks.deinit(allocator);
    }

    const SetError = error{StringAlreadyDefined} || Allocator.Error;

    pub fn set(self: *StringTable, allocator: Allocator, id: u16, string_token: Token, common_resource_attributes: []Token, source: []const u8) SetError!void {
        const block_id = (id / 16) + 1;
        const string_index: u8 = @intCast(u8, id & 0xF);

        var get_or_put_result = try self.blocks.getOrPut(allocator, block_id);
        if (!get_or_put_result.found_existing) {
            get_or_put_result.value_ptr.* = Block{};
            Compiler.applyToMemoryFlags(&get_or_put_result.value_ptr.memory_flags, common_resource_attributes, source);
        } else {
            if (get_or_put_result.value_ptr.set_indexes.isSet(string_index)) {
                return error.StringAlreadyDefined;
            }
        }

        var block = get_or_put_result.value_ptr;
        if (block.getInsertionIndex(string_index)) |insertion_index| {
            try block.string_tokens.insert(allocator, insertion_index, string_token);
        } else {
            try block.string_tokens.append(allocator, string_token);
        }
        block.set_indexes.set(string_index);
    }

    pub fn get(self: *StringTable, id: u16) ?Token {
        const block_id = (id / 16) + 1;
        const string_index = @intCast(u8, id & 0xF);

        const block = self.blocks.getPtr(block_id) orelse return null;
        const token_index = block.getTokenIndex(string_index) orelse return null;
        return block.string_tokens.items[token_index];
    }

    pub fn dump(self: *StringTable) !void {
        var it = self.iterator();
        while (it.next()) |entry| {
            std.debug.print("block: {}\n", .{entry.key_ptr.*});
            entry.value_ptr.dump();
        }
    }
};

test "StringTable" {
    const S = struct {
        fn makeDummyToken(id: usize) Token {
            return Token{
                .id = .invalid,
                .start = id,
                .end = id,
                .line_number = id,
            };
        }
    };
    const allocator = std.testing.allocator;
    var string_table = StringTable{};
    defer string_table.deinit(allocator);

    // randomize an array of ids 0-99
    var ids = ids: {
        var buf: [100]u16 = undefined;
        var i: u16 = 0;
        while (i < buf.len) : (i += 1) {
            buf[i] = i;
        }
        break :ids buf;
    };
    var prng = std.rand.DefaultPrng.init(0);
    var random = prng.random();
    random.shuffle(u16, &ids);

    // set each one in the randomized order
    for (ids) |id| {
        try string_table.set(allocator, id, S.makeDummyToken(id), &.{}, "");
    }

    // make sure each one exists and is the right value when gotten
    var id: u16 = 0;
    while (id < 100) : (id += 1) {
        const dummy = S.makeDummyToken(id);
        try std.testing.expectError(error.StringAlreadyDefined, string_table.set(allocator, id, dummy, &.{}, ""));
        try std.testing.expectEqual(dummy, string_table.get(id).?);
    }

    // make sure non-existent string ids are not found
    try std.testing.expectEqual(@as(?Token, null), string_table.get(100));
}

fn testCompile(source: []const u8, cwd: std.fs.Dir) !void {
    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();

    var diagnostics = Diagnostics.init(std.testing.allocator);
    defer diagnostics.deinit();

    try compile(std.testing.allocator, source, buffer.writer(), cwd, &diagnostics);

    const expected_res = try getExpectedFromWindowsRC(std.testing.allocator, source);
    defer std.testing.allocator.free(expected_res);

    try std.testing.expectEqualSlices(u8, expected_res, buffer.items);
}

fn testCompileWithOutput(source: []const u8, expected_output: []const u8, cwd: std.fs.Dir) !void {
    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();

    var diagnostics = Diagnostics.init(std.testing.allocator);
    defer diagnostics.deinit();

    try compile(std.testing.allocator, source, buffer.writer(), cwd, &diagnostics);

    std.testing.expectEqualSlices(u8, expected_output, buffer.items) catch |e| {
        std.debug.print("got:\n{}\n", .{std.zig.fmtEscapes(buffer.items)});
        std.debug.print("expected:\n{}\n", .{std.zig.fmtEscapes(expected_output)});
        return e;
    };
}

pub fn getExpectedFromWindowsRCWithDir(allocator: Allocator, source: []const u8, cwd: std.fs.Dir, cwd_path: []const u8) ![]const u8 {
    try cwd.writeFile("test.rc", source);

    var result = try std.ChildProcess.exec(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            // TODO: Don't hardcode this
            "C:\\Program Files (x86)\\Windows Kits\\10\\bin\\10.0.19041.0\\x86\\rc.exe",
            "test.rc",
        },
        .cwd = cwd_path,
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

    return cwd.readFileAlloc(allocator, "test.res", std.math.maxInt(usize));
}

pub fn getExpectedFromWindowsRC(allocator: Allocator, source: []const u8) ![]const u8 {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    return getExpectedFromWindowsRCWithDir(allocator, source, tmp.dir, "zig-cache/tmp/" ++ tmp.sub_path);
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
    try testCompileWithOutput(
        "1 RCDATA \"file.bin\"",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00hello world\x00",
        tmp_dir.dir,
    );
    try testCompileWithOutput(
        "1 RCDATA L\"file.bin\"",
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

test "raw data with number expression" {
    try testCompileWithOutput(
        "1 RCDATA { 1+1 }",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00",
        std.fs.cwd(),
    );
    // overflow is wrapping
    try testCompileWithOutput(
        "1 RCDATA { 65535+1 }",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
    // binary operators promote to the largest size of their operands
    try testCompileWithOutput(
        "1 RCDATA { 65535 + 1L }",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00",
        std.fs.cwd(),
    );
}

test "filenames as numeric expressions" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile("-1", "hello world");
    try testCompileWithOutput(
        "1 RCDATA -1",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00hello world\x00",
        tmp_dir.dir,
    );
    try tmp_dir.dir.deleteFile("-1");

    try tmp_dir.dir.writeFile("~1", "hello world");
    try testCompileWithOutput(
        "1 RCDATA ~1",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00hello world\x00",
        tmp_dir.dir,
    );
    try tmp_dir.dir.deleteFile("~1");

    try tmp_dir.dir.writeFile("1", "hello world");
    try testCompileWithOutput(
        "1 RCDATA 1+1",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00hello world\x00",
        tmp_dir.dir,
    );
    try tmp_dir.dir.deleteFile("1");

    try tmp_dir.dir.writeFile("-1", "hello world");
    try testCompileWithOutput(
        "1 RCDATA 1+-1",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00hello world\x00",
        tmp_dir.dir,
    );
    try tmp_dir.dir.deleteFile("-1");

    try tmp_dir.dir.writeFile("-1", "hello world");
    try testCompileWithOutput(
        "1 RCDATA (1+-1)",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00hello world\x00",
        tmp_dir.dir,
    );
    try tmp_dir.dir.deleteFile("-1");
}

test "NameOrOrdinal" {
    // zero is treated as a string
    try testCompileWithOutput(
        "0 0 {}",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00 \x00\x00\x000\x00\x00\x000\x00\x00\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
    // hex zero is also treated as a string, hex numbers work though
    try testCompileWithOutput(
        "0x0 0x100 {}",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00$\x00\x00\x00\xff\xff\x00\x010\x00X\x000\x00\x00\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
    // only the first 4 hex digits matter
    try testCompileWithOutput(
        "0x12345 0X1234 {}",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00 \x00\x00\x00\xff\xff4\x12\xff\xff4\x12\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
    // unsupported radix and u16 overflow are treated as strings
    try testCompileWithOutput(
        "0o1234 65536 {}",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x004\x00\x00\x006\x005\x005\x003\x006\x00\x00\x000\x00O\x001\x002\x003\x004\x00\x00\x00\x00\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
    // non-hex-digits in a hex literal are treated as a terminator
    try testCompileWithOutput(
        "0x4n 0xFAZ92348 {}",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00 \x00\x00\x00\xff\xff\xfa\x00\xff\xff\x04\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
    // 0 at the start is allowed
    try testCompileWithOutput(
        "050 RCDATA {}",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff2\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
    // limit of 256 UTF-16 code units, can cut off between a surrogate pair
    // TODO: Will need fixing up once we handle different code pages, this assumes we treat the input as UTF-8
    try testCompileWithOutput(
        "00614982008907933748980730280674788429543776231864944218790698304852300002973622122844631429099469274282385299397783838528qffL7ShnSIETg0qkLr1UYpbtuv1PMFQRRa0VjDG354GQedJmUPgpp1w1ExVnTzVEiz6K3iPqM1AWGeYALmeODyvEZGOD3MfmGey8fnR4jUeTtB1PzdeWsNDrGzuA8Snxp3NGO𐐷 RCDATA {}",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00 \x02\x00\x00\xff\xff\n\x000\x000\x006\x001\x004\x009\x008\x002\x000\x000\x008\x009\x000\x007\x009\x003\x003\x007\x004\x008\x009\x008\x000\x007\x003\x000\x002\x008\x000\x006\x007\x004\x007\x008\x008\x004\x002\x009\x005\x004\x003\x007\x007\x006\x002\x003\x001\x008\x006\x004\x009\x004\x004\x002\x001\x008\x007\x009\x000\x006\x009\x008\x003\x000\x004\x008\x005\x002\x003\x000\x000\x000\x000\x002\x009\x007\x003\x006\x002\x002\x001\x002\x002\x008\x004\x004\x006\x003\x001\x004\x002\x009\x000\x009\x009\x004\x006\x009\x002\x007\x004\x002\x008\x002\x003\x008\x005\x002\x009\x009\x003\x009\x007\x007\x008\x003\x008\x003\x008\x005\x002\x008\x00Q\x00F\x00F\x00L\x007\x00S\x00H\x00N\x00S\x00I\x00E\x00T\x00G\x000\x00Q\x00K\x00L\x00R\x001\x00U\x00Y\x00P\x00B\x00T\x00U\x00V\x001\x00P\x00M\x00F\x00Q\x00R\x00R\x00A\x000\x00V\x00J\x00D\x00G\x003\x005\x004\x00G\x00Q\x00E\x00D\x00J\x00M\x00U\x00P\x00G\x00P\x00P\x001\x00W\x001\x00E\x00X\x00V\x00N\x00T\x00Z\x00V\x00E\x00I\x00Z\x006\x00K\x003\x00I\x00P\x00Q\x00M\x001\x00A\x00W\x00G\x00E\x00Y\x00A\x00L\x00M\x00E\x00O\x00D\x00Y\x00V\x00E\x00Z\x00G\x00O\x00D\x003\x00M\x00F\x00M\x00G\x00E\x00Y\x008\x00F\x00N\x00R\x004\x00J\x00U\x00E\x00T\x00T\x00B\x001\x00P\x00Z\x00D\x00E\x00W\x00S\x00N\x00D\x00R\x00G\x00Z\x00U\x00A\x008\x00S\x00N\x00X\x00P\x003\x00N\x00G\x00O\x00\x01\xd8\x00\x00\x00\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
}

test "case of string ids and user-defined types" {
    // All ASCII chars should be converted to uppercase
    try testCompileWithOutput(
        "lower lower {}",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x000\x00\x00\x00L\x00O\x00W\x00E\x00R\x00\x00\x00L\x00O\x00W\x00E\x00R\x00\x00\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
}

test "basic icons/cursors" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // TODO: If the .ico is malformed, then things get weird, i.e. if an entry's bits_per_pixel
    //       or num_colors or data_size is wrong (with respect to the bitmap data?) then the .res
    //       can get weird values written to the ICONDIR data for things like bits_per_pixel.

    // This is a well-formed .ico with a 1x1 bmp icon
    try tmp_dir.dir.writeFile("test.ico", "\x00\x00\x01\x00\x01\x00\x01\x01\x00\x00\x01\x00 \x000\x00\x00\x00\x16\x00\x00\x00(\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x01\x00 \x00\x00\x00\x00\x00\x04\x00\x00\x00\x12\x0b\x00\x00\x12\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\xff\xff\x00\x00\x00\x00");

    try testCompileWithOutput(
        "1 ICON test.ico",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x000\x00\x00\x00 \x00\x00\x00\xff\xff\x03\x00\xff\xff\x01\x00\x00\x00\x00\x00\x10\x10\t\x04\x00\x00\x00\x00\x00\x00\x00\x00(\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x01\x00 \x00\x00\x00\x00\x00\x04\x00\x00\x00\x12\x0b\x00\x00\x12\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\xff\xff\x00\x00\x00\x00\x14\x00\x00\x00 \x00\x00\x00\xff\xff\x0e\x00\xff\xff\x01\x00\x00\x00\x00\x000\x10\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x01\x00\x01\x01\x00\x00\x01\x00 \x000\x00\x00\x00\x01\x00",
        tmp_dir.dir,
    );

    // Cursors are just .ico files with a different image type, but they still compile even with the ICON image type
    try testCompileWithOutput(
        "1 CURSOR test.ico",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x000\x00\x00\x00 \x00\x00\x00\xff\xff\x01\x00\xff\xff\x01\x00\x00\x00\x00\x00\x10\x10\t\x04\x00\x00\x00\x00\x00\x00\x00\x00(\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x01\x00 \x00\x00\x00\x00\x00\x04\x00\x00\x00\x12\x0b\x00\x00\x12\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\xff\xff\x00\x00\x00\x00\x14\x00\x00\x00 \x00\x00\x00\xff\xff\x0c\x00\xff\xff\x01\x00\x00\x00\x00\x000\x10\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x01\x00\x01\x01\x00\x00\x01\x00 \x000\x00\x00\x00\x01\x00",
        tmp_dir.dir,
    );

    // Common resource attributes should be applies to the ICON but not the GROUP_ICON
    try testCompileWithOutput(
        "1 ICON FIXED test.ico",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x000\x00\x00\x00 \x00\x00\x00\xff\xff\x03\x00\xff\xff\x01\x00\x00\x00\x00\x00\x00\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00(\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x01\x00 \x00\x00\x00\x00\x00\x04\x00\x00\x00\x12\x0b\x00\x00\x12\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\xff\xff\x00\x00\x00\x00\x14\x00\x00\x00 \x00\x00\x00\xff\xff\x0e\x00\xff\xff\x01\x00\x00\x00\x00\x000\x10\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x01\x00\x01\x01\x00\x00\x01\x00 \x000\x00\x00\x00\x01\x00",
        tmp_dir.dir,
    );
}

test "basic bitmap" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // This is a well-formed .bmp with a 1x1 image
    try tmp_dir.dir.writeFile("test.bmp", "BM<\x00\x00\x00\x00\x00\x00\x006\x00\x00\x00(\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x10\x00\x00\x00\x00\x00\x06\x00\x00\x00\x12\x0b\x00\x00\x12\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\x7f\x00\x00\x00\x00");

    try testCompileWithOutput(
        "1 BITMAP test.bmp",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00.\x00\x00\x00 \x00\x00\x00\xff\xff\x02\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00(\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x10\x00\x00\x00\x00\x00\x06\x00\x00\x00\x12\x0b\x00\x00\x12\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\x7f\x00\x00\x00\x00\x00\x00",
        tmp_dir.dir,
    );
}

test "basic html" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile("test.html", "hello");
    const expected = "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00 \x00\x00\x00\xff\xff\x17\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00hello\x00\x00\x00";

    try testCompileWithOutput(
        "1 HTML { \"hello\" }",
        expected,
        tmp_dir.dir,
    );

    try testCompileWithOutput(
        "1 HTML test.html",
        expected,
        tmp_dir.dir,
    );
}

test "basic stringtable" {
    try testCompileWithOutput(
        "STRINGTABLE { 1, \"hello\" }",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00*\x00\x00\x00 \x00\x00\x00\xff\xff\x06\x00\xff\xff\x01\x00\x00\x00\x00\x000\x10\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00h\x00e\x00l\x00l\x00o\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );

    // overflow in string id, tab in string literal
    try testCompileWithOutput(
        "STRINGTABLE {    -1, \"\ta\" }",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00&\x00\x00\x00 \x00\x00\x00\xff\xff\x06\x00\xff\xff\x00\x10\x00\x00\x00\x000\x10\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00 \x00 \x00a\x00\x00\x00",
        std.fs.cwd(),
    );

    // order of RT_STRING resources in output, multiple RT_STRING blocks
    try testCompileWithOutput(
        \\STRINGTABLE { 512, "a" }
        \\1 RCDATA {}
        \\STRINGTABLE {
        \\  0, "b"
        \\  513, "c"
        \\}
        \\
    ,
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00$\x00\x00\x00 \x00\x00\x00\xff\xff\x06\x00\xff\xff!\x00\x00\x00\x00\x000\x10\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00a\x00\x01\x00c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\"\x00\x00\x00 \x00\x00\x00\xff\xff\x06\x00\xff\xff\x01\x00\x00\x00\x00\x000\x10\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );

    // The first STRINGTABLE that contains a string in a block dictates the memory flags
    // of the entire block
    try testCompileWithOutput(
        \\STRINGTABLE { 512, "a" }
        \\STRINGTABLE FIXED {
        \\  0, "b"
        \\  513, "c"
        \\}
        \\
    ,
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00$\x00\x00\x00 \x00\x00\x00\xff\xff\x06\x00\xff\xff!\x00\x00\x00\x00\x000\x10\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00a\x00\x01\x00c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\"\x00\x00\x00 \x00\x00\x00\xff\xff\x06\x00\xff\xff\x01\x00\x00\x00\x00\x00 \x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
}
