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
            .string_table => @panic("TODO"),
            .string_table_string => @panic("TODO"),
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
            // Zero is not interpretted as a number
            if (ordinal == 0) return null;
            return NameOrOrdinal{ .ordinal = ordinal };
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
                        try self.writeResourceData(writer, file.reader(), image_header.data_size);
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
                    try self.writeResourceData(writer, file.reader(), header.data_size);
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
        try self.writeResourceData(writer, file.reader(), header.data_size);
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
                    else => unreachable, // no other token types should be in a data literal node
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
        try self.writeResourceData(writer, data_fbs.reader(), @intCast(u32, data_buffer.items.len));
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

    pub fn writeResourceData(self: *Compiler, writer: anytype, data_reader: anytype, data_size: u32) !void {
        _ = self;
        var limited_reader = std.io.limitedReader(data_reader, data_size);

        const FifoBuffer = std.fifo.LinearFifo(u8, .{ .Static = 4096 });
        var fifo = FifoBuffer.init();
        try fifo.pump(limited_reader.reader(), writer);

        const padding_after_data = std.mem.alignForward(data_size, 4) - data_size;
        try writer.writeByteNTimes(0, padding_after_data);
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
    blocks: std.AutoHashMapUnmanaged(u16, Block) = .{},

    pub const Block = struct {
        string_tokens: std.ArrayListUnmanaged(Token) = .{},
        set_indexes: std.bit_set.IntegerBitSet(16) = .{ .mask = 0 },

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
    };

    pub fn deinit(self: *StringTable, allocator: Allocator) void {
        var it = self.blocks.valueIterator();
        while (it.next()) |v| {
            v.string_tokens.deinit(allocator);
        }
        self.blocks.deinit(allocator);
    }

    const SetError = error{StringAlreadyDefined} || Allocator.Error;

    pub fn set(self: *StringTable, allocator: Allocator, id: u16, string_token: Token) SetError!void {
        const block_id = (id / 16) + 1;
        const string_index: u8 = @intCast(u8, id & 0xF);

        var get_or_put_result = try self.blocks.getOrPut(allocator, block_id);
        if (!get_or_put_result.found_existing) {
            get_or_put_result.value_ptr.* = Block{};
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

    pub fn dump(self: *StringTable, allocator: Allocator) !void {
        var it = try self.inOrderIterator(allocator);
        defer it.deinit(allocator);
        while (it.next()) |entry| {
            std.debug.print("block: {}\n", .{entry.key_ptr.*});
            entry.value_ptr.dump();
        }
    }

    pub const InOrderIterator = struct {
        string_table: *const StringTable,
        sorted_keys: []u16,
        index: usize = 0,

        pub fn init(allocator: Allocator, string_table: *const StringTable) !InOrderIterator {
            var iterator = InOrderIterator{
                .string_table = string_table,
                .sorted_keys = try allocator.alloc(u16, string_table.blocks.count()),
            };
            var key_it = string_table.blocks.keyIterator();
            var key_i: usize = 0;
            while (key_it.next()) |key_ptr| {
                iterator.sorted_keys[key_i] = key_ptr.*;
                key_i += 1;
            }
            std.sort.sort(u16, iterator.sorted_keys, {}, std.sort.asc(u16));
            return iterator;
        }

        pub fn deinit(self: InOrderIterator, allocator: Allocator) void {
            allocator.free(self.sorted_keys);
        }

        pub fn next(self: *InOrderIterator) ?std.AutoHashMapUnmanaged(u16, Block).Entry {
            if (self.index >= self.sorted_keys.len) return null;
            var entry = self.string_table.blocks.getEntry(self.sorted_keys[self.index]);
            self.index += 1;
            return entry;
        }
    };

    pub fn inOrderIterator(self: *StringTable, allocator: Allocator) !InOrderIterator {
        return InOrderIterator.init(allocator, self);
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
        try string_table.set(allocator, id, S.makeDummyToken(id));
    }

    // make sure each one exists and is the right value when gotten
    var id: u16 = 0;
    while (id < 100) : (id += 1) {
        const dummy = S.makeDummyToken(id);
        try std.testing.expectError(error.StringAlreadyDefined, string_table.set(allocator, id, dummy));
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

test "zero as a NameOrOrdinal" {
    try testCompileWithOutput(
        "0 0 { \"hello\" }",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00 \x00\x00\x000\x00\x00\x000\x00\x00\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00hello\x00\x00\x00",
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
