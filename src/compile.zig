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
const utils = @import("utils.zig");
const NameOrOrdinal = res.NameOrOrdinal;

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
        string_tables: StringTablesByLanguage = .{},
        language: res.Language = .{},
        font_dir: FontDir = .{},
    };

    pub fn writeRoot(self: *Compiler, root: *Node.Root, writer: anytype) !void {
        try writeEmptyResource(writer);
        for (root.body) |node| {
            try self.writeNode(node, writer);
        }

        // now write the FONTDIR (if it has anything in it)
        try self.state.font_dir.writeResData(self, writer);
        // once we've written every else out, we can write out the finalized STRINGTABLE resources
        var string_tables_it = self.state.string_tables.tables.iterator();
        while (string_tables_it.next()) |string_table_entry| {
            var string_table_it = string_table_entry.value_ptr.blocks.iterator();
            while (string_table_it.next()) |entry| {
                try entry.value_ptr.writeResData(self, string_table_entry.key_ptr.*, entry.key_ptr.*, writer);
            }
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
            .accelerators => try self.writeAccelerators(@fieldParentPtr(Node.Accelerators, "base", node), writer),
            .accelerator => unreachable, // handled by writeAccelerators
            .string_table => try self.writeStringTable(@fieldParentPtr(Node.StringTable, "base", node)),
            .string_table_string => unreachable, // handled by writeStringTable
            .language_statement => self.writeLanguageStatement(@fieldParentPtr(Node.LanguageStatement, "base", node)),
            .simple_statement => @panic("TODO"),
        }
    }

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
        const file = self.cwd.openFile(filename.utf8, .{}) catch |err| {
            const filename_string_index = @intCast(
                ErrorDetails.FileOpenError.FilenameStringIndex,
                try self.diagnostics.putString(filename.utf8),
            );
            return self.addErrorDetailsAndFail(.{
                .err = .file_open_error,
                // TODO get the most relevant token for filename, e.g. in an expression like (1+-1), get the -1 token
                .token = node.filename.getFirstToken(),
                .extra = .{ .file_open_error = .{
                    .err = ErrorDetails.FileOpenError.enumFromError(err),
                    .filename_string_index = filename_string_index,
                } },
            });
        };
        defer file.close();

        // Init header with data size zero for now, will need to fill it in later
        var header = try ResourceHeader.init(self.allocator, node.id.slice(self.source), node.type.slice(self.source), 0, self.state.language);
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
                            .language = self.state.language,
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
                .RCDATA, .HTML, .MANIFEST => {
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
                .FONT => {
                    if (self.state.font_dir.ids.get(header.name_value.ordinal) != null) {
                        // Add warning and skip this resource
                        try self.addErrorDetails(ErrorDetails{
                            .err = .font_id_already_defined,
                            .token = node.id,
                            .type = .warning,
                            .extra = .{ .number = header.name_value.ordinal },
                        });
                        try self.addErrorDetails(ErrorDetails{
                            .err = .font_id_already_defined,
                            .token = self.state.font_dir.ids.get(header.name_value.ordinal).?,
                            .type = .note,
                            .extra = .{ .number = header.name_value.ordinal },
                        });
                        return;
                    }
                    header.applyMemoryFlags(node.common_resource_attributes, self.source);
                    const file_size = try file.getEndPos();
                    // TODO: Error on too large files?
                    header.data_size = @intCast(u32, file_size);
                    try header.write(writer);

                    // TODO: This is much weirder than just the first 150 bytes for certain
                    //       file contents, need to investigate more to understand what should
                    //       actually be happening here
                    var header_slurping_reader = utils.headerSlurpingReader(150, file.reader());
                    try writeResourceData(writer, header_slurping_reader.reader(), header.data_size);

                    try self.state.font_dir.add(self.arena, FontDir.Font{
                        .id = header.name_value.ordinal,
                        .header_bytes = header_slurping_reader.slurped_header,
                    }, node.id);
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
    };

    /// Assumes that the node is a number or number expression
    fn evaluateNumberExpression(expression_node: *Node, source: []const u8) Number {
        switch (expression_node.id) {
            .literal => {
                const literal_node = expression_node.cast(.literal).?;
                std.debug.assert(literal_node.token.id == .number);
                return parseNumberLiteral(literal_node.token.slice(source));
            },
            .binary_expression => {
                const binary_expression_node = expression_node.cast(.binary_expression).?;
                const lhs = evaluateNumberExpression(binary_expression_node.left, source);
                const rhs = evaluateNumberExpression(binary_expression_node.right, source);
                const operator_char = binary_expression_node.operator.slice(source)[0];
                return lhs.evaluateOperator(operator_char, rhs);
            },
            .grouped_expression => {
                const grouped_expression_node = expression_node.cast(.grouped_expression).?;
                return evaluateNumberExpression(grouped_expression_node.expression, source);
            },
            else => unreachable,
        }
    }

    pub fn evaluateDataExpression(self: *Compiler, expression_node: *Node) !Data {
        switch (expression_node.id) {
            .literal => {
                const literal_node = expression_node.cast(.literal).?;
                switch (literal_node.token.id) {
                    .number => {
                        const number = evaluateNumberExpression(expression_node, self.source);
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
            .binary_expression, .grouped_expression => {
                const result = evaluateNumberExpression(expression_node, self.source);
                return .{ .number = result };
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

        try self.writeResourceHeader(writer, node.id, node.type, @intCast(u32, data_buffer.items.len), node.common_resource_attributes, self.state.language);

        var data_fbs = std.io.fixedBufferStream(data_buffer.items);
        try writeResourceData(writer, data_fbs.reader(), @intCast(u32, data_buffer.items.len));
    }

    pub fn writeResourceHeader(self: *Compiler, writer: anytype, id_token: Token, type_token: Token, data_size: u32, common_resource_attributes: []Token, language: res.Language) !void {
        var header = try ResourceHeader.init(self.allocator, id_token.slice(self.source), type_token.slice(self.source), data_size, language);
        defer header.deinit(self.allocator);

        header.applyMemoryFlags(common_resource_attributes, self.source);

        try header.write(writer);
    }

    pub fn writeResourceData(writer: anytype, data_reader: anytype, data_size: u32) !void {
        var limited_reader = std.io.limitedReader(data_reader, data_size);

        const FifoBuffer = std.fifo.LinearFifo(u8, .{ .Static = 4096 });
        var fifo = FifoBuffer.init();
        try fifo.pump(limited_reader.reader(), writer);

        try writeDataPadding(writer, data_size);
    }

    pub fn writeDataPadding(writer: anytype, data_size: u32) !void {
        const padding_after_data = std.mem.alignForward(data_size, 4) - data_size;
        try writer.writeByteNTimes(0, padding_after_data);
    }

    pub fn evaluateAcceleratorKeyExpression(self: *Compiler, node: *Node, is_virt: bool) !u16 {
        if (node.isNumberExpression()) {
            return evaluateNumberExpression(node, self.source).asWord();
        } else {
            std.debug.assert(node.isStringLiteral());
            const literal = @fieldParentPtr(Node.Literal, "base", node);
            var slice = literal.token.slice(self.source);
            // TODO: Is this okay?
            if (literal.token.id == .quoted_wide_string) {
                // remove the L/l prefix
                slice = slice[1..];
            }
            const parsed_string = try parseQuotedAsciiString(self.allocator, slice, literal.token.calculateColumn(self.source, 8, null));
            defer self.allocator.free(parsed_string);
            return res.parseAcceleratorKeyString(parsed_string, is_virt);
        }
    }

    pub fn writeAccelerators(self: *Compiler, node: *Node.Accelerators, writer: anytype) !void {
        var data_buffer = std.ArrayList(u8).init(self.allocator);
        defer data_buffer.deinit();
        const data_writer = data_buffer.writer();

        for (node.accelerators) |accel_node, i| {
            const accelerator = @fieldParentPtr(Node.Accelerator, "base", accel_node);
            var modifiers = res.AcceleratorModifiers{};
            for (accelerator.type_and_options) |type_or_option| {
                const modifier = rc.AcceleratorTypeAndOptions.map.get(type_or_option.slice(self.source)).?;
                modifiers.apply(modifier);
            }
            if (accelerator.event.isNumberExpression() and !modifiers.isSet(.ascii) and !modifiers.isSet(.virtkey)) {
                return self.addErrorDetailsAndFail(.{
                    .err = .accelerator_type_required,
                    .token = accelerator.event.getFirstToken(),
                });
            }
            const key = self.evaluateAcceleratorKeyExpression(accelerator.event, modifiers.isSet(.virtkey)) catch {
                // TODO: better error with more context from the caught error
                return self.addErrorDetailsAndFail(.{
                    .err = .invalid_accelerator_key,
                    .token = accelerator.event.getFirstToken(),
                });
            };
            const cmd_id = evaluateNumberExpression(accelerator.idvalue, self.source);

            if (i == node.accelerators.len - 1) {
                modifiers.markLast();
            }

            try data_writer.writeByte(modifiers.value);
            try data_writer.writeByte(0); // padding
            try data_writer.writeIntLittle(u16, key);
            try data_writer.writeIntLittle(u16, cmd_id.asWord());
            try data_writer.writeIntLittle(u16, 0); // padding
        }

        const data_size = @intCast(u32, data_buffer.items.len);
        var header = try ResourceHeader.init(self.allocator, node.id.slice(self.source), node.type.slice(self.source), data_size, self.state.language);
        defer header.deinit(self.allocator);

        header.applyMemoryFlags(node.common_resource_attributes, self.source);
        header.applyOptionalStatements(node.optional_statements, self.source);

        try header.write(writer);

        var data_fbs = std.io.fixedBufferStream(data_buffer.items);
        try writeResourceData(writer, data_fbs.reader(), data_size);
    }

    pub fn writeStringTable(self: *Compiler, node: *Node.StringTable) !void {
        const language = getLanguageFromOptionalStatements(node.optional_statements, self.source) orelse self.state.language;

        for (node.strings) |string_node| {
            const string = @fieldParentPtr(Node.StringTableString, "base", string_node);
            const string_id_data = try self.evaluateDataExpression(string.id);
            const string_id = string_id_data.number.asWord();

            self.state.string_tables.set(self.arena, language, string_id, string.string, &node.base, self.source) catch |err| switch (err) {
                error.StringAlreadyDefined => {
                    try self.addErrorDetails(ErrorDetails{
                        .err = .string_already_defined,
                        .token = string.string, // TODO: point to id instead?
                        .extra = .{ .string_and_language = .{ .id = string_id, .language = language } },
                    });
                    const existing_def_table = self.state.string_tables.tables.getPtr(language).?;
                    const existing_definition = existing_def_table.get(string_id).?;
                    return self.addErrorDetailsAndFail(ErrorDetails{
                        .err = .string_already_defined,
                        .type = .note,
                        .token = existing_definition, // TODO: point to id instead?
                        .extra = .{ .string_and_language = .{ .id = string_id, .language = language } },
                    });
                },
                error.OutOfMemory => |e| return e,
            };
        }
    }

    /// Expects this to be a top-level LANGUAGE statement
    pub fn writeLanguageStatement(self: *Compiler, node: *Node.LanguageStatement) void {
        const primary = Compiler.evaluateNumberExpression(node.primary_language_id, self.source);
        const sublanguage = Compiler.evaluateNumberExpression(node.sublanguage_id, self.source);
        self.state.language.primary_language_id = @truncate(u10, primary.value);
        self.state.language.sublanguage_id = @truncate(u6, sublanguage.value);
    }

    pub const ResourceHeader = struct {
        name_value: NameOrOrdinal,
        type_value: NameOrOrdinal,
        language: res.Language,
        memory_flags: MemoryFlags,
        data_size: DWORD,
        version: DWORD = 0,
        characteristics: DWORD = 0,

        pub fn init(allocator: Allocator, id_slice: []const u8, type_slice: []const u8, data_size: DWORD, language: res.Language) !ResourceHeader {
            const type_value = type: {
                const resource_type = Resource.fromString(type_slice);
                if (resource_type != .user_defined) {
                    if (res.RT.fromResource(resource_type)) |rt_constant| {
                        break :type NameOrOrdinal{ .ordinal = @enumToInt(rt_constant) };
                    } else {
                        @panic("TODO: unhandled resource -> RT constant conversion");
                    }
                } else {
                    break :type try NameOrOrdinal.fromString(allocator, type_slice, false);
                }
            };
            errdefer type_value.deinit(allocator);

            const name_value = try NameOrOrdinal.fromString(allocator, id_slice, true);
            errdefer name_value.deinit(allocator);

            const predefined_resource_type = type_value.predefinedResourceType();

            return ResourceHeader{
                .name_value = name_value,
                .type_value = type_value,
                .data_size = data_size,
                .memory_flags = MemoryFlags.defaults(predefined_resource_type),
                .language = language,
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
            try writer.writeIntLittle(WORD, @bitCast(WORD, self.language)); // LanguageId
            try writer.writeIntLittle(DWORD, self.version); // Version
            try writer.writeIntLittle(DWORD, self.characteristics); // Characteristics
        }

        pub fn predefinedResourceType(self: ResourceHeader) ?res.RT {
            return self.type_value.predefinedResourceType();
        }

        pub fn applyMemoryFlags(self: *ResourceHeader, tokens: []Token, source: []const u8) void {
            applyToMemoryFlags(&self.memory_flags, tokens, source);
        }

        pub fn applyOptionalStatements(self: *ResourceHeader, statements: []*Node, source: []const u8) void {
            applyToOptionalStatements(&self.language, &self.version, &self.characteristics, statements, source);
        }
    };

    fn applyToMemoryFlags(flags: *MemoryFlags, tokens: []Token, source: []const u8) void {
        for (tokens) |token| {
            const attribute = rc.CommonResourceAttributes.map.get(token.slice(source)).?;
            flags.set(attribute);
        }
    }

    fn applyToOptionalStatements(language: *res.Language, version: *u32, characteristics: *u32, statements: []*Node, source: []const u8) void {
        for (statements) |node| switch (node.id) {
            .language_statement => {
                const language_statement = @fieldParentPtr(Node.LanguageStatement, "base", node);
                language.* = languageFromLanguageStatement(language_statement, source);
            },
            .simple_statement => {
                const simple_statement = @fieldParentPtr(Node.SimpleStatement, "base", node);
                const result = Compiler.evaluateNumberExpression(simple_statement.value, source);
                const statement_type = rc.OptionalStatements.map.get(simple_statement.identifier.slice(source)).?;
                switch (statement_type) {
                    .version => version.* = result.value,
                    .characteristics => characteristics.* = result.value,
                    else => unreachable, // only VERSION and CHARACTERISTICS should be in an optional statements list
                }
            },
            else => unreachable, // no other node types should be in an optional statements list
        };
    }

    pub fn languageFromLanguageStatement(language_statement: *const Node.LanguageStatement, source: []const u8) res.Language {
        const primary = Compiler.evaluateNumberExpression(language_statement.primary_language_id, source);
        const sublanguage = Compiler.evaluateNumberExpression(language_statement.sublanguage_id, source);
        return .{
            .primary_language_id = @truncate(u10, primary.value),
            .sublanguage_id = @truncate(u6, sublanguage.value),
        };
    }

    pub fn getLanguageFromOptionalStatements(statements: []*Node, source: []const u8) ?res.Language {
        for (statements) |node| switch (node.id) {
            .language_statement => {
                const language_statement = @fieldParentPtr(Node.LanguageStatement, "base", node);
                return languageFromLanguageStatement(language_statement, source);
            },
            else => continue,
        };
        return null;
    }

    pub fn writeEmptyResource(writer: anytype) !void {
        const header = ResourceHeader{
            .name_value = .{ .ordinal = 0 },
            .type_value = .{ .ordinal = 0 },
            .language = .{
                .primary_language_id = 0,
                .sublanguage_id = 0,
            },
            .memory_flags = .{ .value = 0 },
            .data_size = 0,
        };
        try header.write(writer);
    }

    fn addErrorDetails(self: *Compiler, details: ErrorDetails) Allocator.Error!void {
        try self.diagnostics.append(details);
    }

    fn addErrorDetailsAndFail(self: *Compiler, details: ErrorDetails) error{ CompileError, OutOfMemory } {
        try self.addErrorDetails(details);
        return error.CompileError;
    }
};

pub const FontDir = struct {
    fonts: std.ArrayListUnmanaged(Font) = .{},
    /// To keep track of which ids are set and where they were set from
    ids: std.AutoHashMapUnmanaged(u16, Token) = .{},

    pub const Font = struct {
        id: u16,
        header_bytes: [150]u8,
    };

    pub fn deinit(self: *FontDir, allocator: Allocator) void {
        self.fonts.deinit(allocator);
    }

    pub fn add(self: *FontDir, allocator: Allocator, font: Font, id_token: Token) !void {
        try self.ids.putNoClobber(allocator, font.id, id_token);
        try self.fonts.append(allocator, font);
    }

    pub fn writeResData(self: *FontDir, compiler: *Compiler, writer: anytype) !void {
        if (self.fonts.items.len == 0) return;

        // u16 count + [(u16 id + 150 bytes) for each font]
        const data_size = 2 + (2 + 150) * self.fonts.items.len;
        var header = Compiler.ResourceHeader{
            .name_value = try NameOrOrdinal.nameFromString(compiler.allocator, "FONTDIR"),
            .type_value = NameOrOrdinal{ .ordinal = @enumToInt(res.RT.FONTDIR) },
            .memory_flags = res.MemoryFlags.defaults(res.RT.FONTDIR),
            .language = compiler.state.language,
            .data_size = @intCast(u32, data_size),
        };
        defer header.deinit(compiler.allocator);

        try header.write(writer);
        try writer.writeIntLittle(u16, @intCast(u16, self.fonts.items.len));
        for (self.fonts.items) |font| {
            try writer.writeIntLittle(u16, font.id);
            try writer.writeAll(&font.header_bytes);
        }
        try Compiler.writeDataPadding(writer, @intCast(u32, data_size));
    }
};

pub const StringTablesByLanguage = struct {
    /// String tables for each language are written to the .res file in order depending on
    /// when the first STRINGTABLE for the language was defined, and all blocks for a given
    /// language are written contiguously.
    /// Using an ArrayHashMap here gives us this property for free.
    tables: std.AutoArrayHashMapUnmanaged(res.Language, StringTable) = .{},

    pub fn deinit(self: *StringTablesByLanguage, allocator: Allocator) void {
        self.tables.deinit(allocator);
    }

    pub fn set(self: *StringTablesByLanguage, allocator: Allocator, language: res.Language, id: u16, string_token: Token, node: *Node, source: []const u8) StringTable.SetError!void {
        var get_or_put_result = try self.tables.getOrPut(allocator, language);
        if (!get_or_put_result.found_existing) {
            get_or_put_result.value_ptr.* = StringTable{};
        }
        return get_or_put_result.value_ptr.set(allocator, id, string_token, node, source);
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
        characteristics: u32 = 0,
        version: u32 = 0,

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

        pub fn applyNodeAttributes(self: *Block, node: *Node, source: []const u8) void {
            switch (node.id) {
                .string_table => {
                    const string_table = @fieldParentPtr(Node.StringTable, "base", node);
                    Compiler.applyToMemoryFlags(&self.memory_flags, string_table.common_resource_attributes, source);
                    var dummy_language: res.Language = undefined;
                    Compiler.applyToOptionalStatements(&dummy_language, &self.version, &self.characteristics, string_table.optional_statements, source);
                },
                else => @panic("TODO applyNodeAttributes"),
            }
        }

        pub fn writeResData(self: *Block, compiler: *Compiler, language: res.Language, block_id: u16, writer: anytype) !void {
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
                .language = language,
                .version = self.version,
                .characteristics = self.characteristics,
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

    pub fn set(self: *StringTable, allocator: Allocator, id: u16, string_token: Token, node: *Node, source: []const u8) SetError!void {
        const block_id = (id / 16) + 1;
        const string_index: u8 = @intCast(u8, id & 0xF);

        var get_or_put_result = try self.blocks.getOrPut(allocator, block_id);
        if (!get_or_put_result.found_existing) {
            get_or_put_result.value_ptr.* = Block{};
            get_or_put_result.value_ptr.applyNodeAttributes(node, source);
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

    var dummy_node = Node.StringTable{
        .type = S.makeDummyToken(0),
        .common_resource_attributes = &.{},
        .optional_statements = &.{},
        .begin_token = S.makeDummyToken(0),
        .strings = &.{},
        .end_token = S.makeDummyToken(0),
    };

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
        try string_table.set(allocator, id, S.makeDummyToken(id), &dummy_node.base, "");
    }

    // make sure each one exists and is the right value when gotten
    var id: u16 = 0;
    while (id < 100) : (id += 1) {
        const dummy = S.makeDummyToken(id);
        try std.testing.expectError(error.StringAlreadyDefined, string_table.set(allocator, id, dummy, &dummy_node.base, ""));
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

    try @import("utils.zig").testing.expectEqualBytes(expected_res, buffer.items);
}

fn testCompileWithOutput(source: []const u8, expected_output: []const u8, cwd: std.fs.Dir) !void {
    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();

    var diagnostics = Diagnostics.init(std.testing.allocator);
    defer diagnostics.deinit();

    compile(std.testing.allocator, source, buffer.writer(), cwd, &diagnostics) catch |err| switch (err) {
        error.ParseError, error.CompileError => {
            diagnostics.renderToStdErr(cwd, source, null);
            return err;
        },
        else => return err,
    };

    try @import("utils.zig").testing.expectEqualBytes(expected_output, buffer.items);
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
    // grouped expression
    try testCompileWithOutput(
        "1 RCDATA { (65535 + 1L) }",
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

// TODO: Move to testing NameOrOrdinal directly, no need for this to be coupled to the Compiler
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
        "0o1234 65537 {}",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x004\x00\x00\x006\x005\x005\x003\x007\x00\x00\x000\x00O\x001\x002\x003\x004\x00\x00\x00\x00\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
    // overflow *is* allowed in ids
    try testCompileWithOutput(
        "65537 RCDATA {}",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00",
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

test "stringtable optional-statements" {
    try testCompileWithOutput(
        "STRINGTABLE VERSION 1 CHARACTERISTICS 65536 VERSION 2 { 0 \"hello\" }",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00*\x00\x00\x00 \x00\x00\x00\xff\xff\x06\x00\xff\xff\x01\x00\x00\x00\x00\x000\x10\t\x04\x02\x00\x00\x00\x00\x00\x01\x00\x05\x00h\x00e\x00l\x00l\x00o\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );

    // 63 is the max sublanguage id (u6), so 65 will overflow
    try testCompileWithOutput(
        "STRINGTABLE LANGUAGE 1, 65 { 0 \"hello\" }",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00*\x00\x00\x00 \x00\x00\x00\xff\xff\x06\x00\xff\xff\x01\x00\x00\x00\x00\x000\x10\x01\x04\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00h\x00e\x00l\x00l\x00o\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
}

test "separate stringtable per language" {
    try testCompileWithOutput(
        \\STRINGTABLE LANGUAGE 0,0 { 0 "hello" }
        \\STRINGTABLE LANGUAGE 0,1 { 0 "hello" }
    ,
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00*\x00\x00\x00 \x00\x00\x00\xff\xff\x06\x00\xff\xff\x01\x00\x00\x00\x00\x000\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00h\x00e\x00l\x00l\x00o\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00*\x00\x00\x00 \x00\x00\x00\xff\xff\x06\x00\xff\xff\x01\x00\x00\x00\x00\x000\x10\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00h\x00e\x00l\x00l\x00o\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
}

test "case insensitivity" {
    try testCompileWithOutput(
        "StringTABLE VERSION 1 characteristics 65536 Version 2 Begin 0 \"hello\" end",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00*\x00\x00\x00 \x00\x00\x00\xff\xff\x06\x00\xff\xff\x01\x00\x00\x00\x00\x000\x10\t\x04\x02\x00\x00\x00\x00\x00\x01\x00\x05\x00h\x00e\x00l\x00l\x00o\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
}

test "top-level language statements" {
    try testCompileWithOutput(
        \\1 RCDATA {}
        \\LANGUAGE 1,1
        \\2 RCDATA {}
        \\LANGUAGE 0,0
        \\3 RCDATA {}
    ,
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x02\x00\x00\x00\x00\x000\x00\x01\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x03\x00\x00\x00\x00\x000\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
    try testCompileWithOutput(
        \\LANGUAGE 1,1
        \\STRINGTABLE { 0 "hello" }
    ,
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00*\x00\x00\x00 \x00\x00\x00\xff\xff\x06\x00\xff\xff\x01\x00\x00\x00\x00\x000\x10\x01\x04\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00h\x00e\x00l\x00l\x00o\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
}

test "font resource" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile("empty.fnt", "");

    const expected = "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x08\x00\xff\xff\x01\x00\x00\x00\x00\x00 \x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x08\x00\xff\xff\x02\x00\x00\x00\x00\x00p\x10\t\x04\x00\x00\x00\x00\x00\x00\x00\x002\x01\x00\x00,\x00\x00\x00\xff\xff\x07\x00F\x00O\x00N\x00T\x00D\x00I\x00R\x00\x00\x00\x00\x00\x00\x00P\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00";

    try testCompileWithOutput(
        \\1 FONT FIXED empty.fnt
        \\2 FONT MOVEABLE DISCARDABLE PRELOAD empty.fnt
    ,
        expected,
        tmp_dir.dir,
    );

    // For duplicate IDs, all but the first are ignored
    try testCompileWithOutput(
        \\1 FONT FIXED empty.fnt
        \\2 FONT MOVEABLE DISCARDABLE PRELOAD empty.fnt
        \\2 FONT FIXED empty.fnt
        \\0x1 FONT FIXED empty.fnt
    ,
        expected,
        tmp_dir.dir,
    );
}

test "accelerators resource" {
    try testCompileWithOutput(
        \\1 ACCELERATORS { 1, 1, ASCII }
    ,
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00 \x00\x00\x00\xff\xff\t\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x01\x00\x01\x00\x00\x00",
        std.fs.cwd(),
    );
    try testCompileWithOutput(
        \\1 ACCELERATORS { "c", 65537, VIRTKEY, CONTROL, ALT, SHIFT }
    ,
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00 \x00\x00\x00\xff\xff\t\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x9d\x00C\x00\x01\x00\x00\x00",
        std.fs.cwd(),
    );
}
