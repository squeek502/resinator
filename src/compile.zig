const std = @import("std");
const Allocator = std.mem.Allocator;
const Node = @import("ast.zig").Node;
const Lexer = @import("lex.zig").Lexer;
const Parser = @import("parse.zig").Parser;
const Resource = @import("rc.zig").Resource;
const Token = @import("lex.zig").Token;
const literals = @import("literals.zig");
const Number = literals.Number;
const SourceBytes = literals.SourceBytes;
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
const CodePage = @import("code_pages.zig").CodePage;
const CodePageLookup = @import("ast.zig").CodePageLookup;

pub fn compile(allocator: Allocator, source: []const u8, writer: anytype, cwd: std.fs.Dir, diagnostics: *Diagnostics) !void {
    // TODO: Take this as a parameter
    const default_code_page: CodePage = .windows1252;
    var lexer = Lexer.init(source, default_code_page);
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
        .code_pages = &tree.code_pages,
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
    code_pages: *const CodePageLookup,

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
            .dialog => try self.writeDialog(@fieldParentPtr(Node.Dialog, "base", node), writer),
            .control_statement => unreachable,
            .menu => try self.writeMenu(@fieldParentPtr(Node.Menu, "base", node), writer),
            .menu_item => unreachable,
            .menu_item_separator => unreachable,
            .menu_item_ex => unreachable,
            .popup => unreachable,
            .popup_ex => unreachable,
            .version_info => try self.writeVersionInfo(@fieldParentPtr(Node.VersionInfo, "base", node), writer),
            .version_statement => unreachable,
            .block => unreachable,
            .block_value => unreachable,
            .string_table => try self.writeStringTable(@fieldParentPtr(Node.StringTable, "base", node)),
            .string_table_string => unreachable, // handled by writeStringTable
            .language_statement => self.writeLanguageStatement(@fieldParentPtr(Node.LanguageStatement, "base", node)),
            .font_statement => unreachable,
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
                        const bytes = SourceBytes{ .slice = slice, .code_page = self.code_pages.getForToken(literal_node.token) };
                        const parsed = try literals.parseQuotedAsciiString(self.allocator, bytes, .{
                            .start_column = column,
                            .diagnostics = .{ .diagnostics = self.diagnostics, .token = literal_node.token },
                        });
                        return .{ .utf8 = parsed, .needs_free = true };
                    },
                    .quoted_wide_string => {
                        // TODO: No need to parse this to UTF-16 and then back to UTF-8
                        // if it's already UTF-8. Should have a function that parses wide
                        // strings directly to UTF-8.
                        const slice = literal_node.token.slice(self.source);
                        const column = literal_node.token.calculateColumn(self.source, 8, null);
                        const bytes = SourceBytes{ .slice = slice, .code_page = self.code_pages.getForToken(literal_node.token) };
                        const parsed_string = try literals.parseQuotedWideString(self.allocator, bytes, .{
                            .start_column = column,
                            .diagnostics = .{ .diagnostics = self.diagnostics, .token = literal_node.token },
                        });
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

        const id_bytes = SourceBytes{
            .slice = node.id.slice(self.source),
            .code_page = self.code_pages.getForToken(node.id),
        };
        const type_bytes = SourceBytes{
            .slice = node.type.slice(self.source),
            .code_page = self.code_pages.getForToken(node.type),
        };
        // Init header with data size zero for now, will need to fill it in later
        var header = try ResourceHeader.init(self.allocator, id_bytes, type_bytes, 0, self.state.language);
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
        wide_string: [:0]const u16,

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
    fn evaluateNumberExpression(expression_node: *Node, source: []const u8, code_page_lookup: *const CodePageLookup) Number {
        switch (expression_node.id) {
            .literal => {
                const literal_node = expression_node.cast(.literal).?;
                std.debug.assert(literal_node.token.id == .number);
                const bytes = SourceBytes{
                    .slice = literal_node.token.slice(source),
                    .code_page = code_page_lookup.getForToken(literal_node.token),
                };
                return literals.parseNumberLiteral(bytes);
            },
            .binary_expression => {
                const binary_expression_node = expression_node.cast(.binary_expression).?;
                const lhs = evaluateNumberExpression(binary_expression_node.left, source, code_page_lookup);
                const rhs = evaluateNumberExpression(binary_expression_node.right, source, code_page_lookup);
                const operator_char = binary_expression_node.operator.slice(source)[0];
                return lhs.evaluateOperator(operator_char, rhs);
            },
            .grouped_expression => {
                const grouped_expression_node = expression_node.cast(.grouped_expression).?;
                return evaluateNumberExpression(grouped_expression_node.expression, source, code_page_lookup);
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
                        const number = evaluateNumberExpression(expression_node, self.source, self.code_pages);
                        return .{ .number = number };
                    },
                    .quoted_ascii_string => {
                        const column = literal_node.token.calculateColumn(self.source, 8, null);
                        const bytes = SourceBytes{
                            .slice = literal_node.token.slice(self.source),
                            .code_page = self.code_pages.getForToken(literal_node.token),
                        };
                        const parsed = try literals.parseQuotedAsciiString(self.allocator, bytes, .{
                            .start_column = column,
                            .diagnostics = .{ .diagnostics = self.diagnostics, .token = literal_node.token },
                        });
                        errdefer self.allocator.free(parsed);
                        return .{ .ascii_string = parsed };
                    },
                    .quoted_wide_string => {
                        const column = literal_node.token.calculateColumn(self.source, 8, null);
                        const bytes = SourceBytes{
                            .slice = literal_node.token.slice(self.source),
                            .code_page = self.code_pages.getForToken(literal_node.token),
                        };
                        const parsed_string = try literals.parseQuotedWideString(self.allocator, bytes, .{
                            .start_column = column,
                            .diagnostics = .{ .diagnostics = self.diagnostics, .token = literal_node.token },
                        });
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
                const result = evaluateNumberExpression(expression_node, self.source, self.code_pages);
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
        const id_bytes = SourceBytes{
            .slice = id_token.slice(self.source),
            .code_page = self.code_pages.getForToken(id_token),
        };
        const type_bytes = SourceBytes{
            .slice = type_token.slice(self.source),
            .code_page = self.code_pages.getForToken(type_token),
        };
        var header = try ResourceHeader.init(self.allocator, id_bytes, type_bytes, data_size, language);
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
        try writer.writeByteNTimes(0, numPaddingBytesNeeded(data_size));
    }

    pub fn numPaddingBytesNeeded(data_size: u32) u3 {
        return @intCast(u3, (4 -% data_size) % 4);
    }

    pub fn evaluateAcceleratorKeyExpression(self: *Compiler, node: *Node, is_virt: bool) !u16 {
        if (node.isNumberExpression()) {
            return evaluateNumberExpression(node, self.source, self.code_pages).asWord();
        } else {
            std.debug.assert(node.isStringLiteral());
            const literal = @fieldParentPtr(Node.Literal, "base", node);
            const bytes = SourceBytes{
                .slice = literal.token.slice(self.source),
                .code_page = self.code_pages.getForToken(literal.token),
            };
            const column = literal.token.calculateColumn(self.source, 8, null);
            return res.parseAcceleratorKeyString(bytes, is_virt, .{
                .start_column = column,
                .diagnostics = .{ .diagnostics = self.diagnostics, .token = literal.token },
            });
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
            if (accelerator.event.isNumberExpression() and !modifiers.explicit_ascii_or_virtkey) {
                return self.addErrorDetailsAndFail(.{
                    .err = .accelerator_type_required,
                    .token = accelerator.event.getFirstToken(),
                });
            }
            const key = self.evaluateAcceleratorKeyExpression(accelerator.event, modifiers.isSet(.virtkey)) catch |err| switch (err) {
                error.OutOfMemory => |e| return e,
                else => {
                    // TODO: better error with more context from the caught error
                    return self.addErrorDetailsAndFail(.{
                        .err = .invalid_accelerator_key,
                        .token = accelerator.event.getFirstToken(),
                    });
                },
            };
            const cmd_id = evaluateNumberExpression(accelerator.idvalue, self.source, self.code_pages);

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
        const id_bytes = SourceBytes{
            .slice = node.id.slice(self.source),
            .code_page = self.code_pages.getForToken(node.id),
        };
        const type_bytes = SourceBytes{
            .slice = node.type.slice(self.source),
            .code_page = self.code_pages.getForToken(node.type),
        };
        var header = try ResourceHeader.init(self.allocator, id_bytes, type_bytes, data_size, self.state.language);
        defer header.deinit(self.allocator);

        header.applyMemoryFlags(node.common_resource_attributes, self.source);
        header.applyOptionalStatements(node.optional_statements, self.source, self.code_pages);

        try header.write(writer);

        var data_fbs = std.io.fixedBufferStream(data_buffer.items);
        try writeResourceData(writer, data_fbs.reader(), data_size);
    }

    pub fn writeDialog(self: *Compiler, node: *Node.Dialog, writer: anytype) !void {
        var data_buffer = std.ArrayList(u8).init(self.allocator);
        defer data_buffer.deinit();
        const data_writer = data_buffer.writer();

        const resource = Resource.fromString(.{
            .slice = node.type.slice(self.source),
            .code_page = self.code_pages.getForToken(node.type),
        });
        std.debug.assert(resource == .dialog or resource == .dialogex);

        const OptionalStatementValues = struct {
            style: u32 = res.WS.SYSMENU | res.WS.BORDER | res.WS.POPUP,
            exstyle: u32 = 0,
            class: ?NameOrOrdinal = null,
            menu: ?NameOrOrdinal = null,
            font: ?FontStatementValues = null,
            caption: ?Token = null,
        };
        var optional_statement_values: OptionalStatementValues = .{};
        defer {
            if (optional_statement_values.class) |class| {
                class.deinit(self.allocator);
            }
            if (optional_statement_values.menu) |menu| {
                menu.deinit(self.allocator);
            }
        }
        for (node.optional_statements) |optional_statement| {
            switch (optional_statement.id) {
                .simple_statement => {
                    const simple_statement = @fieldParentPtr(Node.SimpleStatement, "base", optional_statement);
                    const statement_identifier = simple_statement.identifier;
                    const statement_type = rc.OptionalStatements.dialog_map.get(statement_identifier.slice(self.source)) orelse continue;
                    switch (statement_type) {
                        .style, .exstyle => {
                            const style = evaluateNumberExpression(simple_statement.value, self.source, self.code_pages);
                            // STYLE and EXSTYLE are both implicitly long
                            if (statement_type == .style) {
                                optional_statement_values.style = style.value;
                            } else {
                                optional_statement_values.exstyle = style.value;
                            }
                        },
                        .caption => {
                            std.debug.assert(simple_statement.value.id == .literal);
                            const literal_node = @fieldParentPtr(Node.Literal, "base", simple_statement.value);
                            optional_statement_values.caption = literal_node.token;
                        },
                        .class => {
                            const forced_ordinal = optional_statement_values.class != null and optional_statement_values.class.? == .ordinal;
                            // clear out the old one if it exists
                            if (optional_statement_values.class) |prev| {
                                prev.deinit(self.allocator);
                                optional_statement_values.class = null;
                            }

                            if (simple_statement.value.isNumberExpression()) {
                                const class_ordinal = evaluateNumberExpression(simple_statement.value, self.source, self.code_pages);
                                optional_statement_values.class = NameOrOrdinal{ .ordinal = class_ordinal.asWord() };
                            } else {
                                std.debug.assert(simple_statement.value.isStringLiteral());
                                const literal_node = @fieldParentPtr(Node.Literal, "base", simple_statement.value);
                                const parsed = try self.parseQuotedStringAsWideString(literal_node.token);
                                if (forced_ordinal) {
                                    defer self.allocator.free(parsed);
                                    optional_statement_values.class = NameOrOrdinal{ .ordinal = res.ForcedOrdinal.fromUtf16Le(parsed) };
                                } else {
                                    optional_statement_values.class = NameOrOrdinal{ .name = parsed };
                                }
                            }
                        },
                        .menu => {
                            const forced_ordinal = optional_statement_values.menu != null and optional_statement_values.menu.? == .ordinal;
                            // clear out the old one if it exists
                            if (optional_statement_values.menu) |prev| {
                                prev.deinit(self.allocator);
                                optional_statement_values.menu = null;
                            }

                            std.debug.assert(simple_statement.value.id == .literal);
                            const literal_node = @fieldParentPtr(Node.Literal, "base", simple_statement.value);

                            const token_slice = literal_node.token.slice(self.source);
                            const bytes = SourceBytes{
                                .slice = token_slice,
                                .code_page = self.code_pages.getForToken(literal_node.token),
                            };
                            if (forced_ordinal or std.ascii.isDigit(token_slice[0])) {
                                optional_statement_values.menu = .{ .ordinal = res.ForcedOrdinal.fromBytes(bytes) };
                            } else {
                                optional_statement_values.menu = try NameOrOrdinal.nameFromString(self.allocator, bytes);
                            }
                        },
                        else => {},
                    }
                },
                .font_statement => {
                    const font = @fieldParentPtr(Node.FontStatement, "base", optional_statement);
                    if (optional_statement_values.font != null) {
                        optional_statement_values.font.?.node = font;
                    } else {
                        optional_statement_values.font = FontStatementValues{ .node = font };
                    }
                    if (font.weight) |weight| {
                        const value = evaluateNumberExpression(weight, self.source, self.code_pages);
                        optional_statement_values.font.?.weight = value.asWord();
                    }
                    if (font.italic) |italic| {
                        const value = evaluateNumberExpression(italic, self.source, self.code_pages);
                        optional_statement_values.font.?.italic = value.asWord() != 0;
                    }
                },
                else => {},
            }
        }
        const x = evaluateNumberExpression(node.x, self.source, self.code_pages);
        const y = evaluateNumberExpression(node.y, self.source, self.code_pages);
        const width = evaluateNumberExpression(node.width, self.source, self.code_pages);
        const height = evaluateNumberExpression(node.height, self.source, self.code_pages);

        // FONT statement implies DS_SETFONT which is 0x40
        if (optional_statement_values.font) |_| {
            optional_statement_values.style |= 0x40;
        }
        // CAPTION statement implies DS_CAPTION which is 0x00C00000
        if (optional_statement_values.caption) |_| {
            optional_statement_values.style |= 0x00C00000;
        }

        // Header
        if (resource == .dialogex) {
            const help_id: u32 = help_id: {
                if (node.help_id == null) break :help_id 0;
                break :help_id evaluateNumberExpression(node.help_id.?, self.source, self.code_pages).value;
            };
            try data_writer.writeIntLittle(u16, 1); // version number, always 1
            try data_writer.writeIntLittle(u16, 0xFFFF); // signature, always 0xFFFF
            try data_writer.writeIntLittle(u32, help_id);
            try data_writer.writeIntLittle(u32, optional_statement_values.exstyle);
            try data_writer.writeIntLittle(u32, optional_statement_values.style);
        } else {
            try data_writer.writeIntLittle(u32, optional_statement_values.style);
            try data_writer.writeIntLittle(u32, optional_statement_values.exstyle);
        }
        // TODO: Enforce this limit in the parser?
        try data_writer.writeIntLittle(u16, @intCast(u16, node.controls.len));
        try data_writer.writeIntLittle(u16, x.asWord());
        try data_writer.writeIntLittle(u16, y.asWord());
        try data_writer.writeIntLittle(u16, width.asWord());
        try data_writer.writeIntLittle(u16, height.asWord());

        // Menu
        if (optional_statement_values.menu) |menu| {
            try menu.write(data_writer);
        } else {
            try data_writer.writeIntLittle(u16, 0);
        }
        // Class
        if (optional_statement_values.class) |class| {
            try class.write(data_writer);
        } else {
            try data_writer.writeIntLittle(u16, 0);
        }
        // Caption
        if (optional_statement_values.caption) |caption| {
            const parsed = try self.parseQuotedStringAsWideString(caption);
            defer self.allocator.free(parsed);
            try data_writer.writeAll(std.mem.sliceAsBytes(parsed[0 .. parsed.len + 1]));
        } else {
            try data_writer.writeIntLittle(u16, 0);
        }
        // Font
        if (optional_statement_values.font) |font| {
            try self.writeDialogFont(resource, font, data_writer);
        }

        for (node.controls) |control_node| {
            const control = @fieldParentPtr(Node.ControlStatement, "base", control_node);
            const control_type = rc.Control.map.get(control.type.slice(self.source)).?;

            // Each control must be at a 4-byte boundary. However, the Windows RC
            // compiler will miscompile controls if their extra data ends on an odd offset.
            // We will avoid the miscompilation and emit a warning.
            const num_padding = numPaddingBytesNeeded(@intCast(u32, data_buffer.items.len));
            if (num_padding == 1 or num_padding == 3) {
                try self.addErrorDetails(.{
                    .err = .rc_would_miscompile_control_padding,
                    .type = .warning,
                    .token = control.type,
                });
                try self.addErrorDetails(.{
                    .err = .rc_would_miscompile_control_padding,
                    .type = .note,
                    .print_source_line = false,
                    .token = control.type,
                });
            }
            try data_writer.writeByteNTimes(0, num_padding);

            var style: u32 = if (control.style) |style_expression|
                evaluateNumberExpression(style_expression, self.source, self.code_pages).value
            else
                0;

            // Certain styles are implied by the control type
            style |= res.ControlClass.getImpliedStyle(control_type);

            const exstyle: u32 = if (control.exstyle) |exstyle_expression|
                evaluateNumberExpression(exstyle_expression, self.source, self.code_pages).value
            else
                0;

            switch (resource) {
                .dialog => {
                    // Note: Reverse order from DIALOGEX
                    try data_writer.writeIntLittle(u32, style);
                    try data_writer.writeIntLittle(u32, exstyle);
                },
                .dialogex => {
                    const help_id: u32 = if (control.help_id) |help_id_expression|
                        evaluateNumberExpression(help_id_expression, self.source, self.code_pages).value
                    else
                        0;
                    try data_writer.writeIntLittle(u32, help_id);
                    // Note: Reverse order from DIALOG
                    try data_writer.writeIntLittle(u32, exstyle);
                    try data_writer.writeIntLittle(u32, style);
                },
                else => unreachable,
            }

            const control_x = evaluateNumberExpression(control.x, self.source, self.code_pages);
            const control_y = evaluateNumberExpression(control.y, self.source, self.code_pages);
            const control_width = evaluateNumberExpression(control.width, self.source, self.code_pages);
            const control_height = evaluateNumberExpression(control.height, self.source, self.code_pages);

            try data_writer.writeIntLittle(u16, control_x.asWord());
            try data_writer.writeIntLittle(u16, control_y.asWord());
            try data_writer.writeIntLittle(u16, control_width.asWord());
            try data_writer.writeIntLittle(u16, control_height.asWord());

            const control_id = evaluateNumberExpression(control.id, self.source, self.code_pages);
            switch (resource) {
                .dialog => try data_writer.writeIntLittle(u16, control_id.asWord()),
                .dialogex => try data_writer.writeIntLittle(u32, control_id.value),
                else => unreachable,
            }

            if (res.ControlClass.fromControl(control_type)) |control_class| {
                const ordinal = NameOrOrdinal{ .ordinal = @enumToInt(control_class) };
                try ordinal.write(data_writer);
            } else {
                const class_node = control.class.?;
                if (class_node.isNumberExpression()) {
                    const number = evaluateNumberExpression(class_node, self.source, self.code_pages);
                    const ordinal = NameOrOrdinal{ .ordinal = number.asWord() };
                    // This is different from how the Windows RC compiles ordinals here,
                    // but I think that's a miscompilation/bug of the Windows implementation.
                    // The Windows behavior is (where LSB = least significant byte):
                    // - If the LSB is 0x00 => 0xFFFF0000
                    // - If the LSB is < 0x80 => 0x000000<LSB>
                    // - If the LSB is >= 0x80 => 0x0000FF<LSB>
                    //
                    // Because of this, we emit a warning about the potential miscompilation
                    try self.addErrorDetails(.{
                        .err = .rc_would_miscompile_control_class_ordinal,
                        .type = .warning,
                        .token = class_node.getFirstToken(),
                    });
                    try self.addErrorDetails(.{
                        .err = .rc_would_miscompile_control_class_ordinal,
                        .type = .note,
                        .print_source_line = false,
                        .token = class_node.getFirstToken(),
                    });
                    // And then write out the ordinal using a proper a NameOrOrdinal encoding.
                    try ordinal.write(data_writer);
                } else if (class_node.isStringLiteral()) {
                    const literal_node = @fieldParentPtr(Node.Literal, "base", class_node);
                    const parsed = try self.parseQuotedStringAsWideString(literal_node.token);
                    defer self.allocator.free(parsed);
                    if (rc.ControlClass.fromWideString(parsed)) |control_class| {
                        const ordinal = NameOrOrdinal{ .ordinal = @enumToInt(control_class) };
                        try ordinal.write(data_writer);
                    } else {
                        const name = NameOrOrdinal{ .name = parsed };
                        try name.write(data_writer);
                    }
                } else {
                    const literal_node = @fieldParentPtr(Node.Literal, "base", class_node);
                    const literal_slice = literal_node.token.slice(self.source);
                    // This succeeding is guaranteed by the parser
                    const control_class = rc.ControlClass.map.get(literal_slice) orelse unreachable;
                    const ordinal = NameOrOrdinal{ .ordinal = @enumToInt(control_class) };
                    try ordinal.write(data_writer);
                }
            }

            if (control.text) |text_token| {
                const bytes = SourceBytes{
                    .slice = text_token.slice(self.source),
                    .code_page = self.code_pages.getForToken(text_token),
                };
                if (NameOrOrdinal.maybeOrdinalFromString(bytes)) |ordinal| {
                    try ordinal.write(data_writer);
                } else {
                    const text = try self.parseQuotedStringAsWideString(text_token);
                    defer self.allocator.free(text);
                    const name_or_ordinal = NameOrOrdinal{ .name = text };
                    try name_or_ordinal.write(data_writer);
                }
            } else {
                try NameOrOrdinal.writeEmpty(data_writer);
            }

            var extra_data_buf = std.ArrayListUnmanaged(u8){};
            defer extra_data_buf.deinit(self.allocator);
            var extra_data_writer = extra_data_buf.writer(self.allocator);
            for (control.extra_data) |data_expression| {
                const data = try self.evaluateDataExpression(data_expression);
                defer data.deinit(self.allocator);
                try data.write(extra_data_writer);
            }
            try data_writer.writeIntLittle(u16, @intCast(u16, extra_data_buf.items.len));
            try data_writer.writeAll(extra_data_buf.items);
        }

        const data_size = @intCast(u32, data_buffer.items.len);
        const id_bytes = SourceBytes{
            .slice = node.id.slice(self.source),
            .code_page = self.code_pages.getForToken(node.id),
        };
        const type_bytes = SourceBytes{
            .slice = node.type.slice(self.source),
            .code_page = self.code_pages.getForToken(node.type),
        };
        var header = try ResourceHeader.init(self.allocator, id_bytes, type_bytes, data_size, self.state.language);
        defer header.deinit(self.allocator);

        header.applyMemoryFlags(node.common_resource_attributes, self.source);
        header.applyOptionalStatements(node.optional_statements, self.source, self.code_pages);

        try header.write(writer);

        var data_fbs = std.io.fixedBufferStream(data_buffer.items);
        try writeResourceData(writer, data_fbs.reader(), data_size);
    }

    /// Weight and italic carry over from previous FONT statements within a single resource,
    /// so they need to be parsed ahead-of-time and stored
    const FontStatementValues = struct {
        weight: u16 = 0,
        italic: bool = false,
        node: *Node.FontStatement,
    };

    pub fn writeDialogFont(self: *Compiler, resource: Resource, values: FontStatementValues, writer: anytype) !void {
        const node = values.node;
        const point_size = evaluateNumberExpression(node.point_size, self.source, self.code_pages);
        try writer.writeIntLittle(u16, point_size.asWord());

        if (resource == .dialogex) {
            try writer.writeIntLittle(u16, values.weight);
        }

        if (resource == .dialogex) {
            try writer.writeIntLittle(u8, @boolToInt(values.italic));
        }

        if (node.char_set) |char_set| {
            const value = evaluateNumberExpression(char_set, self.source, self.code_pages);
            try writer.writeIntLittle(u8, @truncate(u8, value.value));
        } else if (resource == .dialogex) {
            try writer.writeIntLittle(u8, 1); // DEFAULT_CHARSET
        }

        const typeface = try self.parseQuotedStringAsWideString(node.typeface);
        defer self.allocator.free(typeface);
        try writer.writeAll(std.mem.sliceAsBytes(typeface[0 .. typeface.len + 1]));
    }

    pub fn writeMenu(self: *Compiler, node: *Node.Menu, writer: anytype) !void {
        var data_buffer = std.ArrayList(u8).init(self.allocator);
        defer data_buffer.deinit();
        const data_writer = data_buffer.writer();

        const type_bytes = SourceBytes{
            .slice = node.type.slice(self.source),
            .code_page = self.code_pages.getForToken(node.type),
        };
        const resource = Resource.fromString(type_bytes);
        std.debug.assert(resource == .menu or resource == .menuex);

        // menu header
        const version: u16 = if (resource == .menu) 0 else 1;
        try data_writer.writeIntLittle(u16, version);
        const header_size: u16 = if (resource == .menu) 0 else 4;
        try data_writer.writeIntLittle(u16, header_size); // cbHeaderSize
        // Note: There can be extra bytes at the end, but they are always zero-length for us.
        // MENU   => rgbExtra: [cbHeaderSize]u8
        // MENUEX => rgbExtra: [cbHeaderSize-4]u8

        if (resource == .menuex) {
            if (node.help_id) |help_id_node| {
                const help_id = evaluateNumberExpression(help_id_node, self.source, self.code_pages);
                try data_writer.writeIntLittle(u32, help_id.value);
            } else {
                try data_writer.writeIntLittle(u32, 0);
            }
        }

        for (node.items) |item, i| {
            const is_last = i == node.items.len - 1;
            try self.writeMenuItem(item, data_writer, is_last);
        }

        const data_size = @intCast(u32, data_buffer.items.len);
        const id_bytes = SourceBytes{
            .slice = node.id.slice(self.source),
            .code_page = self.code_pages.getForToken(node.id),
        };
        var header = try ResourceHeader.init(self.allocator, id_bytes, type_bytes, data_size, self.state.language);
        defer header.deinit(self.allocator);

        header.applyMemoryFlags(node.common_resource_attributes, self.source);
        header.applyOptionalStatements(node.optional_statements, self.source, self.code_pages);

        try header.write(writer);

        var data_fbs = std.io.fixedBufferStream(data_buffer.items);
        try writeResourceData(writer, data_fbs.reader(), data_size);
    }

    pub fn writeMenuItem(self: *Compiler, node: *Node, writer: anytype, is_last_of_parent: bool) !void {
        switch (node.id) {
            .menu_item_separator => {
                // This is the 'alternate compability form' of the separator, see
                // https://devblogs.microsoft.com/oldnewthing/20080710-00/?p=21673
                //
                // The 'correct' way is to set the MF_SEPARATOR flag, but the Win32 RC
                // compiler still uses this alternate form, so that's what we use too.
                var flags = res.MenuItemFlags{};
                if (is_last_of_parent) flags.markLast();
                try writer.writeIntLittle(u16, flags.value);
                try writer.writeIntLittle(u16, 0); // id
                try writer.writeIntLittle(u16, 0); // null-terminated UTF-16 text
            },
            .menu_item => {
                const menu_item = @fieldParentPtr(Node.MenuItem, "base", node);
                var flags = res.MenuItemFlags{};
                for (menu_item.option_list) |option_token| {
                    // This failing would be a bug in the parser
                    const option = rc.MenuItem.Option.map.get(option_token.slice(self.source)) orelse unreachable;
                    flags.apply(option);
                }
                if (is_last_of_parent) flags.markLast();
                try writer.writeIntLittle(u16, flags.value);

                var result = evaluateNumberExpression(menu_item.result, self.source, self.code_pages);
                try writer.writeIntLittle(u16, result.asWord());

                var text = try self.parseQuotedStringAsWideString(menu_item.text);
                defer self.allocator.free(text);
                try writer.writeAll(std.mem.sliceAsBytes(text[0 .. text.len + 1]));
            },
            .popup => {
                const popup = @fieldParentPtr(Node.Popup, "base", node);
                var flags = res.MenuItemFlags{ .value = res.MF.POPUP };
                for (popup.option_list) |option_token| {
                    // This failing would be a bug in the parser
                    const option = rc.MenuItem.Option.map.get(option_token.slice(self.source)) orelse unreachable;
                    flags.apply(option);
                }
                if (is_last_of_parent) flags.markLast();
                try writer.writeIntLittle(u16, flags.value);

                var text = try self.parseQuotedStringAsWideString(popup.text);
                defer self.allocator.free(text);
                try writer.writeAll(std.mem.sliceAsBytes(text[0 .. text.len + 1]));

                for (popup.items) |item, i| {
                    const is_last = i == popup.items.len - 1;
                    try self.writeMenuItem(item, writer, is_last);
                }
            },
            inline .menu_item_ex, .popup_ex => |node_type| {
                const menu_item = @fieldParentPtr(node_type.Type(), "base", node);

                if (menu_item.type) |flags| {
                    const value = evaluateNumberExpression(flags, self.source, self.code_pages);
                    try writer.writeIntLittle(u32, value.value);
                } else {
                    try writer.writeIntLittle(u32, 0);
                }

                if (menu_item.state) |state| {
                    const value = evaluateNumberExpression(state, self.source, self.code_pages);
                    try writer.writeIntLittle(u32, value.value);
                } else {
                    try writer.writeIntLittle(u32, 0);
                }

                if (menu_item.id) |id| {
                    const value = evaluateNumberExpression(id, self.source, self.code_pages);
                    try writer.writeIntLittle(u32, value.value);
                } else {
                    try writer.writeIntLittle(u32, 0);
                }

                var flags: u16 = 0;
                if (is_last_of_parent) flags |= @intCast(u16, res.MF.END);
                // This constant doesn't seem to have a named #define, it's different than MF_POPUP
                if (node_type == .popup_ex) flags |= 0x01;
                try writer.writeIntLittle(u16, flags);

                var text = try self.parseQuotedStringAsWideString(menu_item.text);
                defer self.allocator.free(text);
                try writer.writeAll(std.mem.sliceAsBytes(text[0 .. text.len + 1]));

                // Only the combination of the flags u16 and the text bytes can cause
                // non-DWORD alignment, so we can just use the byte length of those
                // two values to realign to DWORD alignment.
                const relevant_bytes = 2 + (text.len + 1) * 2;
                try writeDataPadding(writer, @intCast(u32, relevant_bytes));

                if (node_type == .popup_ex) {
                    if (menu_item.help_id) |help_id_node| {
                        const help_id = evaluateNumberExpression(help_id_node, self.source, self.code_pages);
                        try writer.writeIntLittle(u32, help_id.value);
                    } else {
                        try writer.writeIntLittle(u32, 0);
                    }

                    for (menu_item.items) |item, i| {
                        const is_last = i == menu_item.items.len - 1;
                        try self.writeMenuItem(item, writer, is_last);
                    }
                }
            },
            else => unreachable,
        }
    }

    pub fn writeVersionInfo(self: *Compiler, node: *Node.VersionInfo, writer: anytype) !void {
        var data_buffer = std.ArrayList(u8).init(self.allocator);
        defer data_buffer.deinit();
        const data_writer = data_buffer.writer();

        try data_writer.writeIntLittle(u16, 0); // placeholder size
        try data_writer.writeIntLittle(u16, res.FixedFileInfo.byte_len);
        try data_writer.writeIntLittle(u16, res.VersionNode.type_binary);
        const key_bytes = std.mem.sliceAsBytes(res.FixedFileInfo.key[0 .. res.FixedFileInfo.key.len + 1]);
        try data_writer.writeAll(key_bytes);
        try writeDataPadding(data_writer, @intCast(u32, data_buffer.items.len));

        var fixed_file_info = res.FixedFileInfo{};
        for (node.fixed_info) |fixed_info| {
            switch (fixed_info.id) {
                .version_statement => {
                    const version_statement = @fieldParentPtr(Node.VersionStatement, "base", fixed_info);
                    const version_type = rc.VersionInfo.map.get(version_statement.type.slice(self.source)).?;
                    for (version_statement.parts) |part, i| {
                        const part_value = evaluateNumberExpression(part, self.source, self.code_pages);
                        switch (version_type) {
                            .file_version => {
                                fixed_file_info.file_version.parts[i] = part_value.asWord();
                            },
                            .product_version => {
                                fixed_file_info.product_version.parts[i] = part_value.asWord();
                            },
                            else => unreachable,
                        }
                    }
                },
                .simple_statement => {
                    const statement = @fieldParentPtr(Node.SimpleStatement, "base", fixed_info);
                    const statement_type = rc.VersionInfo.map.get(statement.identifier.slice(self.source)).?;
                    const value = evaluateNumberExpression(statement.value, self.source, self.code_pages);
                    switch (statement_type) {
                        .file_flags_mask => fixed_file_info.file_flags_mask = value.value,
                        .file_flags => fixed_file_info.file_flags = value.value,
                        .file_os => fixed_file_info.file_os = value.value,
                        .file_type => fixed_file_info.file_type = value.value,
                        .file_subtype => fixed_file_info.file_subtype = value.value,
                        else => unreachable,
                    }
                },
                else => unreachable,
            }
        }
        try fixed_file_info.write(data_writer);

        for (node.block_statements) |statement| {
            try self.writeVersionNode(statement, data_writer, &data_buffer);
        }

        // we now know the full size of this node (including its children), so set its size
        std.mem.writeIntLittle(u16, data_buffer.items[0..2], @intCast(u16, data_buffer.items.len));

        const type_bytes = self.sourceBytesForToken(node.versioninfo);
        const data_size = @intCast(u32, data_buffer.items.len);
        const id_bytes = self.sourceBytesForToken(node.id);
        var header = try ResourceHeader.init(self.allocator, id_bytes, type_bytes, data_size, self.state.language);
        defer header.deinit(self.allocator);

        header.applyMemoryFlags(node.common_resource_attributes, self.source);

        try header.write(writer);

        var data_fbs = std.io.fixedBufferStream(data_buffer.items);
        try writeResourceData(writer, data_fbs.reader(), data_size);
    }

    pub fn writeVersionNode(self: *Compiler, node: *Node, writer: anytype, buf: *std.ArrayList(u8)) !void {
        try writeDataPadding(writer, @intCast(u32, buf.items.len));

        const node_and_children_size_offset = buf.items.len;
        try writer.writeIntLittle(u16, 0); // placeholder for size
        const data_size_offset = buf.items.len;
        try writer.writeIntLittle(u16, 0); // placeholder for data size
        const data_type_offset = buf.items.len;
        // Data type is string unless the node contains values that are numbers.
        try writer.writeIntLittle(u16, res.VersionNode.type_string);

        switch (node.id) {
            inline .block, .block_value => |node_type| {
                const block_or_value = @fieldParentPtr(node_type.Type(), "base", node);
                const parsed_key = try self.parseQuotedStringAsWideString(block_or_value.key);
                defer self.allocator.free(parsed_key);

                try writer.writeAll(std.mem.sliceAsBytes(parsed_key[0 .. parsed_key.len + 1]));

                var has_number_value: bool = false;
                for (block_or_value.values) |value_node| {
                    if (value_node.isNumberExpression()) {
                        has_number_value = true;
                        break;
                    }
                }
                // The Win32 RC compiler does some strange stuff with the data size:
                // Strings are counted as UTF-16 code units including the null-terminator
                // Numbers are counted as their byte lengths
                // TODO: Make using the real byte size a configurable option (as long as
                //       the real byte size is still parsable by consumers of the
                //       version info data).
                var values_size_win32_rc: usize = 0;

                // TODO: This can sometimes not be added, need to figure out when exactly
                //       that happens
                try writeDataPadding(writer, @intCast(u32, buf.items.len));

                for (block_or_value.values) |value_node| {
                    if (value_node.isNumberExpression()) {
                        const number = evaluateNumberExpression(value_node, self.source, self.code_pages);
                        // This is used to write u16 or u32 depending on the number's suffix
                        const data_wrapper = Data{ .number = number };
                        try data_wrapper.write(writer);
                        // Numbers use byte count
                        values_size_win32_rc += if (number.is_long) 4 else 2;
                    } else {
                        std.debug.assert(value_node.isStringLiteral());
                        const literal_node = value_node.cast(.literal).?;
                        const parsed_value = try self.parseQuotedStringAsWideString(literal_node.token);
                        defer self.allocator.free(parsed_value);
                        try writer.writeAll(std.mem.sliceAsBytes(parsed_value[0 .. parsed_value.len + 1]));
                        // Strings use UTF-16 code-unit count including the null-terminator
                        // TODO: The null-terminator can sometimes not be included,
                        //       need to figure out when exactly that happens
                        values_size_win32_rc += parsed_value.len + 1;
                    }
                }
                var data_size_slice = buf.items[data_size_offset..];
                std.mem.writeIntLittle(u16, data_size_slice[0..@sizeOf(u16)], @intCast(u16, values_size_win32_rc));

                if (has_number_value) {
                    const data_type_slice = buf.items[data_type_offset..];
                    std.mem.writeIntLittle(u16, data_type_slice[0..@sizeOf(u16)], res.VersionNode.type_binary);
                }

                if (node_type == .block) {
                    const block = block_or_value;
                    for (block.children) |child| {
                        try self.writeVersionNode(child, writer, buf);
                    }
                }
            },
            else => unreachable,
        }

        const node_and_children_size = buf.items.len - node_and_children_size_offset;
        const node_and_children_size_slice = buf.items[node_and_children_size_offset..];
        std.mem.writeIntLittle(u16, node_and_children_size_slice[0..@sizeOf(u16)], @intCast(u16, node_and_children_size));
    }

    pub fn writeStringTable(self: *Compiler, node: *Node.StringTable) !void {
        const language = getLanguageFromOptionalStatements(node.optional_statements, self.source, self.code_pages) orelse self.state.language;

        for (node.strings) |string_node| {
            const string = @fieldParentPtr(Node.StringTableString, "base", string_node);
            const string_id_data = try self.evaluateDataExpression(string.id);
            const string_id = string_id_data.number.asWord();

            self.state.string_tables.set(self.arena, language, string_id, string.string, &node.base, self.source, self.code_pages) catch |err| switch (err) {
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
        const primary = Compiler.evaluateNumberExpression(node.primary_language_id, self.source, self.code_pages);
        const sublanguage = Compiler.evaluateNumberExpression(node.sublanguage_id, self.source, self.code_pages);
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

        pub fn init(allocator: Allocator, id_bytes: SourceBytes, type_bytes: SourceBytes, data_size: DWORD, language: res.Language) !ResourceHeader {
            const type_value = type: {
                const resource_type = Resource.fromString(type_bytes);
                if (resource_type != .user_defined) {
                    if (res.RT.fromResource(resource_type)) |rt_constant| {
                        break :type NameOrOrdinal{ .ordinal = @enumToInt(rt_constant) };
                    } else {
                        std.debug.print("{}\n", .{resource_type});
                        @panic("TODO: unhandled resource -> RT constant conversion");
                    }
                } else {
                    break :type try NameOrOrdinal.fromString(allocator, type_bytes);
                }
            };
            errdefer type_value.deinit(allocator);

            const name_value = try NameOrOrdinal.fromString(allocator, id_bytes);
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

        pub fn applyOptionalStatements(self: *ResourceHeader, statements: []*Node, source: []const u8, code_page_lookup: *const CodePageLookup) void {
            applyToOptionalStatements(&self.language, &self.version, &self.characteristics, statements, source, code_page_lookup);
        }
    };

    fn applyToMemoryFlags(flags: *MemoryFlags, tokens: []Token, source: []const u8) void {
        for (tokens) |token| {
            const attribute = rc.CommonResourceAttributes.map.get(token.slice(source)).?;
            flags.set(attribute);
        }
    }

    /// Only handles the 'base' optional statements that are shared between resource types.
    fn applyToOptionalStatements(language: *res.Language, version: *u32, characteristics: *u32, statements: []*Node, source: []const u8, code_page_lookup: *const CodePageLookup) void {
        for (statements) |node| switch (node.id) {
            .language_statement => {
                const language_statement = @fieldParentPtr(Node.LanguageStatement, "base", node);
                language.* = languageFromLanguageStatement(language_statement, source, code_page_lookup);
            },
            .simple_statement => {
                const simple_statement = @fieldParentPtr(Node.SimpleStatement, "base", node);
                const statement_type = rc.OptionalStatements.map.get(simple_statement.identifier.slice(source)) orelse continue;
                const result = Compiler.evaluateNumberExpression(simple_statement.value, source, code_page_lookup);
                switch (statement_type) {
                    .version => version.* = result.value,
                    .characteristics => characteristics.* = result.value,
                    else => unreachable, // only VERSION and CHARACTERISTICS should be in an optional statements list
                }
            },
            else => {},
        };
    }

    pub fn languageFromLanguageStatement(language_statement: *const Node.LanguageStatement, source: []const u8, code_page_lookup: *const CodePageLookup) res.Language {
        const primary = Compiler.evaluateNumberExpression(language_statement.primary_language_id, source, code_page_lookup);
        const sublanguage = Compiler.evaluateNumberExpression(language_statement.sublanguage_id, source, code_page_lookup);
        return .{
            .primary_language_id = @truncate(u10, primary.value),
            .sublanguage_id = @truncate(u6, sublanguage.value),
        };
    }

    pub fn getLanguageFromOptionalStatements(statements: []*Node, source: []const u8, code_page_lookup: *const CodePageLookup) ?res.Language {
        for (statements) |node| switch (node.id) {
            .language_statement => {
                const language_statement = @fieldParentPtr(Node.LanguageStatement, "base", node);
                return languageFromLanguageStatement(language_statement, source, code_page_lookup);
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

    pub fn sourceBytesForToken(self: *Compiler, token: Token) SourceBytes {
        return .{
            .slice = token.slice(self.source),
            .code_page = self.code_pages.getForToken(token),
        };
    }

    /// Helper that calls parseQuotedStringAsWideString with the relevant context
    /// Resulting slice is allocated by `self.allocator`.
    pub fn parseQuotedStringAsWideString(self: *Compiler, token: Token) ![:0]u16 {
        return literals.parseQuotedStringAsWideString(
            self.allocator,
            self.sourceBytesForToken(token),
            .{
                .start_column = token.calculateColumn(self.source, 8, null),
                .diagnostics = .{ .diagnostics = self.diagnostics, .token = token },
            },
        );
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
            .name_value = try NameOrOrdinal.nameFromString(compiler.allocator, .{ .slice = "FONTDIR", .code_page = .windows1252 }),
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

    pub fn set(self: *StringTablesByLanguage, allocator: Allocator, language: res.Language, id: u16, string_token: Token, node: *Node, source: []const u8, code_page_lookup: *const CodePageLookup) StringTable.SetError!void {
        var get_or_put_result = try self.tables.getOrPut(allocator, language);
        if (!get_or_put_result.found_existing) {
            get_or_put_result.value_ptr.* = StringTable{};
        }
        return get_or_put_result.value_ptr.set(allocator, id, string_token, node, source, code_page_lookup);
    }
};

pub const StringTable = struct {
    /// Blocks are written to the .res file in order depending on when the first string
    /// was added to the block (i.e. `STRINGTABLE { 16 "b" 0 "a" }` would then get written
    /// with block ID 2 (the one with "b") first and block ID 1 (the one with "a") second).
    /// Using an ArrayHashMap here gives us this property for free.
    blocks: std.AutoArrayHashMapUnmanaged(u16, Block) = .{},

    pub const Block = struct {
        strings: std.ArrayListUnmanaged(Token) = .{},
        set_indexes: std.bit_set.IntegerBitSet(16) = .{ .mask = 0 },
        memory_flags: MemoryFlags = MemoryFlags.defaults(res.RT.STRING),
        characteristics: u32 = 0,
        version: u32 = 0,

        /// Returns the index to insert the string into the `strings` list.
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
            const count = self.strings.items.len;
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
                const token = self.strings.items[string_index];
                std.debug.print("{}: [{}] {any}\n", .{ bit_index, string_index, token });
                string_index += 1;
            }
        }

        pub fn applyNodeAttributes(self: *Block, node: *Node, source: []const u8, code_page_lookup: *const CodePageLookup) void {
            switch (node.id) {
                .string_table => {
                    const string_table = @fieldParentPtr(Node.StringTable, "base", node);
                    Compiler.applyToMemoryFlags(&self.memory_flags, string_table.common_resource_attributes, source);
                    var dummy_language: res.Language = undefined;
                    Compiler.applyToOptionalStatements(&dummy_language, &self.version, &self.characteristics, string_table.optional_statements, source, code_page_lookup);
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

                const string_token = self.strings.items[string_i];
                const slice = string_token.slice(compiler.source);
                const column = string_token.calculateColumn(compiler.source, 8, null);
                const code_page = compiler.code_pages.getForToken(string_token);
                const bytes = SourceBytes{ .slice = slice, .code_page = code_page };
                const utf16_string = utf16: {
                    switch (string_token.id) {
                        .quoted_ascii_string => {
                            const parsed = try literals.parseQuotedAsciiString(compiler.allocator, bytes, .{
                                .start_column = column,
                                .diagnostics = .{ .diagnostics = compiler.diagnostics, .token = string_token },
                            });
                            defer compiler.allocator.free(parsed);
                            // TODO: Should this be UTF-8? parseQuotedAsciiString returns a Windows-1252 encoded string.
                            break :utf16 try std.unicode.utf8ToUtf16LeWithNull(compiler.allocator, parsed);
                        },
                        .quoted_wide_string => break :utf16 try literals.parseQuotedWideString(compiler.allocator, bytes, .{
                            .start_column = column,
                            .diagnostics = .{ .diagnostics = compiler.diagnostics, .token = string_token },
                        }),
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
            entry.value_ptr.strings.deinit(allocator);
        }
        self.blocks.deinit(allocator);
    }

    const SetError = error{StringAlreadyDefined} || Allocator.Error;

    pub fn set(self: *StringTable, allocator: Allocator, id: u16, string_token: Token, node: *Node, source: []const u8, code_page_lookup: *const CodePageLookup) SetError!void {
        const block_id = (id / 16) + 1;
        const string_index: u8 = @intCast(u8, id & 0xF);

        var get_or_put_result = try self.blocks.getOrPut(allocator, block_id);
        if (!get_or_put_result.found_existing) {
            get_or_put_result.value_ptr.* = Block{};
            get_or_put_result.value_ptr.applyNodeAttributes(node, source, code_page_lookup);
        } else {
            if (get_or_put_result.value_ptr.set_indexes.isSet(string_index)) {
                return error.StringAlreadyDefined;
            }
        }

        var block = get_or_put_result.value_ptr;
        if (block.getInsertionIndex(string_index)) |insertion_index| {
            try block.strings.insert(allocator, insertion_index, string_token);
        } else {
            try block.strings.append(allocator, string_token);
        }
        block.set_indexes.set(string_index);
    }

    pub fn get(self: *StringTable, id: u16) ?Token {
        const block_id = (id / 16) + 1;
        const string_index = @intCast(u8, id & 0xF);

        const block = self.blocks.getPtr(block_id) orelse return null;
        const token_index = block.getTokenIndex(string_index) orelse return null;
        return block.strings.items[token_index];
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

    var code_page_lookup = CodePageLookup.init(allocator, .windows1252);
    defer code_page_lookup.deinit();

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
        try string_table.set(allocator, id, S.makeDummyToken(id), &dummy_node.base, "", &code_page_lookup);
    }

    // make sure each one exists and is the right value when gotten
    var id: u16 = 0;
    while (id < 100) : (id += 1) {
        const dummy = S.makeDummyToken(id);
        try std.testing.expectError(error.StringAlreadyDefined, string_table.set(allocator, id, dummy, &dummy_node.base, "", &code_page_lookup));
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

    compile(std.testing.allocator, source, buffer.writer(), cwd, &diagnostics) catch |err| switch (err) {
        error.ParseError, error.CompileError => {
            diagnostics.renderToStdErr(cwd, source, null);
            return err;
        },
        else => return err,
    };

    try std.testing.expectEqualSlices(u8, expected_output, buffer.items);
}

fn testCompileError(expected_error_str: []const u8, source: []const u8) !void {
    const allocator = std.testing.allocator;

    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();

    var diagnostics = Diagnostics.init(allocator);
    defer diagnostics.deinit();

    compile(std.testing.allocator, source, buffer.writer(), std.fs.cwd(), &diagnostics) catch |err| switch (err) {
        error.ParseError, error.CompileError => {
            if (diagnostics.errors.items.len < 1) return error.NoDiagnostics;
            if (diagnostics.errors.items[0].type != .err) @panic("TODO handle compile error test with a non-error as the first error");
            var buf: [256]u8 = undefined;
            var fbs = std.io.fixedBufferStream(&buf);
            try diagnostics.errors.items[0].render(fbs.writer(), source, diagnostics.strings.items);
            try std.testing.expectEqualStrings(expected_error_str, fbs.getWritten());
            return;
        },
        else => return err,
    };
    std.debug.print("expected compile error, got .res:\n", .{});
    std.testing.expectEqualSlices(u8, "", buffer.items) catch {};
    return error.UnexpectedSuccess;
}

pub fn getExpectedFromWindowsRCWithDir(allocator: Allocator, source: []const u8, cwd: std.fs.Dir, cwd_path: []const u8) ![]const u8 {
    try cwd.writeFile("test.rc", source);

    var result = try std.ChildProcess.exec(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            // Note: This relies on `rc.exe` being in the PATH
            "rc.exe",
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

test "NameOrOrdinal" {
    // overflow *is* allowed for both id and type
    try testCompileWithOutput(
        "65635 65635 {}",
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00 \x00\x00\x00\xff\xffc\x00\xff\xffc\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00",
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
    try testCompileError(
        "accelerator type [ASCII or VIRTKEY] required when key is an integer",
        "1 ACCELERATORS { 1, 1 }",
    );
}

test "dialog, dialogex resource" {
    try testCompileWithOutput(
        \\1 DIALOGEX FIXED DISCARDABLE 1, 2, 3, 4
        \\STYLE 0x80000000 | 0x00800000
        \\CAPTION "Error!"
        \\EXSTYLE 1
        \\CLASS "hello1"
        \\CLASS 2
        \\MENU "1"
        \\FONT 12 "first", 1001-1, 65537L, 257-2
        \\FONT 8+2,, ,, "second", 0
        \\{}
    ,
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00H\x00\x00\x00 \x00\x00\x00\xff\xff\x05\x00\xff\xff\x01\x00\x00\x00\x00\x000\x10\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\xff\xff\x00\x00\x00\x00\x01\x00\x00\x00@\x00\xc0\x80\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\"\x001\x00\"\x00\x00\x00\xff\xff\x02\x00E\x00r\x00r\x00o\x00r\x00!\x00\x00\x00\n\x00\x00\x00\x01\x01s\x00e\x00c\x00o\x00n\x00d\x00\x00\x00",
        std.fs.cwd(),
    );
    try testCompileWithOutput(
        \\1 DIALOGEX 1, 2, 3, 4
        \\MENU 5+5
        \\CLASS 5+5
        \\{}
    ,
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00$\x00\x00\x00 \x00\x00\x00\xff\xff\x05\x00\xff\xff\x01\x00\x00\x00\x00\x000\x10\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x88\x80\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xc7\x01\xff\xff\n\x00\x00\x00",
        std.fs.cwd(),
    );
    try testCompileWithOutput(
        \\1 DIALOGEX 1, 2, 3, 4
        \\CLASS 5+5
        \\CLASS "forced ordinal"
        \\{}
    ,
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\"\x00\x00\x00 \x00\x00\x00\xff\xff\x05\x00\xff\xff\x01\x00\x00\x00\x00\x000\x10\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x88\x80\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x00\x00\xff\xffF\xf5\x00\x00\x00\x00",
        std.fs.cwd(),
    );
    try testCompileWithOutput(
        \\1 DIALOG 1, 2, 3, 4 {}
    ,
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00 \x00\x00\x00\xff\xff\x05\x00\xff\xff\x01\x00\x00\x00\x00\x000\x10\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x88\x80\x00\x00\x00\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x00\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
    try testCompileWithOutput(
        \\1 DIALOGEX 1, 2, 3, 4 {
        \\  AUTO3STATE,, "mytext",, 900-1,, 1 2 3 4, 0x22222222, 0x12345678, 100 { "AUTO3STATE" }
        \\}
    ,
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00V\x00\x00\x00 \x00\x00\x00\xff\xff\x05\x00\xff\xff\x01\x00\x00\x00\x00\x000\x10\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x88\x80\x01\x00\x01\x00\x02\x00\x03\x00\x04\x00\x00\x00\x00\x00\x00\x00d\x00\x00\x00xV4\x12&\"#r\x01\x00\x02\x00\x03\x00\x04\x00\x83\x03\x00\x00\xff\xff\x80\x00m\x00y\x00t\x00e\x00x\x00t\x00\x00\x00\n\x00AUTO3STATE\x00\x00",
        std.fs.cwd(),
    );
    // Note: Extra data is not tested extensively here due to a miscompilation in the Windows RC
    //       compiler where it would add extra padding bytes if the data of a control ends on
    //       an odd offset. We avoid that here by ensuring that the extra data has an even number
    //       of bytes.
    try testCompileWithOutput(
        \\1 DIALOGEX 1, 2, 3, 4
        \\{
        \\    AUTO3STATE,, "mytext",, 900,, 1 2 3 4, 0, 0, 100 { "1234" }
        \\    AUTOCHECKBOX "mytext", 901, 1, 2, 3, 4, 0, 0, 100
        \\    AUTORADIOBUTTON "mytext", 902, 1, 2, 3, 4, 0, 0, 100
        \\    CHECKBOX "mytext", 903, 1, 2, 3, 4, 0, 0, 100
        \\    COMBOBOX 904,, 1 2 3 4, 0, 0, 100
        \\    CTEXT "mytext", 906, 1, 2, 3, 4, 0, 0, 100
        \\    CTEXT "mytext", 9061, 1, 2, 3, 4
        \\    DEFPUSHBUTTON "mytext", 907, 1, 2, 3, 4, 0, 0, 100
        \\    EDITTEXT 908, 1, 2, 3, 4, 0, 0, 100
        \\    HEDIT 9081, 1, 2, 3, 4, 0, 0, 100
        \\    IEDIT 9082, 1, 2, 3, 4, 0, 0, 100
        \\    GROUPBOX "mytext", 909, 1, 2, 3, 4, 0, 0, 100
        \\    ICON "mytext", 910, 1, 2, 3, 4, 0, 0, 100
        \\    LISTBOX 911, 1, 2, 3, 4, 0, 0, 100
        \\    LTEXT "mytext", 912, 1, 2, 3, 4, 0, 0, 100
        \\    PUSHBOX "mytext", 913, 1, 2, 3, 4, 0, 0, 100
        \\    PUSHBUTTON "mytext", 914, 1, 2, 3, 4, 0, 0, 100
        \\    RADIOBUTTON "mytext", 915, 1, 2, 3, 4, 0, 0, 100
        \\    RTEXT "mytext", 916, 1, 2, 3, 4, 0, 0, 100
        \\    SCROLLBAR 917, 1, 2, 3, 4, 0, 0, 100
        \\    STATE3 "mytext", 918, 1, 2, 3, 4, 0, 0, 100
        \\    USERBUTTON "mytext", 919, 1, 2, 3, 4, 0, 0, 100
        \\}
    ,
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa4\x03\x00\x00 \x00\x00\x00\xff\xff\x05\x00\xff\xff\x01\x00\x00\x00\x00\x000\x10\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x88\x80\x16\x00\x01\x00\x02\x00\x03\x00\x04\x00\x00\x00\x00\x00\x00\x00d\x00\x00\x00\x00\x00\x00\x00\x06\x00\x01P\x01\x00\x02\x00\x03\x00\x04\x00\x84\x03\x00\x00\xff\xff\x80\x00m\x00y\x00t\x00e\x00x\x00t\x00\x00\x00\x04\x001234d\x00\x00\x00\x00\x00\x00\x00\x03\x00\x01P\x01\x00\x02\x00\x03\x00\x04\x00\x85\x03\x00\x00\xff\xff\x80\x00m\x00y\x00t\x00e\x00x\x00t\x00\x00\x00\x00\x00d\x00\x00\x00\x00\x00\x00\x00\t\x00\x00P\x01\x00\x02\x00\x03\x00\x04\x00\x86\x03\x00\x00\xff\xff\x80\x00m\x00y\x00t\x00e\x00x\x00t\x00\x00\x00\x00\x00d\x00\x00\x00\x00\x00\x00\x00\x02\x00\x01P\x01\x00\x02\x00\x03\x00\x04\x00\x87\x03\x00\x00\xff\xff\x80\x00m\x00y\x00t\x00e\x00x\x00t\x00\x00\x00\x00\x00d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00P\x01\x00\x02\x00\x03\x00\x04\x00\x88\x03\x00\x00\xff\xff\x85\x00\x00\x00\x00\x00d\x00\x00\x00\x00\x00\x00\x00\x01\x00\x02P\x01\x00\x02\x00\x03\x00\x04\x00\x8a\x03\x00\x00\xff\xff\x82\x00m\x00y\x00t\x00e\x00x\x00t\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x02P\x01\x00\x02\x00\x03\x00\x04\x00e#\x00\x00\xff\xff\x82\x00m\x00y\x00t\x00e\x00x\x00t\x00\x00\x00\x00\x00d\x00\x00\x00\x00\x00\x00\x00\x01\x00\x01P\x01\x00\x02\x00\x03\x00\x04\x00\x8b\x03\x00\x00\xff\xff\x80\x00m\x00y\x00t\x00e\x00x\x00t\x00\x00\x00\x00\x00d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x81P\x01\x00\x02\x00\x03\x00\x04\x00\x8c\x03\x00\x00\xff\xff\x81\x00\x00\x00\x00\x00d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x81P\x01\x00\x02\x00\x03\x00\x04\x00y#\x00\x00\xff\xff\x81\x00\x00\x00\x00\x00d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x81P\x01\x00\x02\x00\x03\x00\x04\x00z#\x00\x00\xff\xff\x81\x00\x00\x00\x00\x00d\x00\x00\x00\x00\x00\x00\x00\x07\x00\x00P\x01\x00\x02\x00\x03\x00\x04\x00\x8d\x03\x00\x00\xff\xff\x80\x00m\x00y\x00t\x00e\x00x\x00t\x00\x00\x00\x00\x00d\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00P\x01\x00\x02\x00\x03\x00\x04\x00\x8e\x03\x00\x00\xff\xff\x82\x00m\x00y\x00t\x00e\x00x\x00t\x00\x00\x00\x00\x00d\x00\x00\x00\x00\x00\x00\x00\x01\x00\x80P\x01\x00\x02\x00\x03\x00\x04\x00\x8f\x03\x00\x00\xff\xff\x83\x00\x00\x00\x00\x00d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02P\x01\x00\x02\x00\x03\x00\x04\x00\x90\x03\x00\x00\xff\xff\x82\x00m\x00y\x00t\x00e\x00x\x00t\x00\x00\x00\x00\x00d\x00\x00\x00\x00\x00\x00\x00\n\x00\x01P\x01\x00\x02\x00\x03\x00\x04\x00\x91\x03\x00\x00\xff\xff\x80\x00m\x00y\x00t\x00e\x00x\x00t\x00\x00\x00\x00\x00d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01P\x01\x00\x02\x00\x03\x00\x04\x00\x92\x03\x00\x00\xff\xff\x80\x00m\x00y\x00t\x00e\x00x\x00t\x00\x00\x00\x00\x00d\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00P\x01\x00\x02\x00\x03\x00\x04\x00\x93\x03\x00\x00\xff\xff\x80\x00m\x00y\x00t\x00e\x00x\x00t\x00\x00\x00\x00\x00d\x00\x00\x00\x00\x00\x00\x00\x02\x00\x02P\x01\x00\x02\x00\x03\x00\x04\x00\x94\x03\x00\x00\xff\xff\x82\x00m\x00y\x00t\x00e\x00x\x00t\x00\x00\x00\x00\x00d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00P\x01\x00\x02\x00\x03\x00\x04\x00\x95\x03\x00\x00\xff\xff\x84\x00\x00\x00\x00\x00d\x00\x00\x00\x00\x00\x00\x00\x05\x00\x01P\x01\x00\x02\x00\x03\x00\x04\x00\x96\x03\x00\x00\xff\xff\x80\x00m\x00y\x00t\x00e\x00x\x00t\x00\x00\x00\x00\x00d\x00\x00\x00\x00\x00\x00\x00\x08\x00\x01P\x01\x00\x02\x00\x03\x00\x04\x00\x97\x03\x00\x00\xff\xff\x80\x00m\x00y\x00t\x00e\x00x\x00t\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );

    try testCompileWithOutput(
        \\1 DIALOGEX 1, 2, 3, 4
        \\{
        \\    CONTROL "mytext",, 905,, "\x42UTTON",, 1,, 2 3 4 0, 0, 100
        \\    CONTROL "mytext",, 905,, L"EDIT",, 1,, 2 3 4 0, 0, 100
        \\    CONTROL "mytext",, 905,, COMBOBOX,, 1,, 2 3 4 0, 0, 100
        \\}
    ,
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa4\x00\x00\x00 \x00\x00\x00\xff\xff\x05\x00\xff\xff\x01\x00\x00\x00\x00\x000\x10\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x88\x80\x03\x00\x01\x00\x02\x00\x03\x00\x04\x00\x00\x00\x00\x00\x00\x00d\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00P\x02\x00\x03\x00\x04\x00\x00\x00\x89\x03\x00\x00\xff\xff\x80\x00m\x00y\x00t\x00e\x00x\x00t\x00\x00\x00\x00\x00d\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00P\x02\x00\x03\x00\x04\x00\x00\x00\x89\x03\x00\x00\xff\xff\x81\x00m\x00y\x00t\x00e\x00x\x00t\x00\x00\x00\x00\x00d\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00P\x02\x00\x03\x00\x04\x00\x00\x00\x89\x03\x00\x00\xff\xff\x85\x00m\x00y\x00t\x00e\x00x\x00t\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
}

test "menu, menuex resource" {
    try testCompileWithOutput(
        \\1 MENU
        \\{
        \\    MENUITEM SEPARATOR
        \\    MENUITEM "hello", 100, CHECKED, GRAYED, HELP, INACTIVE, MENUBARBREAK, MENUBREAK
        \\}
    ,
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x00\x00 \x00\x00\x00\xff\xff\x04\x00\xff\xff\x01\x00\x00\x00\x00\x000\x10\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xeb@d\x00h\x00e\x00l\x00l\x00o\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
    try testCompileWithOutput(
        \\1 MENU
        \\{
        \\  POPUP "hello", CHECKED, GRAYED, HELP, INACTIVE, MENUBARBREAK, MENUBREAK {
        \\    MENUITEM SEPARATOR
        \\    POPUP "" { MENUITEM SEPARATOR }
        \\    POPUP "" { MENUITEM SEPARATOR }
        \\  }
        \\}
    ,
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00,\x00\x00\x00 \x00\x00\x00\xff\xff\x04\x00\xff\xff\x01\x00\x00\x00\x00\x000\x10\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfb@h\x00e\x00l\x00l\x00o\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x80\x00\x00\x00\x00\x00\x90\x00\x00\x00\x80\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
    try testCompileWithOutput(
        \\1 MENUEX 1000
        \\BEGIN
        \\  POPUP "&File", 200,,, 1001
        \\  BEGIN
        \\    MENUITEM "&Open\tCtrl+O", 100
        \\    MENUITEM "", -1, 0x00000800L
        \\    MENUITEM "&Exit\tAlt+X",  101
        \\  END
        \\  POPUP "&View", 201,,, 1002
        \\  BEGIN
        \\    MENUITEM "&Status Bar", 102,, 0x00000008L
        \\  END
        \\END
    ,
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd0\x00\x00\x00 \x00\x00\x00\xff\xff\x04\x00\xff\xff\x01\x00\x00\x00\x00\x000\x10\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x04\x00\xe8\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc8\x00\x00\x00\x01\x00&\x00F\x00i\x00l\x00e\x00\x00\x00\x00\x00\xe9\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00d\x00\x00\x00\x00\x00&\x00O\x00p\x00e\x00n\x00\t\x00C\x00t\x00r\x00l\x00+\x00O\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\xff\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00e\x00\x00\x00\x80\x00&\x00E\x00x\x00i\x00t\x00\t\x00A\x00l\x00t\x00+\x00X\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc9\x00\x00\x00\x81\x00&\x00V\x00i\x00e\x00w\x00\x00\x00\x00\x00\xea\x03\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00f\x00\x00\x00\x80\x00&\x00S\x00t\x00a\x00t\x00u\x00s\x00 \x00B\x00a\x00r\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
}

test "versioninfo resource" {
    try testCompileWithOutput(
        \\1 VERSIONINFO FIXED
        \\FILEVERSION 1
        \\PRODUCTVERSION 1,3-1,3,4
        \\FILEFLAGSMASK 1
        \\FILEFLAGS (1|2)
        \\FILEOS 2
        \\FILETYPE 3
        \\FILESUBTYPE 4
        \\{}
    ,
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\\\x00\x00\x00 \x00\x00\x00\xff\xff\x10\x00\xff\xff\x01\x00\x00\x00\x00\x00 \x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\\\x004\x00\x00\x00V\x00S\x00_\x00V\x00E\x00R\x00S\x00I\x00O\x00N\x00_\x00I\x00N\x00F\x00O\x00\x00\x00\x00\x00\xbd\x04\xef\xfe\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x00\x00\x02\x00\x01\x00\x04\x00\x03\x00\x01\x00\x00\x00\x03\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00",
        std.fs.cwd(),
    );
    // This tests some edge cases around padding, data size, etc
    try testCompileWithOutput(
        \\test VERSIONINFO
        \\BEGIN
        \\  VALUE "key" 1L 2
        \\END
    ,
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00r\x00\x00\x00(\x00\x00\x00\xff\xff\x10\x00T\x00E\x00S\x00T\x00\x00\x00\x00\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00r\x004\x00\x00\x00V\x00S\x00_\x00V\x00E\x00R\x00S\x00I\x00O\x00N\x00_\x00I\x00N\x00F\x00O\x00\x00\x00\x00\x00\xbd\x04\xef\xfe\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x16\x00\x06\x00\x00\x00k\x00e\x00y\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00",
        std.fs.cwd(),
    );
    // TODO: This seems to cause a miscompilation in the Win32 RC (no padding
    //       bytes are added before the "a", it's size is reported as 1)
    // try testCompileWithOutput(
    //     \\test VERSIONINFO
    //     \\BEGIN
    //     \\  VALUE "key" 1L 2
    //     \\  BLOCK "" BEGIN VALUE "key" "a" END
    //     \\END
    // ,
    //     "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8e\x00\x00\x00(\x00\x00\x00\xff\xff\x10\x00T\x00E\x00S\x00T\x00\x00\x00\x00\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x8e\x004\x00\x00\x00V\x00S\x00_\x00V\x00E\x00R\x00S\x00I\x00O\x00N\x00_\x00I\x00N\x00F\x00O\x00\x00\x00\x00\x00\xbd\x04\xef\xfe\x00\x00\x01\x00\x02\x00\x01\x00\x04\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x16\x00\x06\x00\x00\x00k\x00e\x00y\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x1a\x00\x00\x00\x01\x00\x00\x00\x12\x00\x01\x00\x01\x00k\x00e\x00y\x00\x00\x00a\x00\x00\x00\x00\x00",
    //     std.fs.cwd(),
    // );
}

test "shell32 versioninfo" {
    // This is the shell32.dll VERSIONINFO from https://devblogs.microsoft.com/oldnewthing/20061221-02/?p=28643
    try testCompileWithOutput(
        \\1 VERSIONINFO
        \\FILEVERSION    3,0,2900,2869
        \\PRODUCTVERSION 3,0,2900,2869
        \\FILEFLAGSMASK  0x0000003FL
        \\FILEFLAGS      0
        \\FILEOS         0x00040004L
        \\FILETYPE       0x00000002L
        \\FILESUBTYPE    0x00000000L
        \\BEGIN
        \\ BLOCK "StringFileInfo"
        \\ BEGIN
        \\  BLOCK "040904B0"
        \\  BEGIN
        \\   VALUE "CompanyName", "Microsoft Corporation"
        \\   VALUE "FileDescription", "Windows Shell Common Dll"
        \\   VALUE "FileVersion", "6.00.2900.2869 (xpsp_sp2_gdr.060316-1512)"
        \\   VALUE "InternalName", "SHELL32"
        \\   VALUE "LegalCopyright", "\251 Microsoft Corporation. All rights reserved."
        \\   VALUE "OriginalFilename", "SHELL32.DLL"
        \\   VALUE "ProductName", "Microsoft\256 Windows\256 Operating System"
        \\   VALUE "ProductVersion", "6.00.2900.2869"
        \\  END
        \\ END
        \\ BLOCK "VarFileInfo"
        \\ BEGIN
        \\  VALUE "Translation", 0x0409, 0x04B0
        \\ END
        \\END
    ,
        "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x98\x03\x00\x00 \x00\x00\x00\xff\xff\x10\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00\x98\x034\x00\x00\x00V\x00S\x00_\x00V\x00E\x00R\x00S\x00I\x00O\x00N\x00_\x00I\x00N\x00F\x00O\x00\x00\x00\x00\x00\xbd\x04\xef\xfe\x00\x00\x01\x00\x00\x00\x03\x005\x0bT\x0b\x00\x00\x03\x005\x0bT\x0b?\x00\x00\x00\x00\x00\x00\x00\x04\x00\x04\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf6\x02\x00\x00\x01\x00S\x00t\x00r\x00i\x00n\x00g\x00F\x00i\x00l\x00e\x00I\x00n\x00f\x00o\x00\x00\x00\xd2\x02\x00\x00\x01\x000\x004\x000\x009\x000\x004\x00B\x000\x00\x00\x00L\x00\x16\x00\x01\x00C\x00o\x00m\x00p\x00a\x00n\x00y\x00N\x00a\x00m\x00e\x00\x00\x00\x00\x00M\x00i\x00c\x00r\x00o\x00s\x00o\x00f\x00t\x00 \x00C\x00o\x00r\x00p\x00o\x00r\x00a\x00t\x00i\x00o\x00n\x00\x00\x00Z\x00\x19\x00\x01\x00F\x00i\x00l\x00e\x00D\x00e\x00s\x00c\x00r\x00i\x00p\x00t\x00i\x00o\x00n\x00\x00\x00\x00\x00W\x00i\x00n\x00d\x00o\x00w\x00s\x00 \x00S\x00h\x00e\x00l\x00l\x00 \x00C\x00o\x00m\x00m\x00o\x00n\x00 \x00D\x00l\x00l\x00\x00\x00\x00\x00t\x00*\x00\x01\x00F\x00i\x00l\x00e\x00V\x00e\x00r\x00s\x00i\x00o\x00n\x00\x00\x00\x00\x006\x00.\x000\x000\x00.\x002\x009\x000\x000\x00.\x002\x008\x006\x009\x00 \x00(\x00x\x00p\x00s\x00p\x00_\x00s\x00p\x002\x00_\x00g\x00d\x00r\x00.\x000\x006\x000\x003\x001\x006\x00-\x001\x005\x001\x002\x00)\x00\x00\x000\x00\x08\x00\x01\x00I\x00n\x00t\x00e\x00r\x00n\x00a\x00l\x00N\x00a\x00m\x00e\x00\x00\x00S\x00H\x00E\x00L\x00L\x003\x002\x00\x00\x00\x80\x00.\x00\x01\x00L\x00e\x00g\x00a\x00l\x00C\x00o\x00p\x00y\x00r\x00i\x00g\x00h\x00t\x00\x00\x00\xa9\x00 \x00M\x00i\x00c\x00r\x00o\x00s\x00o\x00f\x00t\x00 \x00C\x00o\x00r\x00p\x00o\x00r\x00a\x00t\x00i\x00o\x00n\x00.\x00 \x00A\x00l\x00l\x00 \x00r\x00i\x00g\x00h\x00t\x00s\x00 \x00r\x00e\x00s\x00e\x00r\x00v\x00e\x00d\x00.\x00\x00\x00@\x00\x0c\x00\x01\x00O\x00r\x00i\x00g\x00i\x00n\x00a\x00l\x00F\x00i\x00l\x00e\x00n\x00a\x00m\x00e\x00\x00\x00S\x00H\x00E\x00L\x00L\x003\x002\x00.\x00D\x00L\x00L\x00\x00\x00j\x00%\x00\x01\x00P\x00r\x00o\x00d\x00u\x00c\x00t\x00N\x00a\x00m\x00e\x00\x00\x00\x00\x00M\x00i\x00c\x00r\x00o\x00s\x00o\x00f\x00t\x00\xae\x00 \x00W\x00i\x00n\x00d\x00o\x00w\x00s\x00\xae\x00 \x00O\x00p\x00e\x00r\x00a\x00t\x00i\x00n\x00g\x00 \x00S\x00y\x00s\x00t\x00e\x00m\x00\x00\x00\x00\x00B\x00\x0f\x00\x01\x00P\x00r\x00o\x00d\x00u\x00c\x00t\x00V\x00e\x00r\x00s\x00i\x00o\x00n\x00\x00\x006\x00.\x000\x000\x00.\x002\x009\x000\x000\x00.\x002\x008\x006\x009\x00\x00\x00\x00\x00D\x00\x00\x00\x01\x00V\x00a\x00r\x00F\x00i\x00l\x00e\x00I\x00n\x00f\x00o\x00\x00\x00\x00\x00$\x00\x04\x00\x00\x00T\x00r\x00a\x00n\x00s\x00l\x00a\x00t\x00i\x00o\x00n\x00\x00\x00\x00\x00\t\x04\xb0\x04",
        std.fs.cwd(),
    );
}
