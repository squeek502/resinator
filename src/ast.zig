const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("lex.zig").Token;
const CodePage = @import("code_pages.zig").CodePage;

pub const Tree = struct {
    node: *Node,
    code_pages: CodePageLookup,

    /// not owned by the tree
    source: []const u8,

    arena: std.heap.ArenaAllocator.State,
    allocator: Allocator,

    pub fn deinit(self: *Tree) void {
        self.arena.promote(self.allocator).deinit();
    }

    pub fn root(self: *Tree) *Node.Root {
        return @fieldParentPtr(Node.Root, "base", self.node);
    }

    pub fn dump(self: *Tree, writer: anytype) @TypeOf(writer).Error!void {
        try self.node.dump(self, writer, 0);
    }
};

pub const CodePageLookup = struct {
    lookup: std.ArrayListUnmanaged(CodePage) = .{},
    allocator: Allocator,
    default_code_page: CodePage,

    pub fn init(allocator: Allocator, default_code_page: CodePage) CodePageLookup {
        return .{
            .allocator = allocator,
            .default_code_page = default_code_page,
        };
    }

    pub fn deinit(self: *CodePageLookup) void {
        self.lookup.deinit(self.allocator);
    }

    /// line_num is 1-indexed
    pub fn setForLineNum(self: *CodePageLookup, line_num: usize, code_page: CodePage) !void {
        const index = line_num - 1;
        if (index >= self.lookup.items.len) {
            const new_size = line_num;
            const missing_lines_start_index = self.lookup.items.len;
            try self.lookup.resize(self.allocator, new_size);

            // If there are any gaps created, we need to fill them in with the value of the
            // last line before the gap. This can happen for e.g. string literals that
            // span multiple lines, or if the start of a file has multiple empty lines.
            const fill_value = if (missing_lines_start_index > 0)
                self.lookup.items[missing_lines_start_index - 1]
            else
                self.default_code_page;
            var i: usize = missing_lines_start_index;
            while (i < new_size - 1) : (i += 1) {
                self.lookup.items[i] = fill_value;
            }
        }
        self.lookup.items[index] = code_page;
    }

    pub fn setForToken(self: *CodePageLookup, token: Token, code_page: CodePage) !void {
        return self.setForLineNum(token.line_number, code_page);
    }

    /// line_num is 1-indexed
    pub fn getForLineNum(self: CodePageLookup, line_num: usize) CodePage {
        return self.lookup.items[line_num - 1];
    }

    pub fn getForToken(self: CodePageLookup, token: Token) CodePage {
        return self.getForLineNum(token.line_number);
    }
};

test "CodePageLookup" {
    var lookup = CodePageLookup.init(std.testing.allocator, .windows1252);
    defer lookup.deinit();

    try lookup.setForLineNum(5, .utf8);
    try std.testing.expectEqual(CodePage.windows1252, lookup.getForLineNum(1));
    try std.testing.expectEqual(CodePage.windows1252, lookup.getForLineNum(2));
    try std.testing.expectEqual(CodePage.windows1252, lookup.getForLineNum(3));
    try std.testing.expectEqual(CodePage.windows1252, lookup.getForLineNum(4));
    try std.testing.expectEqual(CodePage.utf8, lookup.getForLineNum(5));
    try std.testing.expectEqual(@as(usize, 5), lookup.lookup.items.len);

    try lookup.setForLineNum(7, .windows1252);
    try std.testing.expectEqual(CodePage.windows1252, lookup.getForLineNum(1));
    try std.testing.expectEqual(CodePage.windows1252, lookup.getForLineNum(2));
    try std.testing.expectEqual(CodePage.windows1252, lookup.getForLineNum(3));
    try std.testing.expectEqual(CodePage.windows1252, lookup.getForLineNum(4));
    try std.testing.expectEqual(CodePage.utf8, lookup.getForLineNum(5));
    try std.testing.expectEqual(CodePage.utf8, lookup.getForLineNum(6));
    try std.testing.expectEqual(CodePage.windows1252, lookup.getForLineNum(7));
    try std.testing.expectEqual(@as(usize, 7), lookup.lookup.items.len);
}

pub const Node = struct {
    id: Id,

    pub const Id = enum {
        root,
        resource_external,
        resource_raw_data,
        literal,
        binary_expression,
        grouped_expression,
        accelerators,
        accelerator,
        dialog,
        string_table,
        string_table_string,
        language_statement,
        font_statement,
        simple_statement,
        invalid,

        pub fn Type(comptime id: Id) type {
            return switch (id) {
                .root => Root,
                .resource_external => ResourceExternal,
                .resource_raw_data => ResourceRawData,
                .literal => Literal,
                .binary_expression => BinaryExpression,
                .grouped_expression => GroupedExpression,
                .accelerators => Accelerators,
                .accelerator => Accelerator,
                .dialog => Dialog,
                .string_table => StringTable,
                .string_table_string => StringTableString,
                .language_statement => LanguageStatement,
                .font_statement => FontStatement,
                .simple_statement => SimpleStatement,
                .invalid => Invalid,
            };
        }
    };

    pub fn cast(base: *Node, comptime id: Id) ?*id.Type() {
        if (base.id == id) {
            return @fieldParentPtr(id.Type(), "base", base);
        }
        return null;
    }

    pub const Root = struct {
        base: Node = .{ .id = .root },
        body: []*Node,
    };

    pub const ResourceExternal = struct {
        base: Node = .{ .id = .resource_external },
        id: Token,
        type: Token,
        common_resource_attributes: []Token,
        filename: *Node,
    };

    pub const ResourceRawData = struct {
        base: Node = .{ .id = .resource_raw_data },
        id: Token,
        type: Token,
        common_resource_attributes: []Token,
        raw_data: []*Node,
    };

    pub const Literal = struct {
        base: Node = .{ .id = .literal },
        token: Token,
    };

    pub const BinaryExpression = struct {
        base: Node = .{ .id = .binary_expression },
        operator: Token,
        left: *Node,
        right: *Node,
    };

    pub const GroupedExpression = struct {
        base: Node = .{ .id = .grouped_expression },
        open_token: Token,
        expression: *Node,
        close_token: Token,
    };

    pub const Accelerators = struct {
        base: Node = .{ .id = .accelerators },
        id: Token,
        type: Token,
        common_resource_attributes: []Token,
        optional_statements: []*Node,
        begin_token: Token,
        accelerators: []*Node,
        end_token: Token,
    };

    pub const Accelerator = struct {
        base: Node = .{ .id = .accelerator },
        event: *Node,
        idvalue: *Node,
        type_and_options: []Token,
    };

    pub const Dialog = struct {
        base: Node = .{ .id = .dialog },
        id: Token,
        type: Token,
        common_resource_attributes: []Token,
        x: *Node,
        y: *Node,
        width: *Node,
        height: *Node,
        help_id: ?*Node,
        optional_statements: []*Node,
        begin_token: Token,
        controls: []*Node,
        end_token: Token,
    };

    pub const StringTable = struct {
        base: Node = .{ .id = .string_table },
        type: Token,
        common_resource_attributes: []Token,
        optional_statements: []*Node,
        begin_token: Token,
        strings: []*Node,
        end_token: Token,
    };

    pub const StringTableString = struct {
        base: Node = .{ .id = .string_table_string },
        id: *Node,
        maybe_comma: ?Token,
        string: Token,
    };

    pub const LanguageStatement = struct {
        base: Node = .{ .id = .language_statement },
        /// The LANGUAGE token itself
        language_token: Token,
        primary_language_id: *Node,
        sublanguage_id: *Node,
    };

    pub const FontStatement = struct {
        base: Node = .{ .id = .font_statement },
        /// The FONT token itself
        identifier: Token,
        point_size: *Node,
        typeface: Token,
        weight: ?*Node,
        italic: ?*Node,
        char_set: ?*Node,
    };

    /// A statement with one value associated with it.
    /// Used for CAPTION, CHARACTERISTICS, CLASS, EXSTYLE, MENU, STYLE, VERSION
    pub const SimpleStatement = struct {
        base: Node = .{ .id = .simple_statement },
        identifier: Token,
        value: *Node,
    };

    pub const Invalid = struct {
        base: Node = .{ .id = .invalid },
        context: []*Node,
    };

    pub fn isNumberExpression(node: *const Node) bool {
        switch (node.id) {
            .literal => {
                const literal = @fieldParentPtr(Node.Literal, "base", node);
                return switch (literal.token.id) {
                    .number => true,
                    else => false,
                };
            },
            .binary_expression, .grouped_expression => return true,
            else => return false,
        }
    }

    pub fn isStringLiteral(node: *const Node) bool {
        switch (node.id) {
            .literal => {
                const literal = @fieldParentPtr(Node.Literal, "base", node);
                return switch (literal.token.id) {
                    .quoted_ascii_string, .quoted_wide_string => true,
                    else => false,
                };
            },
            else => return false,
        }
    }

    /// Some "expressions" are special cases that need to be considered "valid" expressions
    /// but that don't contribute to anything and are evaluated as if they don't exist at all.
    /// Currently this is only the case for a single ')' as an expression.
    pub fn isExpressionAlwaysSkipped(node: *const Node) bool {
        switch (node.id) {
            .literal => {
                const literal = @fieldParentPtr(Node.Literal, "base", node);
                return switch (literal.token.id) {
                    .close_paren => true,
                    else => false,
                };
            },
            else => return false,
        }
    }

    pub fn getFirstToken(node: *const Node) Token {
        switch (node.id) {
            .root => unreachable,
            .resource_external => {
                const casted = @fieldParentPtr(Node.ResourceExternal, "base", node);
                return casted.id;
            },
            .resource_raw_data => {
                const casted = @fieldParentPtr(Node.ResourceRawData, "base", node);
                return casted.id;
            },
            .literal => {
                const casted = @fieldParentPtr(Node.Literal, "base", node);
                return casted.token;
            },
            .binary_expression => {
                const casted = @fieldParentPtr(Node.BinaryExpression, "base", node);
                return casted.left.getFirstToken();
            },
            .grouped_expression => {
                const casted = @fieldParentPtr(Node.GroupedExpression, "base", node);
                return casted.open_token;
            },
            .accelerators => {
                const casted = @fieldParentPtr(Node.Accelerators, "base", node);
                return casted.id;
            },
            .accelerator => {
                const casted = @fieldParentPtr(Node.Accelerator, "base", node);
                return casted.event.getFirstToken();
            },
            .dialog => {
                const casted = @fieldParentPtr(Node.Dialog, "base", node);
                return casted.id;
            },
            .string_table => {
                const casted = @fieldParentPtr(Node.StringTable, "base", node);
                return casted.type;
            },
            .string_table_string => {
                const casted = @fieldParentPtr(Node.StringTableString, "base", node);
                return casted.id.getFirstToken();
            },
            .language_statement => {
                const casted = @fieldParentPtr(Node.LanguageStatement, "base", node);
                return casted.language_token;
            },
            .font_statement => {
                const casted = @fieldParentPtr(Node.FontStatement, "base", node);
                return casted.identifier;
            },
            .simple_statement => {
                const casted = @fieldParentPtr(Node.SimpleStatement, "base", node);
                return casted.identifier;
            },
            .invalid => @panic("TODO getFirstToken for invalid node"),
        }
    }

    pub fn dump(
        node: *const Node,
        tree: *const Tree,
        writer: anytype,
        indent: usize,
    ) @TypeOf(writer).Error!void {
        try writer.writeByteNTimes(' ', indent);
        try writer.writeAll(@tagName(node.id));
        switch (node.id) {
            .root => {
                try writer.writeAll("\n");
                const root = @fieldParentPtr(Node.Root, "base", node);
                for (root.body) |body_node| {
                    try body_node.dump(tree, writer, indent + 1);
                }
            },
            .resource_external => {
                const resource = @fieldParentPtr(Node.ResourceExternal, "base", node);
                try writer.print(" {s} {s} [{d} common_resource_attributes]\n", .{ resource.id.slice(tree.source), resource.type.slice(tree.source), resource.common_resource_attributes.len });
                try resource.filename.dump(tree, writer, indent + 1);
            },
            .resource_raw_data => {
                const resource = @fieldParentPtr(Node.ResourceRawData, "base", node);
                try writer.print(" {s} {s} [{d} common_resource_attributes] raw data: {}\n", .{ resource.id.slice(tree.source), resource.type.slice(tree.source), resource.common_resource_attributes.len, resource.raw_data.len });
                for (resource.raw_data) |data_expression| {
                    try data_expression.dump(tree, writer, indent + 1);
                }
            },
            .literal => {
                const literal = @fieldParentPtr(Node.Literal, "base", node);
                try writer.writeAll(" ");
                try writer.writeAll(literal.token.slice(tree.source));
                try writer.writeAll("\n");
            },
            .binary_expression => {
                const binary = @fieldParentPtr(Node.BinaryExpression, "base", node);
                try writer.writeAll(" ");
                try writer.writeAll(binary.operator.slice(tree.source));
                try writer.writeAll("\n");
                try binary.left.dump(tree, writer, indent + 1);
                try binary.right.dump(tree, writer, indent + 1);
            },
            .grouped_expression => {
                const grouped = @fieldParentPtr(Node.GroupedExpression, "base", node);
                try writer.writeAll("\n");
                try writer.writeByteNTimes(' ', indent);
                try writer.writeAll(grouped.open_token.slice(tree.source));
                try writer.writeAll("\n");
                try grouped.expression.dump(tree, writer, indent + 1);
                try writer.writeByteNTimes(' ', indent);
                try writer.writeAll(grouped.close_token.slice(tree.source));
                try writer.writeAll("\n");
            },
            .accelerators => {
                const accelerators = @fieldParentPtr(Node.Accelerators, "base", node);
                try writer.print(" {s} {s} [{d} common_resource_attributes]\n", .{ accelerators.id.slice(tree.source), accelerators.type.slice(tree.source), accelerators.common_resource_attributes.len });
                for (accelerators.optional_statements) |statement| {
                    try statement.dump(tree, writer, indent + 1);
                }
                try writer.writeByteNTimes(' ', indent);
                try writer.writeAll(accelerators.begin_token.slice(tree.source));
                try writer.writeAll("\n");
                for (accelerators.accelerators) |accelerator| {
                    try accelerator.dump(tree, writer, indent + 1);
                }
                try writer.writeByteNTimes(' ', indent);
                try writer.writeAll(accelerators.end_token.slice(tree.source));
                try writer.writeAll("\n");
            },
            .accelerator => {
                const accelerator = @fieldParentPtr(Node.Accelerator, "base", node);
                for (accelerator.type_and_options) |option, i| {
                    if (i != 0) try writer.writeAll(",");
                    try writer.writeByte(' ');
                    try writer.writeAll(option.slice(tree.source));
                }
                try writer.writeAll("\n");
                try accelerator.event.dump(tree, writer, indent + 1);
                try accelerator.idvalue.dump(tree, writer, indent + 1);
            },
            .dialog => {
                const dialog = @fieldParentPtr(Node.Dialog, "base", node);
                try writer.print(" {s} {s} [{d} common_resource_attributes]\n", .{ dialog.id.slice(tree.source), dialog.type.slice(tree.source), dialog.common_resource_attributes.len });
                inline for (.{ "x", "y", "width", "height" }) |arg| {
                    try writer.writeByteNTimes(' ', indent + 1);
                    try writer.writeAll(arg ++ ":\n");
                    try @field(dialog, arg).dump(tree, writer, indent + 2);
                }
                if (dialog.help_id) |help_id| {
                    try writer.writeByteNTimes(' ', indent + 1);
                    try writer.writeAll("help_id:\n");
                    try help_id.dump(tree, writer, indent + 2);
                }
                for (dialog.optional_statements) |statement| {
                    try statement.dump(tree, writer, indent + 1);
                }
                try writer.writeByteNTimes(' ', indent);
                try writer.writeAll(dialog.begin_token.slice(tree.source));
                try writer.writeAll("\n");
                for (dialog.controls) |control| {
                    try control.dump(tree, writer, indent + 1);
                }
                try writer.writeByteNTimes(' ', indent);
                try writer.writeAll(dialog.end_token.slice(tree.source));
                try writer.writeAll("\n");
            },
            .string_table => {
                const string_table = @fieldParentPtr(Node.StringTable, "base", node);
                try writer.print(" {s} [{d} common_resource_attributes]\n", .{ string_table.type.slice(tree.source), string_table.common_resource_attributes.len });
                for (string_table.optional_statements) |statement| {
                    try statement.dump(tree, writer, indent + 1);
                }
                try writer.writeByteNTimes(' ', indent);
                try writer.writeAll(string_table.begin_token.slice(tree.source));
                try writer.writeAll("\n");
                for (string_table.strings) |string| {
                    try string.dump(tree, writer, indent + 1);
                }
                try writer.writeByteNTimes(' ', indent);
                try writer.writeAll(string_table.end_token.slice(tree.source));
                try writer.writeAll("\n");
            },
            .string_table_string => {
                try writer.writeAll("\n");
                const string = @fieldParentPtr(Node.StringTableString, "base", node);
                try string.id.dump(tree, writer, indent + 1);
                try writer.writeByteNTimes(' ', indent + 1);
                try writer.print("{s}\n", .{string.string.slice(tree.source)});
            },
            .language_statement => {
                const language = @fieldParentPtr(Node.LanguageStatement, "base", node);
                try writer.print(" {s}\n", .{language.language_token.slice(tree.source)});
                try language.primary_language_id.dump(tree, writer, indent + 1);
                try language.sublanguage_id.dump(tree, writer, indent + 1);
            },
            .font_statement => {
                const font = @fieldParentPtr(Node.FontStatement, "base", node);
                try writer.print(" {s} typeface: {s}\n", .{ font.identifier.slice(tree.source), font.typeface.slice(tree.source) });
                try writer.writeByteNTimes(' ', indent + 1);
                try writer.writeAll("point_size:\n");
                try font.point_size.dump(tree, writer, indent + 2);
                inline for (.{ "weight", "italic", "char_set" }) |arg| {
                    if (@field(font, arg)) |arg_node| {
                        try writer.writeByteNTimes(' ', indent + 1);
                        try writer.writeAll(arg ++ ":\n");
                        try arg_node.dump(tree, writer, indent + 2);
                    }
                }
            },
            .simple_statement => {
                const statement = @fieldParentPtr(Node.SimpleStatement, "base", node);
                try writer.print(" {s}\n", .{statement.identifier.slice(tree.source)});
                try statement.value.dump(tree, writer, indent + 1);
            },
            .invalid => {
                const invalid = @fieldParentPtr(Node.Invalid, "base", node);
                try writer.print(" context.len: {}\n", .{invalid.context.len});
                for (invalid.context) |context_node| {
                    try context_node.dump(tree, writer, indent + 1);
                }
            },
        }
    }
};
