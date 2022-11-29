const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("lex.zig").Token;

pub const Tree = struct {
    node: *Node,

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

pub const Node = struct {
    id: Id,

    pub const Id = enum {
        root,
        resource_external,
        resource_raw_data,
        literal,
        binary_expression,
        grouped_expression,
        string_table,
        string_table_string,
        language_statement,
        invalid,

        pub fn Type(comptime id: Id) type {
            return switch (id) {
                .root => Root,
                .resource_external => ResourceExternal,
                .resource_raw_data => ResourceRawData,
                .literal => Literal,
                .binary_expression => BinaryExpression,
                .grouped_expression => GroupedExpression,
                .string_table => StringTable,
                .string_table_string => StringTableString,
                .language_statement => LanguageStatement,
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

    pub const StringTable = struct {
        base: Node = .{ .id = .string_table },
        type: Token,
        common_resource_attributes: []Token,
        language: ?*Node.LanguageStatement,
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
            .string_table => {
                const string_table = @fieldParentPtr(Node.StringTable, "base", node);
                try writer.print(" {s} [{d} common_resource_attributes]\n", .{ string_table.type.slice(tree.source), string_table.common_resource_attributes.len });
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
