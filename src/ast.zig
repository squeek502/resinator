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

        pub fn Type(id: Id) type {
            return switch (id) {
                .root => Root,
                .resource_external => ResourceExternal,
                .resource_raw_data => ResourceRawData,
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
        filename: Token,
    };

    pub const ResourceRawData = struct {
        base: Node = .{ .id = .resource_raw_data },
        id: Token,
        type: Token,
        common_resource_attributes: []Token,
        raw_data: []Token,
    };

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
                try writer.print(" {s} {s} [{d} common_resource_attributes] {s}\n", .{ resource.id.slice(tree.source), resource.type.slice(tree.source), resource.common_resource_attributes.len, resource.filename.slice(tree.source) });
            },
            .resource_raw_data => {
                const resource = @fieldParentPtr(Node.ResourceRawData, "base", node);
                try writer.print(" {s} {s} [{d} common_resource_attributes] raw data: {}\n", .{ resource.id.slice(tree.source), resource.type.slice(tree.source), resource.common_resource_attributes.len, resource.raw_data.len });
                for (resource.raw_data) |data_token| {
                    try writer.writeByteNTimes(' ', indent + 1);
                    try writer.print("{s}\n", .{data_token.slice(tree.source)});
                }
            },
        }
    }
};
