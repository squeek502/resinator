const std = @import("std");
const Lexer = @import("lex.zig").Lexer;
const Token = @import("lex.zig").Token;
const Node = @import("ast.zig").Node;
const Tree = @import("ast.zig").Tree;
const Resource = @import("rc.zig").Resource;
const Allocator = std.mem.Allocator;
const ErrorDetails = @import("errors.zig").ErrorDetails;
const Diagnostics = @import("errors.zig").Diagnostics;

pub const Parser = struct {
    const Self = @This();

    lexer: *Lexer,
    /// values that need to be initialized per-parse
    state: Parser.State = undefined,

    pub const Error = error{ParseError} || Allocator.Error;

    pub fn init(lexer: *Lexer) Parser {
        return Parser{
            .lexer = lexer,
        };
    }

    pub const State = struct {
        token: Token,
        allocator: Allocator,
        arena: Allocator,
        diagnostics: *Diagnostics,
    };

    pub fn parse(self: *Self, allocator: Allocator, diagnostics: *Diagnostics) Error!*Tree {
        var arena = std.heap.ArenaAllocator.init(allocator);
        errdefer arena.deinit();

        self.state = Parser.State{
            .token = undefined,
            .allocator = allocator,
            .arena = arena.allocator(),
            .diagnostics = diagnostics,
        };

        try self.nextToken(.whitespace_delimiter_only);
        const parsed_root = try self.parseRoot();

        const tree = try self.state.arena.create(Tree);
        tree.* = .{
            .node = parsed_root,
            .source = self.lexer.buffer,
            .arena = arena.state,
            .allocator = allocator,
        };
        return tree;
    }

    fn parseRoot(self: *Self) Error!*Node {
        var statements = std.ArrayList(*Node).init(self.state.allocator);
        defer statements.deinit();

        try self.parseStatements(&statements);
        try self.check(.eof);

        const node = try self.state.arena.create(Node.Root);
        node.* = .{
            .body = try self.state.arena.dupe(*Node, statements.items),
        };
        return &node.base;
    }

    fn parseStatements(self: *Self, statements: *std.ArrayList(*Node)) Error!void {
        while (self.state.token.id != .eof) {
            // TODO: Catch something like an 'invalid resource' error and
            //       append an Invalid node instead or something like that.
            //       This kind of seems to be how the Windows RC compiler works,
            //       as if it hits an invalid token it just kinda resets and
            //       starts parsing from scratch again with the next token.
            var statement = try self.parseStatement();
            try statements.append(statement);
        }
    }

    fn parseStatement(self: *Self) Error!*Node {
        const first_token = self.state.token;
        if (first_token.id == .literal and std.mem.eql(u8, first_token.slice(self.lexer.buffer), "LANGUAGE")) {
            return self.parseLanguageStatement();
        } else if (first_token.id == .literal and std.mem.eql(u8, first_token.slice(self.lexer.buffer), "STRINGTABLE")) {
            try self.nextToken(.normal);

            var common_resource_attributes = std.ArrayList(Token).init(self.state.allocator);
            defer common_resource_attributes.deinit();

            while (self.state.token.id == .literal and common_resource_attributes_set.has(self.state.token.slice(self.lexer.buffer))) {
                try common_resource_attributes.append(self.state.token);
                try self.nextToken(.normal);
            }

            const begin_token = self.state.token;
            try self.checkBegin();
            try self.nextToken(.normal);

            var strings = std.ArrayList(*Node).init(self.state.allocator);
            defer strings.deinit();
            while (true) {
                switch (self.state.token.id) {
                    .literal => {
                        // TODO: Handle this differently, might be nice to fold this into .close_brace
                        if (std.mem.eql(u8, "END", self.tokenSlice())) {
                            break;
                        }
                    },
                    .close_brace => {
                        break;
                    },
                    .eof => {
                        return self.failDetails(ErrorDetails{
                            .err = .unfinished_string_table_block,
                            .token = self.state.token,
                        });
                    },
                    else => {},
                }
                const id_expression_result = try self.parseExpression(false);
                if (id_expression_result.node.id == .literal and id_expression_result.node.cast(.literal).?.token.id != .number) {
                    return self.failDetails(ErrorDetails{
                        .err = .expected_token, // TODO: maybe a more specific error
                        .token = id_expression_result.node.cast(.literal).?.token,
                        .extra = .{ .expected = .number },
                    });
                }

                if (!id_expression_result.has_unconsumed_token) {
                    try self.nextToken(.normal);
                }

                const comma_token: ?Token = comma: {
                    if (self.state.token.id == .comma) {
                        const token = self.state.token;
                        try self.nextToken(.normal);
                        break :comma token;
                    } else {
                        break :comma null;
                    }
                };

                if (self.state.token.id != .quoted_ascii_string and self.state.token.id != .quoted_wide_string) {
                    return self.failDetails(ErrorDetails{
                        .err = .expected_token, // TODO: probably a more specific error message
                        .token = self.state.token,
                        .extra = .{ .expected = .quoted_ascii_string },
                    });
                }

                const string_node = try self.state.arena.create(Node.StringTableString);
                string_node.* = .{
                    .id = id_expression_result.node,
                    .maybe_comma = comma_token,
                    .string = self.state.token,
                };
                try strings.append(&string_node.base);

                try self.nextToken(.normal);
            }

            if (strings.items.len == 0) {
                return self.failDetails(ErrorDetails{
                    .err = .expected_token, // TODO: probably a more specific error message
                    .token = self.state.token,
                    .extra = .{ .expected = .number },
                });
            }

            const end_token = self.state.token;
            try self.checkEnd();
            try self.nextToken(.whitespace_delimiter_only);

            const node = try self.state.arena.create(Node.StringTable);
            node.* = .{
                .type = first_token,
                .common_resource_attributes = try self.state.arena.dupe(Token, common_resource_attributes.items),
                .language = null, // TODO
                .begin_token = begin_token,
                .strings = try self.state.arena.dupe(*Node, strings.items),
                .end_token = end_token,
            };
            return &node.base;
        } else {
            try self.checkId();
            const id_token = first_token;
            try self.nextToken(.whitespace_delimiter_only);
            const resource = try self.checkResource();
            const type_token = self.state.token;
            try self.nextToken(.normal);

            switch (resource) {
                .icon, .font, .cursor, .bitmap, .messagetable, .user_defined, .rcdata, .html => {
                    var common_resource_attributes = std.ArrayList(Token).init(self.state.allocator);
                    defer common_resource_attributes.deinit();

                    while (self.state.token.id == .literal and common_resource_attributes_set.has(self.state.token.slice(self.lexer.buffer))) {
                        try common_resource_attributes.append(self.state.token);
                        try self.nextToken(.normal);
                    }

                    if (try self.testBegin()) {
                        try self.nextToken(.normal);

                        var raw_data = std.ArrayList(*Node).init(self.state.allocator);
                        defer raw_data.deinit();
                        while (true) {
                            switch (self.state.token.id) {
                                .literal => {
                                    // TODO: Handle this differently, might be nice to fold this into .close_brace
                                    if (std.mem.eql(u8, "END", self.tokenSlice())) {
                                        break;
                                    }
                                },
                                .comma => {
                                    // comma as the first token in a raw data block is an error
                                    if (raw_data.items.len == 0) {
                                        return self.failDetails(ErrorDetails{
                                            .err = .expected_something_else,
                                            .token = self.state.token,
                                            .extra = .{ .expected_types = .{
                                                .number = true,
                                                .number_expression = true,
                                                .string_literal = true,
                                            } },
                                        });
                                    }
                                    // otherwise just skip over commas
                                    try self.nextToken(.normal);
                                    continue;
                                },
                                .close_brace => {
                                    try self.nextToken(.whitespace_delimiter_only);
                                    break;
                                },
                                .eof => {
                                    return self.failDetails(ErrorDetails{
                                        .err = .unfinished_raw_data_block,
                                        .token = self.state.token,
                                    });
                                },
                                else => {},
                            }
                            const expression_result = try self.parseExpression(false);
                            try raw_data.append(expression_result.node);

                            if (!expression_result.node.isExpressionAlwaysSkipped() and !expression_result.node.isNumberExpression() and !expression_result.node.isStringLiteral()) {
                                return self.failDetails(ErrorDetails{
                                    .err = .expected_something_else,
                                    .token = expression_result.node.getFirstToken(),
                                    .extra = .{ .expected_types = .{
                                        .number = true,
                                        .number_expression = true,
                                        .string_literal = true,
                                    } },
                                });
                            }

                            if (expression_result.node.isNumberExpression() and expression_result.has_unconsumed_token and self.state.token.id == .close_paren) {
                                // <number expression>) is an error
                                return self.failDetails(ErrorDetails{
                                    .err = .expected_token,
                                    .token = self.state.token,
                                    .extra = .{ .expected = .operator },
                                });
                            }

                            if (!expression_result.has_unconsumed_token) {
                                try self.nextToken(.normal);
                            }
                        }

                        const node = try self.state.arena.create(Node.ResourceRawData);
                        node.* = .{
                            .id = id_token,
                            .type = type_token,
                            .common_resource_attributes = try self.state.arena.dupe(Token, common_resource_attributes.items),
                            .raw_data = try self.state.arena.dupe(*Node, raw_data.items),
                        };
                        return &node.base;
                    }

                    var filename_expression_result = try self.parseExpression(false);
                    if (!filename_expression_result.has_unconsumed_token) {
                        try self.nextToken(.whitespace_delimiter_only);
                    }

                    const node = try self.state.arena.create(Node.ResourceExternal);
                    node.* = .{
                        .id = id_token,
                        .type = type_token,
                        .common_resource_attributes = try self.state.arena.dupe(Token, common_resource_attributes.items),
                        .filename = filename_expression_result.node,
                    };
                    return &node.base;
                },
                .stringtable => unreachable,
                else => @panic("TODO unhandled resource type"),
            }
        }
    }

    fn parseLanguageStatement(self: *Self) Error!*Node {
        const language_token = self.state.token;
        try self.nextToken(.normal);

        const primary_language_result = try self.parseExpression(false);
        if (!primary_language_result.node.isNumberExpression()) {
            return self.failDetails(ErrorDetails{
                .err = .expected_something_else,
                .token = primary_language_result.node.getFirstToken(),
                .extra = .{ .expected_types = .{
                    .number = true,
                    .number_expression = true,
                } },
            });
        }
        if (!primary_language_result.has_unconsumed_token) {
            try self.nextToken(.normal);
        }

        try self.check(.comma);
        try self.nextToken(.normal);

        const sublanguage_result = try self.parseExpression(false);
        if (!sublanguage_result.node.isNumberExpression()) {
            return self.failDetails(ErrorDetails{
                .err = .expected_something_else,
                .token = sublanguage_result.node.getFirstToken(),
                .extra = .{ .expected_types = .{
                    .number = true,
                    .number_expression = true,
                } },
            });
        }
        if (!sublanguage_result.has_unconsumed_token) {
            // TODO: This is not always the right method to parse the next token
            try self.nextToken(.normal);
        }

        const node = try self.state.arena.create(Node.LanguageStatement);
        node.* = .{
            .language_token = language_token,
            .primary_language_id = primary_language_result.node,
            .sublanguage_id = sublanguage_result.node,
        };
        return &node.base;
    }

    const ExpressionResult = struct {
        node: *Node,
        has_unconsumed_token: bool = false,
    };

    fn parseExpression(self: *Self, is_known_to_be_number_expression: bool) Error!ExpressionResult {
        const possible_lhs: *Node = lhs: {
            switch (self.state.token.id) {
                .quoted_ascii_string, .quoted_wide_string => {
                    const node = try self.state.arena.create(Node.Literal);
                    node.* = .{
                        .token = self.state.token,
                    };
                    return .{ .node = &node.base };
                },
                .literal => {
                    const node = try self.state.arena.create(Node.Literal);
                    node.* = .{
                        .token = self.state.token,
                    };
                    return .{ .node = &node.base };
                },
                .number => {
                    const node = try self.state.arena.create(Node.Literal);
                    node.* = .{
                        .token = self.state.token,
                    };
                    break :lhs &node.base;
                },
                .open_paren => {
                    const open_paren_token = self.state.token;

                    try self.nextToken(.normal);
                    const expression_result = try self.parseExpression(true);

                    if (!expression_result.node.isNumberExpression()) {
                        return self.failDetails(ErrorDetails{
                            .err = .expected_something_else,
                            .token = expression_result.node.getFirstToken(),
                            .extra = .{ .expected_types = .{
                                .number = true,
                                .number_expression = true,
                            } },
                        });
                    }

                    if (!expression_result.has_unconsumed_token) {
                        try self.nextToken(.normal);
                    }

                    const node = try self.state.arena.create(Node.GroupedExpression);
                    node.* = .{
                        .open_token = open_paren_token,
                        .expression = expression_result.node,
                        .close_token = self.state.token,
                    };

                    // TODO: Add context to error about where the open paren is
                    try self.check(.close_paren);

                    break :lhs &node.base;
                },
                .close_paren => {
                    // A single close paren counts as a valid "expression", but
                    // only when its the first and only token in the expression.
                    // Very strange.
                    if (!is_known_to_be_number_expression) {
                        const node = try self.state.arena.create(Node.Literal);
                        node.* = .{
                            .token = self.state.token,
                        };
                        return .{ .node = &node.base };
                    }
                },
                else => {},
            }

            // TODO: This may not be the correct way to handle this in all cases?
            return self.failDetails(ErrorDetails{
                .err = .expected_something_else,
                .token = self.state.token,
                .extra = .{ .expected_types = .{
                    .number = true,
                    .number_expression = true,
                    .string_literal = !is_known_to_be_number_expression,
                } },
            });
        };

        try self.nextToken(.normal_expect_operator);
        const possible_operator = self.state.token;
        switch (possible_operator.id) {
            .operator => {},
            else => {
                return .{
                    .node = possible_lhs,
                    .has_unconsumed_token = true,
                };
            },
        }

        try self.nextToken(.normal);
        const rhs_result = try self.parseExpression(true);

        if (!rhs_result.node.isNumberExpression()) {
            return self.failDetails(ErrorDetails{
                .err = .expected_something_else,
                .token = rhs_result.node.getFirstToken(),
                .extra = .{ .expected_types = .{
                    .number = true,
                    .number_expression = true,
                } },
            });
        }

        const node = try self.state.arena.create(Node.BinaryExpression);
        node.* = .{
            .left = possible_lhs,
            .operator = possible_operator,
            .right = rhs_result.node,
        };

        return .{
            .node = &node.base,
            .has_unconsumed_token = rhs_result.has_unconsumed_token,
        };
    }

    fn warnDetails(self: *Self, details: ErrorDetails) Allocator.Error!void {
        try self.state.diagnostics.append(details);
    }

    fn failDetails(self: *Self, details: ErrorDetails) Error {
        try self.warnDetails(details);
        return error.ParseError;
    }

    fn nextToken(self: *Self, comptime method: Lexer.LexMethod) Error!void {
        self.state.token = self.lexer.next(method) catch |err| {
            return self.failDetails(self.lexer.getErrorDetails(err));
        };
    }

    fn tokenSlice(self: *Self) []const u8 {
        return self.state.token.slice(self.lexer.buffer);
    }

    /// Check that the current token is something that can be used as an ID
    fn checkId(self: *Self) !void {
        switch (self.state.token.id) {
            .literal => {},
            else => {
                return self.failDetails(ErrorDetails{
                    .err = .expected_token,
                    .token = self.state.token,
                    .extra = .{ .expected = .literal },
                });
            },
        }
    }

    fn testBegin(self: *Self) !bool {
        switch (self.state.token.id) {
            .open_brace => {
                return true;
            },
            .literal => {
                if (std.mem.eql(u8, "BEGIN", self.state.token.slice(self.lexer.buffer))) {
                    return true;
                }
                return false;
            },
            else => return false,
        }
    }

    fn checkBegin(self: *Self) !void {
        if (!(try self.testBegin())) {
            return self.failDetails(ErrorDetails{
                .err = .expected_token,
                .token = self.state.token,
                .extra = .{ .expected = .open_brace },
            });
        }
    }

    fn testEnd(self: *Self) !bool {
        switch (self.state.token.id) {
            .close_brace => {
                return true;
            },
            .literal => {
                if (std.mem.eql(u8, "END", self.state.token.slice(self.lexer.buffer))) {
                    return true;
                }
                return false;
            },
            else => return false,
        }
    }

    fn checkEnd(self: *Self) !void {
        if (!(try self.testEnd())) {
            return self.failDetails(ErrorDetails{
                .err = .expected_token,
                .token = self.state.token,
                .extra = .{ .expected = .close_brace },
            });
        }
    }

    fn check(self: *Self, expected_token_id: Token.Id) !void {
        if (self.state.token.id != expected_token_id) {
            return self.failDetails(ErrorDetails{
                .err = .expected_token,
                .token = self.state.token,
                .extra = .{ .expected = expected_token_id },
            });
        }
    }

    fn checkResource(self: *Self) !Resource {
        switch (self.state.token.id) {
            .literal => return Resource.fromString(self.state.token.slice(self.lexer.buffer)),
            else => {
                return self.failDetails(ErrorDetails{
                    .err = .expected_token,
                    .token = self.state.token,
                    .extra = .{ .expected = .literal },
                });
            },
        }
    }
};

const common_resource_attributes_set = std.ComptimeStringMap(void, .{
    .{"PRELOAD"},
    .{"LOADONCALL"},
    .{"FIXED"},
    .{"MOVEABLE"},
    .{"DISCARDABLE"},
    .{"PURE"},
    .{"IMPURE"},
    .{"SHARED"},
    .{"NONSHARED"},
});

fn testParse(source: []const u8, expected_ast_dump: []const u8) !void {
    const allocator = std.testing.allocator;
    var diagnostics = Diagnostics.init(allocator);
    defer diagnostics.deinit();
    var lexer = Lexer.init(source);
    var parser = Parser.init(&lexer);
    var tree = parser.parse(allocator, &diagnostics) catch |err| switch (err) {
        error.ParseError => {
            diagnostics.renderToStdErr(std.fs.cwd(), source, null);
            return err;
        },
        else => |e| return e,
    };
    defer tree.deinit();

    var buf = std.ArrayList(u8).init(allocator);
    defer buf.deinit();

    try tree.dump(buf.writer());
    try std.testing.expectEqualStrings(expected_ast_dump, buf.items);
}

test "basic icons" {
    try testParse("id ICON MOVEABLE filename.ico",
        \\root
        \\ resource_external id ICON [1 common_resource_attributes]
        \\  literal filename.ico
        \\
    );
    try testParse(
        \\id1 ICON MOVEABLE filename.ico
        \\id2 ICON filename.ico
    ,
        \\root
        \\ resource_external id1 ICON [1 common_resource_attributes]
        \\  literal filename.ico
        \\ resource_external id2 ICON [0 common_resource_attributes]
        \\  literal filename.ico
        \\
    );
    try testParse(
        \\id1 ICON MOVEABLE filename.ico id2 ICON filename.ico
    ,
        \\root
        \\ resource_external id1 ICON [1 common_resource_attributes]
        \\  literal filename.ico
        \\ resource_external id2 ICON [0 common_resource_attributes]
        \\  literal filename.ico
        \\
    );
    try testParse(
        \\"id1" ICON "filename.ico"
        \\L"id2" ICON L"filename.ico"
    ,
        \\root
        \\ resource_external "id1" ICON [0 common_resource_attributes]
        \\  literal "filename.ico"
        \\ resource_external L"id2" ICON [0 common_resource_attributes]
        \\  literal L"filename.ico"
        \\
    );
}

test "user-defined" {
    try testParse("id \"quoted\" file.bin",
        \\root
        \\ resource_external id "quoted" [0 common_resource_attributes]
        \\  literal file.bin
        \\
    );
}

test "raw data" {
    try testParse("id RCDATA {}",
        \\root
        \\ resource_raw_data id RCDATA [0 common_resource_attributes] raw data: 0
        \\
    );
    try testParse("id RCDATA { 1,2,3 }",
        \\root
        \\ resource_raw_data id RCDATA [0 common_resource_attributes] raw data: 3
        \\  literal 1
        \\  literal 2
        \\  literal 3
        \\
    );
    try testParse("id RCDATA { L\"1\",\"2\",3 }",
        \\root
        \\ resource_raw_data id RCDATA [0 common_resource_attributes] raw data: 3
        \\  literal L"1"
        \\  literal "2"
        \\  literal 3
        \\
    );
    try testParse("id RCDATA { 1\t,,  ,,,2,,  ,  3 ,,,  , }",
        \\root
        \\ resource_raw_data id RCDATA [0 common_resource_attributes] raw data: 3
        \\  literal 1
        \\  literal 2
        \\  literal 3
        \\
    );
    try testParse("id RCDATA { 1 2 3 }",
        \\root
        \\ resource_raw_data id RCDATA [0 common_resource_attributes] raw data: 3
        \\  literal 1
        \\  literal 2
        \\  literal 3
        \\
    );
}

test "number expressions" {
    try testParse("id RCDATA { 1-- }",
        \\root
        \\ resource_raw_data id RCDATA [0 common_resource_attributes] raw data: 1
        \\  binary_expression -
        \\   literal 1
        \\   literal -
        \\
    );
    try testParse("id RCDATA { (1) }",
        \\root
        \\ resource_raw_data id RCDATA [0 common_resource_attributes] raw data: 1
        \\  grouped_expression
        \\  (
        \\   literal 1
        \\  )
        \\
    );
    try testParse("id RCDATA { (1+-1) }",
        \\root
        \\ resource_raw_data id RCDATA [0 common_resource_attributes] raw data: 1
        \\  grouped_expression
        \\  (
        \\   binary_expression +
        \\    literal 1
        \\    literal -1
        \\  )
        \\
    );
}

test "STRINGTABLE" {
    try testParse("STRINGTABLE { 0 \"hello\" }",
        \\root
        \\ string_table STRINGTABLE [0 common_resource_attributes]
        \\ {
        \\  string_table_string
        \\   literal 0
        \\   "hello"
        \\ }
        \\
    );
    try testParse("STRINGTABLE { 0, \"hello\" }",
        \\root
        \\ string_table STRINGTABLE [0 common_resource_attributes]
        \\ {
        \\  string_table_string
        \\   literal 0
        \\   "hello"
        \\ }
        \\
    );
    try testParse(
        \\STRINGTABLE {
        \\  (0+1), "hello"
        \\  -1, L"hello"
        \\}
    ,
        \\root
        \\ string_table STRINGTABLE [0 common_resource_attributes]
        \\ {
        \\  string_table_string
        \\   grouped_expression
        \\   (
        \\    binary_expression +
        \\     literal 0
        \\     literal 1
        \\   )
        \\   "hello"
        \\  string_table_string
        \\   literal -1
        \\   L"hello"
        \\ }
        \\
    );

    try testParse("STRINGTABLE FIXED { 0 \"hello\" }",
        \\root
        \\ string_table STRINGTABLE [1 common_resource_attributes]
        \\ {
        \\  string_table_string
        \\   literal 0
        \\   "hello"
        \\ }
        \\
    );

    // TODO: optional-statements on STRINGTABLE
    // try testParse("STRINGTABLE VERSION 1 { 0 \"hello\" }");
}

test "control characters as whitespace" {
    // any non-illegal control character is treated as whitespace
    try testParse("id RCDATA { 1\x052 }",
        \\root
        \\ resource_raw_data id RCDATA [0 common_resource_attributes] raw data: 2
        \\  literal 1
        \\  literal 2
        \\
    );
    // some illegal control characters are legal inside of string literals
    try testParse("id RCDATA { \"\x01\" }",
        \\root
        \\ resource_raw_data id RCDATA [0 common_resource_attributes] raw data: 1
        \\
    ++ "  literal \"\x01\"\n"); // needed to get the actual byte \x01 in the expected output
}

test "top-level language statement" {
    try testParse("LANGUAGE 0, 0",
        \\root
        \\ language_statement LANGUAGE
        \\  literal 0
        \\  literal 0
        \\
    );
}

test "parse errors" {
    try testParseError("unfinished raw data block at '<eof>', expected closing '}' or 'END'", "id RCDATA { 1");
    try testParseError("unfinished string literal at '<eof>', expected closing '\"'", "id RCDATA \"unfinished string");
    try testParseError("expected '<literal>', got '<eof>'", "id");
    try testParseError("expected ')', got '}'", "id RCDATA { (1 }");
    try testParseError("character '\\x1A' is not allowed", "id RCDATA { \"\x1A\" }");
    try testParseError("character '\\x01' is not allowed outside of string literals", "id RCDATA { \x01 }");
    try testParseError("escaping quotes with \\\" is not allowed (use \"\" instead)", "id RCDATA { \"\\\"\"\" }");
}

fn testParseError(expected_error_str: []const u8, source: []const u8) !void {
    const allocator = std.testing.allocator;
    var diagnostics = Diagnostics.init(allocator);
    defer diagnostics.deinit();
    var lexer = Lexer.init(source);
    var parser = Parser.init(&lexer);
    var tree = parser.parse(allocator, &diagnostics) catch |err| switch (err) {
        error.OutOfMemory => |e| return e,
        error.ParseError => {
            if (diagnostics.errors.items.len < 1) return error.NoDiagnostics;
            if (diagnostics.errors.items.len > 1) @panic("TODO handle parse test with multiple errors");
            var buf: [256]u8 = undefined;
            var fbs = std.io.fixedBufferStream(&buf);
            try diagnostics.errors.items[0].render(fbs.writer(), source);
            try std.testing.expectEqualStrings(expected_error_str, fbs.getWritten());
            return;
        },
    };
    std.debug.print("expected parse error, got tree:\n", .{});
    try tree.dump(std.io.getStdErr().writer());
    return error.UnexpectedSuccess;
}
