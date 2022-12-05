const std = @import("std");
const Lexer = @import("lex.zig").Lexer;
const Token = @import("lex.zig").Token;
const Node = @import("ast.zig").Node;
const Tree = @import("ast.zig").Tree;
const Resource = @import("rc.zig").Resource;
const Allocator = std.mem.Allocator;
const ErrorDetails = @import("errors.zig").ErrorDetails;
const Diagnostics = @import("errors.zig").Diagnostics;
const rc = @import("rc.zig");
const res = @import("res.zig");

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
        lookahead_lexer: Lexer,
        allocator: Allocator,
        arena: Allocator,
        diagnostics: *Diagnostics,
    };

    pub fn parse(self: *Self, allocator: Allocator, diagnostics: *Diagnostics) Error!*Tree {
        var arena = std.heap.ArenaAllocator.init(allocator);
        errdefer arena.deinit();

        self.state = Parser.State{
            .token = undefined,
            .lookahead_lexer = undefined,
            .allocator = allocator,
            .arena = arena.allocator(),
            .diagnostics = diagnostics,
        };

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
        while (true) {
            try self.nextToken(.whitespace_delimiter_only);
            if (self.state.token.id == .eof) break;
            // TODO: Catch something like an 'invalid resource' error and
            //       append an Invalid node instead or something like that.
            //       This kind of seems to be how the Windows RC compiler works,
            //       as if it hits an invalid token it just kinda resets and
            //       starts parsing from scratch again with the next token.
            var statement = try self.parseStatement();
            try statements.append(statement);
        }
    }

    /// Expects the current token to be a possible common resource attribute.
    /// After return, the current token will be the first token after all common resource attributes.
    /// The returned slice is allocated by the parser's arena
    fn parseCommonResourceAttributes(self: *Self) ![]Token {
        var common_resource_attributes = std.ArrayListUnmanaged(Token){};
        while (self.state.token.id == .literal and rc.CommonResourceAttributes.map.has(self.state.token.slice(self.lexer.buffer))) {
            try common_resource_attributes.append(self.state.arena, self.state.token);
            try self.nextToken(.normal);
        }
        return common_resource_attributes.toOwnedSlice(self.state.arena);
    }

    /// Like `parseCommonResourceAttributes`, but expects the current token
    /// to be the token before possible common resource attributes.
    /// After return, the current token will be the token immediately before the end of the
    /// common resource attributes (if any). If there are no common resource attributes, the
    /// current token is unchanged.
    /// The returned slice is allocated by the parser's arena
    fn parseCommonResourceAttributesLookahead(self: *Self) ![]Token {
        var common_resource_attributes = std.ArrayListUnmanaged(Token){};
        while (true) {
            const maybe_common_resource_attribute = try self.lookaheadToken(.normal);
            if (maybe_common_resource_attribute.id == .literal and rc.CommonResourceAttributes.map.has(maybe_common_resource_attribute.slice(self.lexer.buffer))) {
                try common_resource_attributes.append(self.state.arena, maybe_common_resource_attribute);
                self.nextToken(.normal) catch unreachable;
            } else {
                break;
            }
        }
        return common_resource_attributes.toOwnedSlice(self.state.arena);
    }

    /// Expects the current token to be a possible optional statement keyword.
    /// After return, the current token will be the first token after all optional statements.
    /// The returned slice is allocated by the parser's arena
    fn parseOptionalStatements(self: *Self) ![]*Node {
        var optional_statements = std.ArrayListUnmanaged(*Node){};
        while (self.state.token.id == .literal) {
            const slice = self.state.token.slice(self.lexer.buffer);
            const optional_statement_type = rc.OptionalStatements.map.get(slice) orelse break;
            switch (optional_statement_type) {
                .language => {
                    const language = try self.parseLanguageStatement();
                    try optional_statements.append(self.state.arena, language);
                    try self.nextToken(.normal);
                },
                .version, .characteristics => {
                    const identifier = self.state.token;
                    const value = try self.parseExpression(false);
                    if (!value.isNumberExpression()) {
                        return self.addErrorDetailsAndFail(ErrorDetails{
                            .err = .expected_something_else,
                            .token = value.getFirstToken(),
                            .extra = .{ .expected_types = .{
                                .number = true,
                                .number_expression = true,
                            } },
                        });
                    }
                    const node = try self.state.arena.create(Node.SimpleStatement);
                    node.* = .{
                        .identifier = identifier,
                        .value = value,
                    };
                    try optional_statements.append(self.state.arena, &node.base);
                    try self.nextToken(.normal);
                },
            }
        }
        return optional_statements.toOwnedSlice(self.state.arena);
    }

    /// Expects the current token to be the first token of the statement.
    fn parseStatement(self: *Self) Error!*Node {
        const first_token = self.state.token;
        // TODO: Is this actually guaranteed? Should it be?
        std.debug.assert(first_token.id == .literal);

        if (rc.TopLevelKeywords.map.get(first_token.slice(self.lexer.buffer))) |keyword| switch (keyword) {
            .language => {
                const language_statement = try self.parseLanguageStatement();
                return language_statement;
            },
            .stringtable => {
                try self.nextToken(.normal);
                // common resource attributes must all be contiguous and come before optional-statements
                const common_resource_attributes = try self.parseCommonResourceAttributes();
                const optional_statements = try self.parseOptionalStatements();

                const begin_token = self.state.token;
                try self.check(.begin);

                var strings = std.ArrayList(*Node).init(self.state.allocator);
                defer strings.deinit();
                while (true) {
                    const maybe_end_token = try self.lookaheadToken(.normal);
                    switch (maybe_end_token.id) {
                        .end => {
                            self.nextToken(.normal) catch unreachable;
                            break;
                        },
                        .eof => {
                            return self.addErrorDetailsAndFail(ErrorDetails{
                                .err = .unfinished_string_table_block,
                                .token = maybe_end_token,
                            });
                        },
                        else => {},
                    }
                    const id_expression = try self.parseExpression(false);
                    if (!id_expression.isNumberExpression()) {
                        return self.addErrorDetailsAndFail(ErrorDetails{
                            .err = .expected_something_else,
                            .token = id_expression.getFirstToken(),
                            .extra = .{ .expected_types = .{
                                .number = true,
                                .number_expression = true,
                            } },
                        });
                    }

                    try self.nextToken(.normal);
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
                        return self.addErrorDetailsAndFail(ErrorDetails{
                            .err = .expected_something_else,
                            .token = self.state.token,
                            .extra = .{ .expected_types = .{ .string_literal = true } },
                        });
                    }

                    const string_node = try self.state.arena.create(Node.StringTableString);
                    string_node.* = .{
                        .id = id_expression,
                        .maybe_comma = comma_token,
                        .string = self.state.token,
                    };
                    try strings.append(&string_node.base);
                }

                if (strings.items.len == 0) {
                    return self.addErrorDetailsAndFail(ErrorDetails{
                        .err = .expected_token, // TODO: probably a more specific error message
                        .token = self.state.token,
                        .extra = .{ .expected = .number },
                    });
                }

                const end_token = self.state.token;
                try self.check(.end);

                const node = try self.state.arena.create(Node.StringTable);
                node.* = .{
                    .type = first_token,
                    .common_resource_attributes = common_resource_attributes,
                    .optional_statements = optional_statements,
                    .begin_token = begin_token,
                    .strings = try self.state.arena.dupe(*Node, strings.items),
                    .end_token = end_token,
                };
                return &node.base;
            },
        };

        const id_token = first_token;
        try self.nextToken(.whitespace_delimiter_only);
        const resource = try self.checkResource();
        const type_token = self.state.token;

        if (resource == .string_num) {
            try self.addErrorDetails(.{
                .err = .string_resource_as_numeric_type,
                .token = type_token,
            });
            return self.addErrorDetailsAndFail(.{
                .err = .string_resource_as_numeric_type,
                .token = type_token,
                .type = .note,
                .print_source_line = false,
            });
        }

        if (resource == .font) {
            const maybe_ordinal = res.NameOrOrdinal.maybeOrdinalFromString(id_token.slice(self.lexer.buffer), true);
            if (maybe_ordinal == null) {
                return self.addErrorDetailsAndFail(ErrorDetails{
                    .err = .id_must_be_ordinal,
                    .token = id_token,
                    .extra = .{ .resource = resource },
                });
            }
        }

        switch (resource) {
            .accelerators => {
                try self.nextToken(.normal);
                // common resource attributes must all be contiguous and come before optional-statements
                const common_resource_attributes = try self.parseCommonResourceAttributes();
                const optional_statements = try self.parseOptionalStatements();

                const begin_token = self.state.token;
                try self.check(.begin);

                var accelerators = std.ArrayListUnmanaged(*Node){};

                while (true) {
                    const lookahead = try self.lookaheadToken(.normal);
                    switch (lookahead.id) {
                        .end, .eof => {
                            self.nextToken(.normal) catch unreachable;
                            break;
                        },
                        else => {},
                    }
                    const event = try self.parseExpression(false);
                    if (!event.isNumberExpression() and !event.isStringLiteral()) {
                        return self.addErrorDetailsAndFail(.{
                            .err = .expected_something_else,
                            .token = self.state.token,
                            .extra = .{ .expected_types = .{
                                .number = true,
                                .number_expression = true,
                                .string_literal = true,
                            } },
                        });
                    }

                    try self.nextToken(.normal);
                    try self.check(.comma);

                    const idvalue = try self.parseExpression(false);
                    if (!idvalue.isNumberExpression()) {
                        return self.addErrorDetailsAndFail(.{
                            .err = .expected_something_else,
                            .token = self.state.token,
                            .extra = .{ .expected_types = .{
                                .number = true,
                                .number_expression = true,
                            } },
                        });
                    }

                    var type_and_options = std.ArrayListUnmanaged(Token){};
                    while (true) {
                        const maybe_comma = try self.lookaheadToken(.normal);
                        if (maybe_comma.id != .comma) break;
                        self.nextToken(.normal) catch unreachable;

                        try self.nextToken(.normal);
                        if (!rc.AcceleratorTypeAndOptions.map.has(self.tokenSlice())) {
                            return self.addErrorDetailsAndFail(.{
                                .err = .expected_something_else,
                                .token = self.state.token,
                                .extra = .{ .expected_types = .{
                                    .accelerator_type_or_option = true,
                                } },
                            });
                        }
                        try type_and_options.append(self.state.arena, self.state.token);
                    }

                    const node = try self.state.arena.create(Node.Accelerator);
                    node.* = .{
                        .event = event,
                        .idvalue = idvalue,
                        .type_and_options = type_and_options.toOwnedSlice(self.state.arena),
                    };
                    try accelerators.append(self.state.arena, &node.base);
                }

                const end_token = self.state.token;
                try self.check(.end);

                const node = try self.state.arena.create(Node.Accelerators);
                node.* = .{
                    .id = id_token,
                    .type = type_token,
                    .common_resource_attributes = common_resource_attributes,
                    .optional_statements = optional_statements,
                    .begin_token = begin_token,
                    .accelerators = accelerators.toOwnedSlice(self.state.arena),
                    .end_token = end_token,
                };
                return &node.base;
            },
            .stringtable => unreachable,
            // Just try everything as a 'generic' resource (raw data or external file)
            // TODO: More fine-grained switch cases as necessary
            else => {
                const common_resource_attributes = try self.parseCommonResourceAttributesLookahead();

                const maybe_begin = try self.lookaheadToken(.normal);
                if (maybe_begin.id == .begin) {
                    self.nextToken(.normal) catch unreachable;

                    if (!resource.canUseRawData()) {
                        try self.addErrorDetails(ErrorDetails{
                            .err = .resource_type_cant_use_raw_data,
                            .token = maybe_begin,
                            .extra = .{ .resource = resource },
                        });
                        return self.addErrorDetailsAndFail(ErrorDetails{
                            .err = .resource_type_cant_use_raw_data,
                            .type = .note,
                            .print_source_line = false,
                            .token = maybe_begin,
                        });
                    }

                    var raw_data = std.ArrayList(*Node).init(self.state.allocator);
                    defer raw_data.deinit();
                    while (true) {
                        const maybe_end_token = try self.lookaheadToken(.normal);
                        switch (maybe_end_token.id) {
                            .comma => {
                                // comma as the first token in a raw data block is an error
                                if (raw_data.items.len == 0) {
                                    return self.addErrorDetailsAndFail(ErrorDetails{
                                        .err = .expected_something_else,
                                        .token = maybe_end_token,
                                        .extra = .{ .expected_types = .{
                                            .number = true,
                                            .number_expression = true,
                                            .string_literal = true,
                                        } },
                                    });
                                }
                                // otherwise just skip over commas
                                self.nextToken(.normal) catch unreachable;
                                continue;
                            },
                            .end => {
                                self.nextToken(.normal) catch unreachable;
                                break;
                            },
                            .eof => {
                                return self.addErrorDetailsAndFail(ErrorDetails{
                                    .err = .unfinished_raw_data_block,
                                    .token = maybe_end_token,
                                });
                            },
                            else => {},
                        }
                        const expression = try self.parseExpression(false);
                        try raw_data.append(expression);

                        if (!expression.isExpressionAlwaysSkipped() and !expression.isNumberExpression() and !expression.isStringLiteral()) {
                            return self.addErrorDetailsAndFail(ErrorDetails{
                                .err = .expected_something_else,
                                .token = expression.getFirstToken(),
                                .extra = .{ .expected_types = .{
                                    .number = true,
                                    .number_expression = true,
                                    .string_literal = true,
                                } },
                            });
                        }

                        if (expression.isNumberExpression()) {
                            const maybe_close_paren = try self.lookaheadToken(.normal);
                            if (maybe_close_paren.id == .close_paren) {
                                // <number expression>) is an error
                                return self.addErrorDetailsAndFail(ErrorDetails{
                                    .err = .expected_token,
                                    .token = maybe_close_paren,
                                    .extra = .{ .expected = .operator },
                                });
                            }
                        }
                    }

                    const node = try self.state.arena.create(Node.ResourceRawData);
                    node.* = .{
                        .id = id_token,
                        .type = type_token,
                        .common_resource_attributes = common_resource_attributes,
                        .raw_data = try self.state.arena.dupe(*Node, raw_data.items),
                    };
                    return &node.base;
                }

                var filename_expression = try self.parseExpression(false);

                const node = try self.state.arena.create(Node.ResourceExternal);
                node.* = .{
                    .id = id_token,
                    .type = type_token,
                    .common_resource_attributes = common_resource_attributes,
                    .filename = filename_expression,
                };
                return &node.base;
            },
        }
    }

    /// Expects the current token to be a literal token that contains the string LANGUAGE
    fn parseLanguageStatement(self: *Self) Error!*Node {
        const language_token = self.state.token;

        const primary_language = try self.parseExpression(false);
        if (!primary_language.isNumberExpression()) {
            return self.addErrorDetailsAndFail(ErrorDetails{
                .err = .expected_something_else,
                .token = primary_language.getFirstToken(),
                .extra = .{ .expected_types = .{
                    .number = true,
                    .number_expression = true,
                } },
            });
        }

        try self.nextToken(.normal);
        try self.check(.comma);

        const sublanguage = try self.parseExpression(false);
        if (!sublanguage.isNumberExpression()) {
            return self.addErrorDetailsAndFail(ErrorDetails{
                .err = .expected_something_else,
                .token = sublanguage.getFirstToken(),
                .extra = .{ .expected_types = .{
                    .number = true,
                    .number_expression = true,
                } },
            });
        }

        const node = try self.state.arena.create(Node.LanguageStatement);
        node.* = .{
            .language_token = language_token,
            .primary_language_id = primary_language,
            .sublanguage_id = sublanguage,
        };
        return &node.base;
    }

    /// Expects the current token to have already been dealt with, and that the
    /// expression will start on the next token.
    /// After return, the current token will have been dealt with.
    fn parseExpression(self: *Self, is_known_to_be_number_expression: bool) Error!*Node {
        try self.nextToken(.normal);
        const possible_lhs: *Node = lhs: {
            switch (self.state.token.id) {
                .quoted_ascii_string, .quoted_wide_string => {
                    const node = try self.state.arena.create(Node.Literal);
                    node.* = .{
                        .token = self.state.token,
                    };
                    return &node.base;
                },
                .literal => {
                    const node = try self.state.arena.create(Node.Literal);
                    node.* = .{
                        .token = self.state.token,
                    };
                    return &node.base;
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

                    const expression = try self.parseExpression(true);

                    if (!expression.isNumberExpression()) {
                        return self.addErrorDetailsAndFail(ErrorDetails{
                            .err = .expected_something_else,
                            .token = expression.getFirstToken(),
                            .extra = .{ .expected_types = .{
                                .number = true,
                                .number_expression = true,
                            } },
                        });
                    }

                    try self.nextToken(.normal);
                    // TODO: Add context to error about where the open paren is
                    try self.check(.close_paren);

                    const node = try self.state.arena.create(Node.GroupedExpression);
                    node.* = .{
                        .open_token = open_paren_token,
                        .expression = expression,
                        .close_token = self.state.token,
                    };
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
                        return &node.base;
                    }
                },
                else => {},
            }

            // TODO: This may not be the correct way to handle this in all cases?
            return self.addErrorDetailsAndFail(ErrorDetails{
                .err = .expected_something_else,
                .token = self.state.token,
                .extra = .{ .expected_types = .{
                    .number = true,
                    .number_expression = true,
                    .string_literal = !is_known_to_be_number_expression,
                } },
            });
        };

        const possible_operator = try self.lookaheadToken(.normal_expect_operator);
        switch (possible_operator.id) {
            .operator => self.nextToken(.normal_expect_operator) catch unreachable,
            else => return possible_lhs,
        }

        const rhs_node = try self.parseExpression(true);

        if (!rhs_node.isNumberExpression()) {
            return self.addErrorDetailsAndFail(ErrorDetails{
                .err = .expected_something_else,
                .token = rhs_node.getFirstToken(),
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
            .right = rhs_node,
        };

        return &node.base;
    }

    fn addErrorDetails(self: *Self, details: ErrorDetails) Allocator.Error!void {
        try self.state.diagnostics.append(details);
    }

    fn addErrorDetailsAndFail(self: *Self, details: ErrorDetails) Error {
        try self.addErrorDetails(details);
        return error.ParseError;
    }

    fn nextToken(self: *Self, comptime method: Lexer.LexMethod) Error!void {
        self.state.token = self.lexer.next(method) catch |err| {
            return self.addErrorDetailsAndFail(self.lexer.getErrorDetails(err));
        };
    }

    fn lookaheadToken(self: *Self, comptime method: Lexer.LexMethod) Error!Token {
        self.state.lookahead_lexer = self.lexer.*;
        return self.state.lookahead_lexer.next(method) catch |err| {
            return self.addErrorDetailsAndFail(self.state.lookahead_lexer.getErrorDetails(err));
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
                return self.addErrorDetailsAndFail(ErrorDetails{
                    .err = .expected_token,
                    .token = self.state.token,
                    .extra = .{ .expected = .literal },
                });
            },
        }
    }

    fn check(self: *Self, expected_token_id: Token.Id) !void {
        if (self.state.token.id != expected_token_id) {
            return self.addErrorDetailsAndFail(ErrorDetails{
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
                return self.addErrorDetailsAndFail(ErrorDetails{
                    .err = .expected_token,
                    .token = self.state.token,
                    .extra = .{ .expected = .literal },
                });
            },
        }
    }
};

fn testParse(source: []const u8, expected_ast_dump: []const u8) !void {
    const allocator = std.testing.allocator;
    var diagnostics = Diagnostics.init(allocator);
    defer diagnostics.deinit();
    // TODO: test different code pages
    var lexer = Lexer.init(source, .windows1252);
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

    try testParse("STRINGTABLE { 1+1 \"hello\" }",
        \\root
        \\ string_table STRINGTABLE [0 common_resource_attributes]
        \\ {
        \\  string_table_string
        \\   binary_expression +
        \\    literal 1
        \\    literal 1
        \\   "hello"
        \\ }
        \\
    );

    // duplicate optional statements are preserved in the AST
    try testParse("STRINGTABLE LANGUAGE 1,1 LANGUAGE 1,2 { 0 \"hello\" }",
        \\root
        \\ string_table STRINGTABLE [0 common_resource_attributes]
        \\  language_statement LANGUAGE
        \\   literal 1
        \\   literal 1
        \\  language_statement LANGUAGE
        \\   literal 1
        \\   literal 2
        \\ {
        \\  string_table_string
        \\   literal 0
        \\   "hello"
        \\ }
        \\
    );

    try testParse("STRINGTABLE FIXED VERSION 1 CHARACTERISTICS (1+2) { 0 \"hello\" }",
        \\root
        \\ string_table STRINGTABLE [1 common_resource_attributes]
        \\  simple_statement VERSION
        \\   literal 1
        \\  simple_statement CHARACTERISTICS
        \\   grouped_expression
        \\   (
        \\    binary_expression +
        \\     literal 1
        \\     literal 2
        \\   )
        \\ {
        \\  string_table_string
        \\   literal 0
        \\   "hello"
        \\ }
        \\
    );
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

test "accelerators" {
    try testParse("1 ACCELERATORS FIXED VERSION 1 {}",
        \\root
        \\ accelerators 1 ACCELERATORS [1 common_resource_attributes]
        \\  simple_statement VERSION
        \\   literal 1
        \\ {
        \\ }
        \\
    );
    try testParse("1 ACCELERATORS { \"^C\", 1 L\"a\", 2 }",
        \\root
        \\ accelerators 1 ACCELERATORS [0 common_resource_attributes]
        \\ {
        \\  accelerator
        \\   literal "^C"
        \\   literal 1
        \\  accelerator
        \\   literal L"a"
        \\   literal 2
        \\ }
        \\
    );
    try testParse("1 ACCELERATORS { (1+1), -1+1, CONTROL, ASCII, VIRTKEY, ALT, SHIFT }",
        \\root
        \\ accelerators 1 ACCELERATORS [0 common_resource_attributes]
        \\ {
        \\  accelerator CONTROL, ASCII, VIRTKEY, ALT, SHIFT
        \\   grouped_expression
        \\   (
        \\    binary_expression +
        \\     literal 1
        \\     literal 1
        \\   )
        \\   binary_expression +
        \\    literal -1
        \\    literal 1
        \\ }
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
    try testParseError("expected number or number expression; got '\"hello\"'", "STRINGTABLE { \"hello\" }");
    try testParseError("expected quoted string literal; got '1'", "STRINGTABLE { 1, 1 }");
    try testParseError("expected '<filename>', found '{' (resource type 'icon' can't use raw data)", "1 ICON {}");
    try testParseError("id of resource type 'font' must be an ordinal (u16), got 'string'", "string FONT filename");
    try testParseError("expected accelerator type or option [ASCII, VIRTKEY, etc]; got 'NOTANOPTIONORTYPE'", "1 ACCELERATORS { 1, 1, NOTANOPTIONORTYPE");
    try testParseError("expected number, number expression, or quoted string literal; got 'hello'", "1 ACCELERATORS { hello, 1 }");
    try testParseError("expected number or number expression; got '\"hello\"'", "1 ACCELERATORS { 1, \"hello\" }");
    try testParseError("the number 6 (RT_STRING) cannot be used as a resource type", "1 6 {}");
}

fn testParseError(expected_error_str: []const u8, source: []const u8) !void {
    const allocator = std.testing.allocator;
    var diagnostics = Diagnostics.init(allocator);
    defer diagnostics.deinit();
    // TODO: test different code pages
    var lexer = Lexer.init(source, .windows1252);
    var parser = Parser.init(&lexer);
    var tree = parser.parse(allocator, &diagnostics) catch |err| switch (err) {
        error.OutOfMemory => |e| return e,
        error.ParseError => {
            if (diagnostics.errors.items.len < 1) return error.NoDiagnostics;
            if (diagnostics.errors.items[0].type != .err) @panic("TODO handle parse test with a non-error as the first error");
            var buf: [256]u8 = undefined;
            var fbs = std.io.fixedBufferStream(&buf);
            try diagnostics.errors.items[0].render(fbs.writer(), source, diagnostics.strings.items);
            try std.testing.expectEqualStrings(expected_error_str, fbs.getWritten());
            return;
        },
    };
    std.debug.print("expected parse error, got tree:\n", .{});
    try tree.dump(std.io.getStdErr().writer());
    return error.UnexpectedSuccess;
}
