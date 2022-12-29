const std = @import("std");
const Lexer = @import("lex.zig").Lexer;
const Token = @import("lex.zig").Token;
const Node = @import("ast.zig").Node;
const Tree = @import("ast.zig").Tree;
const CodePageLookup = @import("ast.zig").CodePageLookup;
const Resource = @import("rc.zig").Resource;
const Allocator = std.mem.Allocator;
const ErrorDetails = @import("errors.zig").ErrorDetails;
const Diagnostics = @import("errors.zig").Diagnostics;
const SourceBytes = @import("literals.zig").SourceBytes;
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
        code_page_lookup: CodePageLookup,
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
            .code_page_lookup = CodePageLookup.init(arena.allocator(), self.lexer.current_code_page),
        };

        const parsed_root = try self.parseRoot();

        const tree = try self.state.arena.create(Tree);
        tree.* = .{
            .node = parsed_root,
            .code_pages = self.state.code_page_lookup,
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
    fn parseOptionalStatements(self: *Self, resource: Resource) ![]*Node {
        var optional_statements = std.ArrayListUnmanaged(*Node){};
        while (self.state.token.id == .literal) {
            const slice = self.state.token.slice(self.lexer.buffer);
            const optional_statement_type = rc.OptionalStatements.map.get(slice) orelse switch (resource) {
                .dialog, .dialogex => rc.OptionalStatements.dialog_map.get(slice) orelse break,
                else => break,
            };
            switch (optional_statement_type) {
                .language => {
                    const language = try self.parseLanguageStatement();
                    try optional_statements.append(self.state.arena, language);
                    try self.nextToken(.normal);
                },
                // Number only
                .version, .characteristics, .style, .exstyle => {
                    const identifier = self.state.token;
                    const value = try self.parseExpression(false);
                    try self.checkNumberExpression(value);
                    const node = try self.state.arena.create(Node.SimpleStatement);
                    node.* = .{
                        .identifier = identifier,
                        .value = value,
                    };
                    try optional_statements.append(self.state.arena, &node.base);
                    try self.nextToken(.normal);
                },
                // String only
                .caption => {
                    const identifier = self.state.token;
                    try self.nextToken(.normal);
                    const value = self.state.token;
                    if (!value.isStringLiteral()) {
                        return self.addErrorDetailsAndFail(ErrorDetails{
                            .err = .expected_something_else,
                            .token = value,
                            .extra = .{ .expected_types = .{
                                .string_literal = true,
                            } },
                        });
                    }
                    // TODO: Wrapping this in a Node.Literal is superfluous but necessary
                    //       to put it in a SimpleStatement
                    const value_node = try self.state.arena.create(Node.Literal);
                    value_node.* = .{
                        .token = value,
                    };
                    const node = try self.state.arena.create(Node.SimpleStatement);
                    node.* = .{
                        .identifier = identifier,
                        .value = &value_node.base,
                    };
                    try optional_statements.append(self.state.arena, &node.base);
                    try self.nextToken(.normal);
                },
                // String or number
                .class => {
                    const identifier = self.state.token;
                    const value = try self.parseExpression(false);
                    if (!value.isNumberExpression() and !value.isStringLiteral()) {
                        return self.addErrorDetailsAndFail(ErrorDetails{
                            .err = .expected_something_else,
                            .token = value.getFirstToken(),
                            .extra = .{ .expected_types = .{
                                .number = true,
                                .number_expression = true,
                                .string_literal = true,
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
                // Special case
                .menu => {
                    const identifier = self.state.token;
                    try self.nextToken(.whitespace_delimiter_only);
                    try self.check(.literal);
                    // TODO: Wrapping this in a Node.Literal is superfluous but necessary
                    //       to put it in a SimpleStatement
                    const value_node = try self.state.arena.create(Node.Literal);
                    value_node.* = .{
                        .token = self.state.token,
                    };
                    const node = try self.state.arena.create(Node.SimpleStatement);
                    node.* = .{
                        .identifier = identifier,
                        .value = &value_node.base,
                    };
                    try optional_statements.append(self.state.arena, &node.base);
                    try self.nextToken(.normal);
                },
                .font => {
                    const identifier = self.state.token;
                    const point_size = try self.parseExpression(false);
                    try self.checkNumberExpression(point_size);

                    // The comma between point_size and typeface is both optional and
                    // there can be any number of them
                    try self.skipAnyCommas();

                    try self.nextToken(.normal);
                    const typeface = self.state.token;
                    if (!typeface.isStringLiteral()) {
                        return self.addErrorDetailsAndFail(ErrorDetails{
                            .err = .expected_something_else,
                            .token = typeface,
                            .extra = .{ .expected_types = .{
                                .string_literal = true,
                            } },
                        });
                    }

                    const ExSpecificValues = struct {
                        weight: ?*Node = null,
                        italic: ?*Node = null,
                        char_set: ?*Node = null,
                    };
                    var ex_specific = ExSpecificValues{};
                    ex_specific: {
                        switch (resource) {
                            .dialogex => {
                                {
                                    if (!(try self.parseOptionalToken(.comma))) break :ex_specific;

                                    ex_specific.weight = try self.parseExpression(false);
                                    try self.checkNumberExpression(ex_specific.weight.?);
                                }
                                {
                                    if (!(try self.parseOptionalToken(.comma))) break :ex_specific;

                                    ex_specific.italic = try self.parseExpression(false);
                                    try self.checkNumberExpression(ex_specific.italic.?);
                                }
                                {
                                    if (!(try self.parseOptionalToken(.comma))) break :ex_specific;

                                    ex_specific.char_set = try self.parseExpression(false);
                                    try self.checkNumberExpression(ex_specific.char_set.?);
                                }
                            },
                            .dialog => {},
                            else => @panic("TODO non-DIALOG resource with FONT optional statement"),
                        }
                    }

                    const node = try self.state.arena.create(Node.FontStatement);
                    node.* = .{
                        .identifier = identifier,
                        .point_size = point_size,
                        .typeface = typeface,
                        .weight = ex_specific.weight,
                        .italic = ex_specific.italic,
                        .char_set = ex_specific.char_set,
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
                const optional_statements = try self.parseOptionalStatements(.stringtable);

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

                    const comma_token: ?Token = if (try self.parseOptionalToken(.comma)) self.state.token else null;

                    try self.nextToken(.normal);
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
        const id_code_page = self.lexer.current_code_page;
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
            const id_bytes = SourceBytes{
                .slice = id_token.slice(self.lexer.buffer),
                .code_page = id_code_page,
            };
            const maybe_ordinal = res.NameOrOrdinal.maybeOrdinalFromString(id_bytes);
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
                const optional_statements = try self.parseOptionalStatements(resource);

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
                        if (!(try self.parseOptionalToken(.comma))) break;

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
                        .type_and_options = try type_and_options.toOwnedSlice(self.state.arena),
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
                    .accelerators = try accelerators.toOwnedSlice(self.state.arena),
                    .end_token = end_token,
                };
                return &node.base;
            },
            .dialog, .dialogex => {
                // common resource attributes must all be contiguous and come before optional-statements
                const common_resource_attributes = try self.parseCommonResourceAttributesLookahead();

                const x = try self.parseExpression(false);
                try self.checkNumberExpression(x);
                _ = try self.parseOptionalToken(.comma);

                const y = try self.parseExpression(false);
                try self.checkNumberExpression(y);
                _ = try self.parseOptionalToken(.comma);

                const width = try self.parseExpression(false);
                try self.checkNumberExpression(width);
                _ = try self.parseOptionalToken(.comma);

                const height = try self.parseExpression(false);
                try self.checkNumberExpression(height);
                try self.nextToken(.normal);

                const help_id: ?*Node = help_id: {
                    if (resource == .dialogex and self.state.token.id == .comma) {
                        const expression = try self.parseExpression(false);
                        try self.checkNumberExpression(expression);
                        try self.nextToken(.normal);
                        break :help_id expression;
                    }
                    break :help_id null;
                };

                const optional_statements = try self.parseOptionalStatements(resource);

                const begin_token = self.state.token;
                try self.check(.begin);

                var controls = std.ArrayListUnmanaged(*Node){};
                defer controls.deinit(self.state.allocator);
                while (try self.parseControlStatement(resource)) |control_node| {
                    try controls.append(self.state.allocator, control_node);
                }

                try self.nextToken(.normal);
                const end_token = self.state.token;
                try self.check(.end);

                const node = try self.state.arena.create(Node.Dialog);
                node.* = .{
                    .id = id_token,
                    .type = type_token,
                    .common_resource_attributes = common_resource_attributes,
                    .x = x,
                    .y = y,
                    .width = width,
                    .height = height,
                    .help_id = help_id,
                    .optional_statements = optional_statements,
                    .begin_token = begin_token,
                    .controls = try self.state.arena.dupe(*Node, controls.items),
                    .end_token = end_token,
                };
                return &node.base;
            },
            .menu, .menuex => {
                try self.nextToken(.normal);
                // common resource attributes must all be contiguous and come before optional-statements
                const common_resource_attributes = try self.parseCommonResourceAttributes();
                const optional_statements = try self.parseOptionalStatements(.stringtable);

                const begin_token = self.state.token;
                try self.check(.begin);

                var items = std.ArrayListUnmanaged(*Node){};
                defer items.deinit(self.state.allocator);
                while (try self.parseMenuItemStatement(resource)) |item_node| {
                    try items.append(self.state.allocator, item_node);
                }

                if (items.items.len == 0) {
                    return self.addErrorDetailsAndFail(.{
                        .err = .empty_menu_not_allowed,
                        .token = type_token,
                    });
                }

                try self.nextToken(.normal);
                const end_token = self.state.token;
                try self.check(.end);

                const node = try self.state.arena.create(Node.Menu);
                node.* = .{
                    .id = id_token,
                    .type = type_token,
                    .common_resource_attributes = common_resource_attributes,
                    .optional_statements = optional_statements,
                    .begin_token = begin_token,
                    .items = try self.state.arena.dupe(*Node, items.items),
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

                    const raw_data = try self.parseRawDataBlock();

                    const node = try self.state.arena.create(Node.ResourceRawData);
                    node.* = .{
                        .id = id_token,
                        .type = type_token,
                        .common_resource_attributes = common_resource_attributes,
                        .raw_data = raw_data,
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

    /// Expects the current token to be a begin token.
    /// After return, the current token will be the end token.
    fn parseRawDataBlock(self: *Self) Error![]*Node {
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
        return try self.state.arena.dupe(*Node, raw_data.items);
    }

    /// Expects the current token to be handled, and that the control statement will
    /// begin on the next token.
    /// After return, the current token will be the token immediately before the end of the
    /// control statement (or unchanged if the function returns null).
    fn parseControlStatement(self: *Self, resource: Resource) Error!?*Node {
        const control_token = try self.lookaheadToken(.normal);
        const control = rc.Control.map.get(control_token.slice(self.lexer.buffer)) orelse return null;
        self.nextToken(.normal) catch unreachable;

        try self.skipAnyCommas();

        var text: ?Token = null;
        if (control.hasTextParam()) {
            try self.nextToken(.normal);
            switch (self.state.token.id) {
                .quoted_ascii_string, .quoted_wide_string, .number => {
                    text = self.state.token;
                },
                else => {
                    return self.addErrorDetailsAndFail(ErrorDetails{
                        .err = .expected_something_else,
                        .token = self.state.token,
                        .extra = .{ .expected_types = .{
                            .number = true,
                            .string_literal = true,
                        } },
                    });
                },
            }
            try self.skipAnyCommas();
        }

        const id = try self.parseExpression(false);

        try self.skipAnyCommas();

        var class: ?*Node = null;
        var style: ?*Node = null;
        if (control == .control) {
            class = try self.parseExpression(false);
            if (class.?.id == .literal) {
                const class_literal = @fieldParentPtr(Node.Literal, "base", class.?);
                if (class_literal.token.id == .literal and !rc.ControlClass.map.has(class_literal.token.slice(self.lexer.buffer))) {
                    return self.addErrorDetailsAndFail(.{
                        .err = .expected_something_else,
                        .token = self.state.token,
                        .extra = .{ .expected_types = .{
                            .control_class = true,
                        } },
                    });
                }
            }
            try self.skipAnyCommas();
            style = try self.parseExpression(false);
            try self.skipAnyCommas();
        }

        const x = try self.parseExpression(false);
        _ = try self.parseOptionalToken(.comma);
        const y = try self.parseExpression(false);
        _ = try self.parseOptionalToken(.comma);
        const width = try self.parseExpression(false);
        _ = try self.parseOptionalToken(.comma);
        const height = try self.parseExpression(false);

        if (control != .control) {
            if (try self.parseOptionalToken(.comma)) {
                style = try self.parseExpression(false);
            }
        }

        var exstyle: ?*Node = null;
        if (style != null and try self.parseOptionalToken(.comma)) {
            exstyle = try self.parseExpression(false);
        }
        var help_id: ?*Node = null;
        if (resource == .dialogex and exstyle != null and try self.parseOptionalToken(.comma)) {
            help_id = try self.parseExpression(false);
        }

        var extra_data: []*Node = &[_]*Node{};
        if (try self.parseOptionalToken(.begin)) {
            extra_data = try self.parseRawDataBlock();
        }

        const node = try self.state.arena.create(Node.ControlStatement);
        node.* = .{
            .type = control_token,
            .text = text,
            .class = class,
            .id = id,
            .x = x,
            .y = y,
            .width = width,
            .height = height,
            .style = style,
            .exstyle = exstyle,
            .help_id = help_id,
            .extra_data = extra_data,
        };
        return &node.base;
    }

    /// Expects the current token to be handled, and that the menuitem/popup statement will
    /// begin on the next token.
    /// After return, the current token will be the token immediately before the end of the
    /// menuitem statement (or unchanged if the function returns null).
    fn parseMenuItemStatement(self: *Self, resource: Resource) Error!?*Node {
        const menuitem_token = try self.lookaheadToken(.normal);
        const menuitem = rc.MenuItem.map.get(menuitem_token.slice(self.lexer.buffer)) orelse return null;
        self.nextToken(.normal) catch unreachable;

        switch (resource) {
            .menu => switch (menuitem) {
                .menuitem => {
                    try self.nextToken(.normal);
                    if (rc.MenuItem.isSeparator(self.state.token.slice(self.lexer.buffer))) {
                        const separator_token = self.state.token;
                        // There can be any number of trailing commas after SEPARATOR
                        try self.skipAnyCommas();
                        const node = try self.state.arena.create(Node.MenuItemSeparator);
                        node.* = .{
                            .menuitem = menuitem_token,
                            .separator = separator_token,
                        };
                        return &node.base;
                    } else {
                        const text = self.state.token;
                        if (!text.isStringLiteral()) {
                            return self.addErrorDetailsAndFail(ErrorDetails{
                                .err = .expected_something_else,
                                .token = text,
                                .extra = .{ .expected_types = .{
                                    .string_literal = true,
                                } },
                            });
                        }
                        try self.skipAnyCommas();

                        const result = try self.parseExpression(false);
                        try self.checkNumberExpression(result);

                        _ = try self.parseOptionalToken(.comma);

                        var options = std.ArrayListUnmanaged(Token){};
                        while (true) {
                            const option_token = try self.lookaheadToken(.normal);
                            if (!rc.MenuItem.Option.map.has(option_token.slice(self.lexer.buffer))) {
                                break;
                            }
                            self.nextToken(.normal) catch unreachable;
                            try options.append(self.state.arena, option_token);
                            try self.skipAnyCommas();
                        }

                        const node = try self.state.arena.create(Node.MenuItem);
                        node.* = .{
                            .menuitem = menuitem_token,
                            .text = text,
                            .result = result,
                            .option_list = try options.toOwnedSlice(self.state.arena),
                        };
                        return &node.base;
                    }
                },
                .popup => {
                    try self.nextToken(.normal);
                    const text = self.state.token;
                    if (!text.isStringLiteral()) {
                        return self.addErrorDetailsAndFail(ErrorDetails{
                            .err = .expected_something_else,
                            .token = text,
                            .extra = .{ .expected_types = .{
                                .string_literal = true,
                            } },
                        });
                    }
                    try self.skipAnyCommas();

                    var options = std.ArrayListUnmanaged(Token){};
                    while (true) {
                        const option_token = try self.lookaheadToken(.normal);
                        if (!rc.MenuItem.Option.map.has(option_token.slice(self.lexer.buffer))) {
                            break;
                        }
                        self.nextToken(.normal) catch unreachable;
                        try options.append(self.state.arena, option_token);
                        try self.skipAnyCommas();
                    }

                    try self.nextToken(.normal);
                    const begin_token = self.state.token;
                    try self.check(.begin);

                    var items = std.ArrayListUnmanaged(*Node){};
                    while (try self.parseMenuItemStatement(resource)) |item_node| {
                        try items.append(self.state.arena, item_node);
                    }

                    if (items.items.len == 0) {
                        return self.addErrorDetailsAndFail(.{
                            .err = .empty_menu_not_allowed,
                            .token = menuitem_token,
                        });
                    }

                    try self.nextToken(.normal);
                    const end_token = self.state.token;
                    try self.check(.end);

                    const node = try self.state.arena.create(Node.Popup);
                    node.* = .{
                        .popup = menuitem_token,
                        .text = text,
                        .option_list = try options.toOwnedSlice(self.state.arena),
                        .begin_token = begin_token,
                        .items = try items.toOwnedSlice(self.state.arena),
                        .end_token = end_token,
                    };
                    return &node.base;
                },
            },
            .menuex => {
                try self.nextToken(.normal);
                const text = self.state.token;
                if (!text.isStringLiteral()) {
                    return self.addErrorDetailsAndFail(ErrorDetails{
                        .err = .expected_something_else,
                        .token = text,
                        .extra = .{ .expected_types = .{
                            .string_literal = true,
                        } },
                    });
                }

                var params_finished = false;
                const id = try self.parseMenuItemExParam(&params_finished);
                const item_type = try self.parseMenuItemExParam(&params_finished);
                const state = try self.parseMenuItemExParam(&params_finished);

                if (menuitem == .menuitem) {
                    // trailing comma is allowed, skip it
                    _ = try self.parseOptionalToken(.comma);

                    const node = try self.state.arena.create(Node.MenuItemEx);
                    node.* = .{
                        .menuitem = menuitem_token,
                        .text = text,
                        .id = id,
                        .type = item_type,
                        .state = state,
                    };
                    return &node.base;
                }

                const help_id = try self.parseMenuItemExParam(&params_finished);

                // trailing comma is allowed, skip it
                _ = try self.parseOptionalToken(.comma);

                try self.nextToken(.normal);
                const begin_token = self.state.token;
                try self.check(.begin);

                var items = std.ArrayListUnmanaged(*Node){};
                while (try self.parseMenuItemStatement(resource)) |item_node| {
                    try items.append(self.state.arena, item_node);
                }

                if (items.items.len == 0) {
                    return self.addErrorDetailsAndFail(.{
                        .err = .empty_menu_not_allowed,
                        .token = menuitem_token,
                    });
                }

                try self.nextToken(.normal);
                const end_token = self.state.token;
                try self.check(.end);

                const node = try self.state.arena.create(Node.PopupEx);
                node.* = .{
                    .popup = menuitem_token,
                    .text = text,
                    .id = id,
                    .type = item_type,
                    .state = state,
                    .help_id = help_id,
                    .begin_token = begin_token,
                    .items = try items.toOwnedSlice(self.state.arena),
                    .end_token = end_token,
                };
                return &node.base;
            },
            else => unreachable,
        }
        @compileError("unreachable");
    }

    fn parseMenuItemExParam(self: *Self, finished: *bool) Error!?*Node {
        if (finished.*) return null;
        if (!(try self.parseOptionalToken(.comma))) {
            finished.* = true;
            return null;
        }
        // consecutive commas is treated as a blank parameter and
        // there can still be more parameters afterwards
        if ((try self.lookaheadToken(.normal)).id == .comma) {
            return null;
        }
        const node = try self.parseExpression(false);
        try self.checkNumberExpression(node);
        return node;
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
                    node.* = .{ .token = self.state.token };
                    return &node.base;
                },
                .literal => {
                    const node = try self.state.arena.create(Node.Literal);
                    node.* = .{ .token = self.state.token };
                    return &node.base;
                },
                .number => {
                    const node = try self.state.arena.create(Node.Literal);
                    node.* = .{ .token = self.state.token };
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
                        node.* = .{ .token = self.state.token };
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

    /// Skips any amount of commas (including zero)
    /// In other words, it will skip the regex `,*`
    /// Assumes the token(s) should be parsed with `.normal` as the method.
    fn skipAnyCommas(self: *Self) !void {
        while (try self.parseOptionalToken(.comma)) {}
    }

    /// Advances the current token only if the token's id matches the specified `id`.
    /// Assumes the token should be parsed with `.normal` as the method.
    /// Returns true if the token matched, false otherwise.
    fn parseOptionalToken(self: *Self, id: Token.Id) Error!bool {
        const maybe_token = try self.lookaheadToken(.normal);
        if (maybe_token.id != id) return false;
        self.nextToken(.normal) catch unreachable;
        return true;
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
        // After every token, set the code page for its line
        try self.state.code_page_lookup.setForToken(self.state.token, self.lexer.current_code_page);
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
            .literal => return Resource.fromString(.{
                .slice = self.state.token.slice(self.lexer.buffer),
                .code_page = self.lexer.current_code_page,
            }),
            else => {
                return self.addErrorDetailsAndFail(ErrorDetails{
                    .err = .expected_token,
                    .token = self.state.token,
                    .extra = .{ .expected = .literal },
                });
            },
        }
    }

    fn checkNumberExpression(self: *Self, expression: *Node) !void {
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

test "dialogs" {
    try testParse("1 DIALOG FIXED 1, 2, 3, (3 - 1) LANGUAGE 1, 2 {}",
        \\root
        \\ dialog 1 DIALOG [1 common_resource_attributes]
        \\  x:
        \\   literal 1
        \\  y:
        \\   literal 2
        \\  width:
        \\   literal 3
        \\  height:
        \\   grouped_expression
        \\   (
        \\    binary_expression -
        \\     literal 3
        \\     literal 1
        \\   )
        \\  language_statement LANGUAGE
        \\   literal 1
        \\   literal 2
        \\ {
        \\ }
        \\
    );
    try testParse("1 DIALOGEX 1, 2, 3, 4, 5 {}",
        \\root
        \\ dialog 1 DIALOGEX [0 common_resource_attributes]
        \\  x:
        \\   literal 1
        \\  y:
        \\   literal 2
        \\  width:
        \\   literal 3
        \\  height:
        \\   literal 4
        \\  help_id:
        \\   literal 5
        \\ {
        \\ }
        \\
    );
    try testParse("1 DIALOG 1, 2, 3, 4 FONT 1 \"hello\" {}",
        \\root
        \\ dialog 1 DIALOG [0 common_resource_attributes]
        \\  x:
        \\   literal 1
        \\  y:
        \\   literal 2
        \\  width:
        \\   literal 3
        \\  height:
        \\   literal 4
        \\  font_statement FONT typeface: "hello"
        \\   point_size:
        \\    literal 1
        \\ {
        \\ }
        \\
    );
    try testParse(
        \\1 DIALOGEX FIXED DISCARDABLE 1, 2, 3, 4
        \\STYLE 0x80000000L | 0x00800000L
        \\CAPTION "Error!"
        \\EXSTYLE 1
        \\CLASS "hello1"
        \\CLASS 2
        \\MENU 2+"4"
        \\MENU "1"
        \\FONT 12 "first", 1001-1, 65537L, 257-2
        \\FONT 8+2,, ,, "second", 0
        \\{}
    ,
        \\root
        \\ dialog 1 DIALOGEX [2 common_resource_attributes]
        \\  x:
        \\   literal 1
        \\  y:
        \\   literal 2
        \\  width:
        \\   literal 3
        \\  height:
        \\   literal 4
        \\  simple_statement STYLE
        \\   binary_expression |
        \\    literal 0x80000000L
        \\    literal 0x00800000L
        \\  simple_statement CAPTION
        \\   literal "Error!"
        \\  simple_statement EXSTYLE
        \\   literal 1
        \\  simple_statement CLASS
        \\   literal "hello1"
        \\  simple_statement CLASS
        \\   literal 2
        \\  simple_statement MENU
        \\   literal 2+"4"
        \\  simple_statement MENU
        \\   literal "1"
        \\  font_statement FONT typeface: "first"
        \\   point_size:
        \\    literal 12
        \\   weight:
        \\    binary_expression -
        \\     literal 1001
        \\     literal 1
        \\   italic:
        \\    literal 65537L
        \\   char_set:
        \\    binary_expression -
        \\     literal 257
        \\     literal 2
        \\  font_statement FONT typeface: "second"
        \\   point_size:
        \\    binary_expression +
        \\     literal 8
        \\     literal 2
        \\   weight:
        \\    literal 0
        \\ {
        \\ }
        \\
    );
}

test "dialog controls" {
    try testParse(
        \\1 DIALOGEX 1, 2, 3, 4
        \\{
        \\    AUTO3STATE,, "mytext",, 900,, 1 2 3 4, 0, 0, 100 { "AUTO3STATE" }
        \\    AUTOCHECKBOX "mytext", 901, 1, 2, 3, 4, 0, 0, 100 { "AUTOCHECKBOX" }
        \\    AUTORADIOBUTTON "mytext", 902, 1, 2, 3, 4, 0, 0, 100 { "AUTORADIOBUTTON" }
        \\    CHECKBOX "mytext", 903, 1, 2, 3, 4, 0, 0, 100 { "CHECKBOX" }
        \\    COMBOBOX 904,, 1 2 3 4, 0, 0, 100 { "COMBOBOX" }
        \\    CONTROL "mytext",, 905,, "\x42UTTON",, 1,, 2 3 4 0, 0, 100 { "CONTROL (BUTTON)" }
        \\    CONTROL 1,, 9051,, (0x80+1),, 1,, 2 3 4 0, 0, 100 { "CONTROL (0x80)" }
        \\    CONTROL 1,, 9052,, (0x80+1),, 1,, 2 3 4 0 { "CONTROL (0x80)" }
        \\    CTEXT "mytext", 906, 1, 2, 3, 4, 0, 0, 100 { "CTEXT" }
        \\    CTEXT "mytext", 9061, 1, 2, 3, 4 { "CTEXT" }
        \\    DEFPUSHBUTTON "mytext", 907, 1, 2, 3, 4, 0, 0, 100 { "DEFPUSHBUTTON" }
        \\    EDITTEXT 908, 1, 2, 3, 4, 0, 0, 100 { "EDITTEXT" }
        \\    HEDIT 9081, 1, 2, 3, 4, 0, 0, 100 { "HEDIT" }
        \\    IEDIT 9082, 1, 2, 3, 4, 0, 0, 100 { "IEDIT" }
        \\    GROUPBOX "mytext", 909, 1, 2, 3, 4, 0, 0, 100 { "GROUPBOX" }
        \\    ICON "mytext", 910, 1, 2, 3, 4, 0, 0, 100 { "ICON" }
        \\    LISTBOX 911, 1, 2, 3, 4, 0, 0, 100 { "LISTBOX" }
        \\    LTEXT "mytext", 912, 1, 2, 3, 4, 0, 0, 100 { "LTEXT" }
        \\    PUSHBOX "mytext", 913, 1, 2, 3, 4, 0, 0, 100 { "PUSHBOX" }
        \\    PUSHBUTTON "mytext", 914, 1, 2, 3, 4, 0, 0, 100 { "PUSHBUTTON" }
        \\    RADIOBUTTON "mytext", 915, 1, 2, 3, 4, 0, 0, 100 { "RADIOBUTTON" }
        \\    RTEXT "mytext", 916, 1, 2, 3, 4, 0, 0, 100 { "RTEXT" }
        \\    SCROLLBAR 917, 1, 2, 3, 4, 0, 0, 100 { "SCROLLBAR" }
        \\    STATE3 "mytext", 918, 1, 2, 3, 4, 0, 0, 100 { "STATE3" }
        \\    USERBUTTON "mytext", 919, 1, 2, 3, 4, 0, 0, 100 { "USERBUTTON" }
        \\}
    ,
        \\root
        \\ dialog 1 DIALOGEX [0 common_resource_attributes]
        \\  x:
        \\   literal 1
        \\  y:
        \\   literal 2
        \\  width:
        \\   literal 3
        \\  height:
        \\   literal 4
        \\ {
        \\  control_statement AUTO3STATE text: "mytext"
        \\   id:
        \\    literal 900
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "AUTO3STATE"
        \\  }
        \\  control_statement AUTOCHECKBOX text: "mytext"
        \\   id:
        \\    literal 901
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "AUTOCHECKBOX"
        \\  }
        \\  control_statement AUTORADIOBUTTON text: "mytext"
        \\   id:
        \\    literal 902
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "AUTORADIOBUTTON"
        \\  }
        \\  control_statement CHECKBOX text: "mytext"
        \\   id:
        \\    literal 903
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "CHECKBOX"
        \\  }
        \\  control_statement COMBOBOX
        \\   id:
        \\    literal 904
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "COMBOBOX"
        \\  }
        \\  control_statement CONTROL text: "mytext"
        \\   class:
        \\    literal "\x42UTTON"
        \\   id:
        \\    literal 905
        \\   x:
        \\    literal 2
        \\   y:
        \\    literal 3
        \\   width:
        \\    literal 4
        \\   height:
        \\    literal 0
        \\   style:
        \\    literal 1
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "CONTROL (BUTTON)"
        \\  }
        \\  control_statement CONTROL text: 1
        \\   class:
        \\    grouped_expression
        \\    (
        \\     binary_expression +
        \\      literal 0x80
        \\      literal 1
        \\    )
        \\   id:
        \\    literal 9051
        \\   x:
        \\    literal 2
        \\   y:
        \\    literal 3
        \\   width:
        \\    literal 4
        \\   height:
        \\    literal 0
        \\   style:
        \\    literal 1
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "CONTROL (0x80)"
        \\  }
        \\  control_statement CONTROL text: 1
        \\   class:
        \\    grouped_expression
        \\    (
        \\     binary_expression +
        \\      literal 0x80
        \\      literal 1
        \\    )
        \\   id:
        \\    literal 9052
        \\   x:
        \\    literal 2
        \\   y:
        \\    literal 3
        \\   width:
        \\    literal 4
        \\   height:
        \\    literal 0
        \\   style:
        \\    literal 1
        \\  {
        \\   literal "CONTROL (0x80)"
        \\  }
        \\  control_statement CTEXT text: "mytext"
        \\   id:
        \\    literal 906
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "CTEXT"
        \\  }
        \\  control_statement CTEXT text: "mytext"
        \\   id:
        \\    literal 9061
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\  {
        \\   literal "CTEXT"
        \\  }
        \\  control_statement DEFPUSHBUTTON text: "mytext"
        \\   id:
        \\    literal 907
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "DEFPUSHBUTTON"
        \\  }
        \\  control_statement EDITTEXT
        \\   id:
        \\    literal 908
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "EDITTEXT"
        \\  }
        \\  control_statement HEDIT
        \\   id:
        \\    literal 9081
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "HEDIT"
        \\  }
        \\  control_statement IEDIT
        \\   id:
        \\    literal 9082
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "IEDIT"
        \\  }
        \\  control_statement GROUPBOX text: "mytext"
        \\   id:
        \\    literal 909
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "GROUPBOX"
        \\  }
        \\  control_statement ICON text: "mytext"
        \\   id:
        \\    literal 910
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "ICON"
        \\  }
        \\  control_statement LISTBOX
        \\   id:
        \\    literal 911
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "LISTBOX"
        \\  }
        \\  control_statement LTEXT text: "mytext"
        \\   id:
        \\    literal 912
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "LTEXT"
        \\  }
        \\  control_statement PUSHBOX text: "mytext"
        \\   id:
        \\    literal 913
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "PUSHBOX"
        \\  }
        \\  control_statement PUSHBUTTON text: "mytext"
        \\   id:
        \\    literal 914
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "PUSHBUTTON"
        \\  }
        \\  control_statement RADIOBUTTON text: "mytext"
        \\   id:
        \\    literal 915
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "RADIOBUTTON"
        \\  }
        \\  control_statement RTEXT text: "mytext"
        \\   id:
        \\    literal 916
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "RTEXT"
        \\  }
        \\  control_statement SCROLLBAR
        \\   id:
        \\    literal 917
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "SCROLLBAR"
        \\  }
        \\  control_statement STATE3 text: "mytext"
        \\   id:
        \\    literal 918
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "STATE3"
        \\  }
        \\  control_statement USERBUTTON text: "mytext"
        \\   id:
        \\    literal 919
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "USERBUTTON"
        \\  }
        \\ }
        \\
    );

    // help_id param is not supported if the resource is DIALOG
    try testParseError("expected '<'}' or END>', got ','",
        \\1 DIALOG 1, 2, 3, 4
        \\{
        \\    AUTO3STATE,, "mytext",, 900,, 1 2 3 4, 0, 0, 100 { "AUTO3STATE" }
        \\}
    );

    try testParseError("expected control class [BUTTON, EDIT, etc]; got 'SOMETHING'",
        \\1 DIALOG 1, 2, 3, 4
        \\{
        \\    CONTROL "", 900, SOMETHING, 0, 1, 2, 3, 4
        \\}
    );
}

test "menus" {
    try testParseError(
        "empty menu of type 'MENU' not allowed",
        "1 MENU {}",
    );
    try testParseError(
        "empty menu of type 'MENUEX' not allowed",
        "1 MENUEX {}",
    );
    try testParseError(
        "empty menu of type 'POPUP' not allowed",
        "1 MENU { MENUITEM SEPARATOR POPUP \"\" {} }",
    );
    try testParse(
        \\1 MENU FIXED VERSION 1 CHARACTERISTICS (1+2) {
        \\    MENUITEM SEPARATOR,,
        \\    MENUITEM "HELLO",, 100, CHECKED,, GRAYED,,
        \\    MENUITEM "HELLO" 100 GRAYED INACTIVE
        \\    MENUITEM L"hello" (100+2)
        \\    POPUP "hello" {
        \\        MENUITEM "goodbye", 100
        \\        POPUP "goodbye",, GRAYED CHECKED
        \\        BEGIN
        \\            POPUP "" { MENUITEM SEPARATOR }
        \\        END
        \\    }
        \\}
    ,
        \\root
        \\ menu 1 MENU [1 common_resource_attributes]
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
        \\  menu_item_separator MENUITEM SEPARATOR
        \\  menu_item MENUITEM "HELLO" [2 options]
        \\   literal 100
        \\  menu_item MENUITEM "HELLO" [2 options]
        \\   literal 100
        \\  menu_item MENUITEM L"hello" [0 options]
        \\   grouped_expression
        \\   (
        \\    binary_expression +
        \\     literal 100
        \\     literal 2
        \\   )
        \\  popup POPUP "hello" [0 options]
        \\  {
        \\   menu_item MENUITEM "goodbye" [0 options]
        \\    literal 100
        \\   popup POPUP "goodbye" [2 options]
        \\   BEGIN
        \\    popup POPUP "" [0 options]
        \\    {
        \\     menu_item_separator MENUITEM SEPARATOR
        \\    }
        \\   END
        \\  }
        \\ }
        \\
    );

    try testParse(
        \\1 MENUEX FIXED VERSION 1 CHARACTERISTICS (1+2) {
        \\    MENUITEM "", -1, 0x00000800L
        \\    MENUITEM ""
        \\    MENUITEM "hello",,,,
        \\    MENUITEM "hello",,,1,
        \\    POPUP "hello",,,,, {
        \\        POPUP "goodbye",,,,3,
        \\        BEGIN
        \\            POPUP "" { MENUITEM "" }
        \\        END
        \\    }
        \\}
    ,
        \\root
        \\ menu 1 MENUEX [1 common_resource_attributes]
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
        \\  menu_item_ex MENUITEM ""
        \\   id:
        \\    literal -1
        \\   type:
        \\    literal 0x00000800L
        \\  menu_item_ex MENUITEM ""
        \\  menu_item_ex MENUITEM "hello"
        \\  menu_item_ex MENUITEM "hello"
        \\   state:
        \\    literal 1
        \\  popup_ex POPUP "hello"
        \\  {
        \\   popup_ex POPUP "goodbye"
        \\    help_id:
        \\     literal 3
        \\   BEGIN
        \\    popup_ex POPUP ""
        \\    {
        \\     menu_item_ex MENUITEM ""
        \\    }
        \\   END
        \\  }
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
