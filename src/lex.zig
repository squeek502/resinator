//! Expects to be run after the C preprocessor and after `removeComments`.
//! This means that the lexer assumes that:
//! - Splices (\ at the end of a line) have been handled/collapsed.
//! - Preprocessor directives and macros have been expanded (any remaing should be skipped with the exception of `#pragma code_page`).
//! - All comments have been removed.

const std = @import("std");
const Resource = @import("rc.zig").Resource;
const isValidNumberDataLiteral = @import("literals.zig").isValidNumberDataLiteral;

const dumpTokensDuringTests = true;

pub const Token = struct {
    id: Id,
    start: usize,
    end: usize,
    line_number: usize,

    pub const Id = enum {
        literal,
        quoted_ascii_string,
        quoted_wide_string,
        operator,
        open_brace,
        close_brace,
        comma,
        open_paren,
        close_paren,
        invalid,
        eof,
    };

    pub fn slice(self: Token, buffer: []const u8) []const u8 {
        return buffer[self.start..self.end];
    }
};

pub const LexError = error{
    UnfinishedStringLiteral,
    UnfinishedComment,
};

pub const Lexer = struct {
    const Self = @This();

    buffer: []const u8,
    index: usize,
    line_number: usize = 1,
    at_start_of_line: bool = true,
    state_modifier: StateModifier = .none,
    resource_type_seen: ?Resource = null,

    pub const Error = LexError;

    pub fn init(buffer: []const u8) Self {
        return Self{
            .buffer = buffer,
            .index = 0,
        };
    }

    pub fn dump(self: *Self, token: *const Token) void {
        std.debug.print("{s}:{d}: {s}\n", .{ @tagName(token.id), token.line_number, std.fmt.fmtSliceEscapeLower(token.slice(self.buffer)) });
    }

    const StateModifier = enum {
        none,
        seen_id,
        seen_type,
        scope_data,
        language,
    };

    const StateWhitespaceDelimiterOnly = enum {
        start,
        literal,
        preprocessor,
    };

    pub fn nextWhitespaceDelimeterOnly(self: *Self) LexError!Token {
        const start_index = self.index;
        var result = Token{
            .id = .eof,
            .start = start_index,
            .end = undefined,
            .line_number = self.line_number,
        };
        var state = StateWhitespaceDelimiterOnly.start;

        var last_line_ending_index: ?usize = null;
        while (self.index < self.buffer.len) : (self.index += 1) {
            const c = self.buffer[self.index];
            switch (state) {
                .start => switch (c) {
                    '\r', '\n' => {
                        result.start = self.index + 1;
                        result.line_number = self.incrementLineNumber(&last_line_ending_index);
                    },
                    // space, tab, vertical tab, form feed
                    ' ', '\t', '\x0b', '\x0c' => {
                        result.start = self.index + 1;
                    },
                    '#' => {
                        if (self.at_start_of_line) {
                            state = .preprocessor;
                        } else {
                            state = .literal;
                        }
                        self.at_start_of_line = false;
                    },
                    else => {
                        state = .literal;
                        self.at_start_of_line = false;
                    },
                },
                .literal => switch (c) {
                    '\r', '\n', ' ', '\t', '\x0b', '\x0c' => {
                        result.id = .literal;
                        break;
                    },
                    else => {},
                },
                .preprocessor => switch (c) {
                    '\r', '\n' => {
                        result.start = self.index + 1;
                        state = .start;
                        result.line_number = self.incrementLineNumber(&last_line_ending_index);
                    },
                    else => {},
                },
            }
        } else { // got EOF
            switch (state) {
                .start => {},
                .literal => {
                    result.id = .literal;
                },
                .preprocessor => {
                    result.start = self.index;
                },
            }
        }

        result.end = self.index;
        return result;
    }

    const StateNormal = enum {
        start,
        literal_or_quoted_wide_string,
        quoted_ascii_string,
        quoted_wide_string,
        literal,
        minus,
        number_literal,
        preprocessor,
    };

    /// TODO: A not-terrible name
    pub fn nextNormal(self: *Self) LexError!Token {
        const start_index = self.index;
        var result = Token{
            .id = .eof,
            .start = start_index,
            .end = undefined,
            .line_number = self.line_number,
        };
        var state = StateNormal.start;

        var last_line_ending_index: ?usize = null;
        while (self.index < self.buffer.len) : (self.index += 1) {
            const c = self.buffer[self.index];
            switch (state) {
                .start => switch (c) {
                    '\r', '\n' => {
                        result.start = self.index + 1;
                        result.line_number = self.incrementLineNumber(&last_line_ending_index);
                    },
                    // space, tab, vertical tab, form feed
                    ' ', '\t', '\x0b', '\x0c' => {
                        result.start = self.index + 1;
                    },
                    'L', 'l' => {
                        state = .literal_or_quoted_wide_string;
                        self.at_start_of_line = false;
                    },
                    '"' => {
                        state = .quoted_ascii_string;
                        self.at_start_of_line = false;
                    },
                    '+', '&', '|', '~' => {
                        self.index += 1;
                        result.id = .operator;
                        self.at_start_of_line = false;
                        break;
                    },
                    '-' => {
                        state = .minus;
                        self.at_start_of_line = false;
                    },
                    '0'...'9' => {
                        state = .number_literal;
                        self.at_start_of_line = false;
                    },
                    '#' => {
                        if (self.at_start_of_line) {
                            state = .preprocessor;
                        } else {
                            state = .literal;
                        }
                        self.at_start_of_line = false;
                    },
                    '{' => {
                        self.index += 1;
                        result.id = .open_brace;
                        self.at_start_of_line = false;
                        break;
                    },
                    '}' => {
                        self.index += 1;
                        result.id = .close_brace;
                        self.at_start_of_line = false;
                        break;
                    },
                    ',' => {
                        self.index += 1;
                        result.id = .comma;
                        self.at_start_of_line = false;
                        break;
                    },
                    else => {
                        state = .literal;
                        self.at_start_of_line = false;
                    },
                },
                .preprocessor => switch (c) {
                    '\r', '\n' => {
                        result.start = self.index + 1;
                        state = .start;
                        result.line_number = self.incrementLineNumber(&last_line_ending_index);
                    },
                    else => {},
                },
                .minus => switch (c) {
                    ' ', '\t', '\x0b', '\x0c', '\r', '\n', '"', ',', '{', '}', '+', '-', '|', '&', '~', '(', ')' => {
                        result.id = .operator;
                        break;
                    },
                    '0'...'9' => {
                        state = .number_literal;
                    },
                    else => {
                        state = .literal;
                    },
                },
                .number_literal => switch (c) {
                    ' ', '\t', '\x0b', '\x0c', '\r', '\n', '"', ',', '{', '}', '+', '-', '|', '&', '~', '(', ')' => {
                        result.id = .literal;
                        break;
                    },
                    else => {},
                },
                .literal_or_quoted_wide_string => switch (c) {
                    ' ', '\t', '\x0b', '\x0c', '\r', '\n', ',', '{', '}' => {
                        result.id = .literal;
                        break;
                    },
                    '"' => {
                        state = .quoted_wide_string;
                    },
                    else => {
                        state = .literal;
                    },
                },
                .literal => switch (c) {
                    // space, tab, vertical tab, form feed, carriage return, new line, double quotes
                    ' ', '\t', '\x0b', '\x0c', '\r', '\n', '"', ',', '{', '}' => {
                        result.id = .literal;
                        break;
                    },
                    else => {},
                },
                .quoted_ascii_string, .quoted_wide_string => switch (c) {
                    '\r', '\n' => {
                        return LexError.UnfinishedStringLiteral;
                    },
                    // TODO escaped "
                    '"' => {
                        result.id = if (state == .quoted_ascii_string) .quoted_ascii_string else .quoted_wide_string;
                        self.index += 1;
                        break;
                    },
                    else => {},
                },
            }
        } else { // got EOF
            switch (state) {
                .start => {},
                .literal_or_quoted_wide_string, .literal => {
                    result.id = .literal;
                },
                .preprocessor => {
                    result.start = self.index;
                },
                .number_literal => {
                    result.id = .literal; // TODO: Separate number literal token?
                },
                .minus => {
                    result.id = .operator; // TODO: This seems too context dependent, might need a separate token id
                },
                .quoted_ascii_string,
                .quoted_wide_string,
                => return LexError.UnfinishedStringLiteral,
            }
        }

        result.end = self.index;
        return result;
    }

    const StateNumberExpression = enum {
        start,
        invalid,
        minus,
        number_literal,
    };

    pub fn nextNumberExpression(self: *Lexer) LexError!Token {
        const start_index = self.index;
        var result = Token{
            .id = .eof,
            .start = start_index,
            .end = undefined,
            .line_number = self.line_number,
        };
        var state = StateNumberExpression.start;

        var last_line_ending_index: ?usize = null;
        while (self.index < self.buffer.len) : (self.index += 1) {
            const c = self.buffer[self.index];
            switch (state) {
                .start => switch (c) {
                    '\r', '\n' => {
                        result.start = self.index + 1;
                        result.line_number = self.incrementLineNumber(&last_line_ending_index);
                    },
                    // space, tab, vertical tab, form feed
                    ' ', '\t', '\x0b', '\x0c' => {
                        result.start = self.index + 1;
                    },
                    '+', '&', '|', '~' => {
                        self.index += 1;
                        result.id = .operator;
                        self.at_start_of_line = false;
                        break;
                    },
                    '-' => {
                        state = .minus;
                        self.at_start_of_line = false;
                    },
                    '0'...'9' => {
                        state = .number_literal;
                        self.at_start_of_line = false;
                    },
                    '(', ')' => {
                        self.index += 1;
                        result.id = if (c == '(') .open_paren else .close_paren;
                        self.at_start_of_line = false;
                        break;
                    },
                    else => {
                        state = .invalid;
                        self.at_start_of_line = false;
                    },
                },
                .minus => switch (c) {
                    '0'...'9' => {
                        state = .number_literal;
                    },
                    else => {
                        result.id = .operator;
                        break;
                    },
                },
                .number_literal => switch (c) {
                    ' ', '\t', '\x0b', '\x0c', '\r', '\n', '"', ',', '{', '}', '+', '-', '|', '&', '~', '(', ')' => {
                        result.id = .literal;
                        break;
                    },
                    else => {},
                },
                .invalid => switch (c) {
                    ' ', '\t', '\x0b', '\x0c', '\r', '\n', '"', ',', '{', '}', '+', '-', '|', '&', '~', '(', ')' => {
                        result.id = .invalid;
                        break;
                    },
                    else => {},
                },
            }
        } else { // got EOF
            switch (state) {
                .start => {},
                .invalid => {
                    result.id = .invalid;
                },
                .number_literal => {
                    result.id = .literal; // TODO: Separate number literal token?
                },
                .minus => {
                    result.id = .operator; // TODO: This seems too context dependent, might need a separate token id
                },
            }
        }

        result.end = self.index;
        return result;
    }

    /// Like incrementLineNumber but checks that the current char is a line ending first
    fn maybeIncrementLineNumber(self: *Self, last_line_ending_index: *?usize) usize {
        const c = self.buffer[self.index];
        if (c == '\r' or c == '\n') {
            return self.incrementLineNumber(last_line_ending_index);
        }
        return self.line_number;
    }

    /// Increments line_number appropriately (handling line ending pairs)
    /// and returns the new line number.
    /// note: mutates last_line_ending_index.*
    fn incrementLineNumber(self: *Self, last_line_ending_index: *?usize) usize {
        if (self.currentIndexFormsLineEndingPair(last_line_ending_index.*)) {
            last_line_ending_index.* = null;
        } else {
            self.line_number += 1;
            last_line_ending_index.* = self.index;
        }
        self.at_start_of_line = true;
        return self.line_number;
    }

    /// \r\n and \n\r pairs are treated as a single line ending (but not \r\r \n\n)
    /// expects self.index and last_line_ending_index (if non-null) to contain line endings
    fn currentIndexFormsLineEndingPair(self: *Self, last_line_ending_index: ?usize) bool {
        if (last_line_ending_index == null) return false;

        // must immediately precede the current index
        if (last_line_ending_index.? != self.index - 1) return false;

        const cur_line_ending = self.buffer[self.index];
        const last_line_ending = self.buffer[last_line_ending_index.?];

        // sanity check
        std.debug.assert(cur_line_ending == '\r' or cur_line_ending == '\n');
        std.debug.assert(last_line_ending == '\r' or last_line_ending == '\n');

        // can't be \n\n or \r\r
        if (last_line_ending == cur_line_ending) return false;

        return true;
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

fn testLexNormal(source: []const u8, expected_tokens: []const Token.Id) !void {
    var lexer = Lexer.init(source);
    if (dumpTokensDuringTests) std.debug.print("\n----------------------\n{s}\n----------------------\n", .{lexer.buffer});
    for (expected_tokens) |expected_token_id| {
        const token = try lexer.nextNormal();
        if (dumpTokensDuringTests) lexer.dump(&token);
        try std.testing.expectEqual(expected_token_id, token.id);
    }
    const last_token = try lexer.nextNormal();
    try std.testing.expectEqual(Token.Id.eof, last_token.id);
}

fn testLexNumberExpression(source: []const u8, expected_tokens: []const Token.Id) !void {
    var lexer = Lexer.init(source);
    if (dumpTokensDuringTests) std.debug.print("\n----------------------\n{s}\n----------------------\n", .{lexer.buffer});
    for (expected_tokens) |expected_token_id| {
        const token = try lexer.nextNumberExpression();
        if (dumpTokensDuringTests) lexer.dump(&token);
        try std.testing.expectEqual(expected_token_id, token.id);
    }
    const last_token = try lexer.nextNumberExpression();
    try std.testing.expectEqual(Token.Id.eof, last_token.id);
}

fn expectLexError(expected: LexError, actual: anytype) !void {
    try std.testing.expectError(expected, actual);
    if (dumpTokensDuringTests) std.debug.print("{!}\n", .{actual});
}

test "normal: numbers and literals" {
    try testLexNormal("1", &.{.literal});
    try testLexNormal("-1", &.{.literal});
    try testLexNormal("- 1", &.{ .operator, .literal });
    try testLexNormal("-a", &.{.literal});
}

test "number expressions" {
    try testLexNumberExpression("1-a", &.{ .literal, .operator, .invalid });
    try testLexNumberExpression("1-\"hello", &.{ .literal, .operator, .invalid });
    try testLexNumberExpression("1-{", &.{ .literal, .operator, .invalid });
    try testLexNumberExpression("-- 1", &.{ .operator, .operator, .literal });
    // TODO: This is broken, but might need to be handled differently
    //       `rc` treats this as - (interpretted as a number literal) minus 1
    //try testLexNumberExpression("--1", &.{ .operator, .operator, .literal });
}
