//! Expects to be run after the C preprocessor and after `removeComments`.
//! This means that the lexer assumes that:
//! - Splices (\ at the end of a line) have been handled/collapsed.
//! - Preprocessor directives and macros have been expanded (any remaing should be skipped with the exception of `#pragma code_page`).
//! - All comments have been removed.

const std = @import("std");
const ErrorDetails = @import("errors.zig").ErrorDetails;
const columnsUntilTabStop = @import("literals.zig").columnsUntilTabStop;

const dumpTokensDuringTests = true;

pub const Token = struct {
    id: Id,
    start: usize,
    end: usize,
    line_number: usize,

    pub const Id = enum {
        literal,
        number,
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

        pub fn nameForErrorDisplay(self: Id) []const u8 {
            return switch (self) {
                .literal => "<literal>",
                .number => "<number>",
                .quoted_ascii_string => "<quoted ascii string>",
                .quoted_wide_string => "<quoted wide string>",
                .operator => "<operator>",
                .open_brace => "<open brace or BEGIN>",
                .close_brace => "<close brace or END>",
                .comma => ",",
                .open_paren => "(",
                .close_paren => ")",
                .invalid => unreachable,
                .eof => "<eof>",
            };
        }
    };

    pub fn slice(self: Token, buffer: []const u8) []const u8 {
        return buffer[self.start..self.end];
    }

    pub fn nameForErrorDisplay(self: Token, buffer: []const u8) []const u8 {
        return switch (self.id) {
            .eof => self.id.nameForErrorDisplay(),
            else => self.slice(buffer),
        };
    }

    pub fn calculateColumn(token: Token, source: []const u8, tab_columns: usize, maybe_line_start: ?usize) usize {
        const line_start = maybe_line_start orelse token.getLineStart(source);

        var i: usize = line_start;
        var column: usize = 0;
        while (i < token.start) : (i += 1) {
            const c = source[i];
            switch (c) {
                '\t' => column += columnsUntilTabStop(column, tab_columns),
                else => column += 1,
            }
        }
        return column;
    }

    pub fn getLineStart(token: Token, source: []const u8) usize {
        const line_start = line_start: {
            if (token.start != 0) {
                // start checking at the byte before the token
                var index = token.start - 1;
                while (true) {
                    if (source[index] == '\n') break :line_start @min(source.len - 1, index + 1);
                    if (index != 0) index -= 1 else break;
                }
            }
            break :line_start 0;
        };
        return line_start;
    }

    pub fn getLine(token: Token, source: []const u8, maybe_line_start: ?usize) []const u8 {
        const line_start = maybe_line_start orelse token.getLineStart(source);

        var line_end = line_start + 1;
        while (line_end < source.len and source[line_end] != '\n') : (line_end += 1) {}
        while (line_end > 0 and source[line_end - 1] == '\r') : (line_end -= 1) {}

        return source[line_start..line_end];
    }
};

pub const LexError = error{
    UnfinishedStringLiteral,
    StringLiteralTooLong,
    IllegalByte,
    IllegalByteOutsideStringLiterals,
};

pub const Lexer = struct {
    const Self = @This();

    buffer: []const u8,
    index: usize,
    line_number: usize = 1,
    at_start_of_line: bool = true,
    error_context_token: ?Token = null,

    pub const string_literal_length_limit = 4097;

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

    pub const LexMethod = enum {
        whitespace_delimiter_only,
        normal,
        normal_expect_operator,
    };

    pub fn next(self: *Self, comptime method: LexMethod) LexError!Token {
        switch (method) {
            .whitespace_delimiter_only => return self.nextWhitespaceDelimeterOnly(),
            .normal => return self.nextNormal(),
            .normal_expect_operator => return self.nextNormalWithContext(.expect_operator),
        }
    }

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
            try self.checkForIllegalByte(c, false);
            switch (state) {
                .start => switch (c) {
                    '\r', '\n' => {
                        result.start = self.index + 1;
                        result.line_number = self.incrementLineNumber(&last_line_ending_index);
                    },
                    ' ', '\t', '\x05'...'\x08', '\x0B'...'\x0C', '\x0E'...'\x1F' => {
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
                    '\r', '\n', ' ', '\t', '\x05'...'\x08', '\x0B'...'\x0C', '\x0E'...'\x1F' => {
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
        quoted_ascii_string_maybe_end,
        quoted_wide_string_maybe_end,
        literal,
        number_literal,
        preprocessor,
    };

    /// TODO: A not-terrible name
    pub fn nextNormal(self: *Self) LexError!Token {
        return self.nextNormalWithContext(.any);
    }

    pub fn nextNormalWithContext(self: *Self, context: enum { expect_operator, any }) LexError!Token {
        const start_index = self.index;
        var result = Token{
            .id = .eof,
            .start = start_index,
            .end = undefined,
            .line_number = self.line_number,
        };
        var state = StateNormal.start;

        var last_line_ending_index: ?usize = null;
        // Note: The Windows RC compiler uses a non-standard method of computing
        //       length for its 'string literal too long' errors; it isn't easily
        //       explained or intuitive (it's sort-of pre-parsed byte length but with
        //       a few of exceptions/edge cases).
        var string_literal_length: usize = 0;
        var string_literal_collapsing_whitespace: bool = false;
        while (self.index < self.buffer.len) : (self.index += 1) {
            const c = self.buffer[self.index];
            const in_string_literal = switch (state) {
                .quoted_ascii_string,
                .quoted_wide_string,
                .quoted_ascii_string_maybe_end,
                .quoted_wide_string_maybe_end,
                => true,
                else => false,
            };
            try self.checkForIllegalByte(c, in_string_literal);
            switch (state) {
                .start => switch (c) {
                    '\r', '\n' => {
                        result.start = self.index + 1;
                        result.line_number = self.incrementLineNumber(&last_line_ending_index);
                    },
                    ' ', '\t', '\x05'...'\x08', '\x0B'...'\x0C', '\x0E'...'\x1F' => {
                        result.start = self.index + 1;
                    },
                    'L', 'l' => {
                        state = .literal_or_quoted_wide_string;
                        self.at_start_of_line = false;
                    },
                    '"' => {
                        state = .quoted_ascii_string;
                        self.at_start_of_line = false;
                        string_literal_collapsing_whitespace = false;
                        string_literal_length = 0;
                    },
                    '+', '&', '|' => {
                        self.index += 1;
                        result.id = .operator;
                        self.at_start_of_line = false;
                        break;
                    },
                    '-' => {
                        if (context == .expect_operator) {
                            self.index += 1;
                            result.id = .operator;
                            self.at_start_of_line = false;
                            break;
                        } else {
                            state = .number_literal;
                            self.at_start_of_line = false;
                        }
                    },
                    '0'...'9', '~' => {
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
                    '{', '}' => {
                        self.index += 1;
                        result.id = if (c == '{') .open_brace else .close_brace;
                        self.at_start_of_line = false;
                        break;
                    },
                    '(', ')' => {
                        self.index += 1;
                        result.id = if (c == '(') .open_paren else .close_paren;
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
                .number_literal => switch (c) {
                    // zig fmt: off
                    ' ', '\t', '\x05'...'\x08', '\x0B'...'\x0C', '\x0E'...'\x1F',
                    '\r', '\n', '"', ',', '{', '}', '+', '-', '|', '&', '~', '(', ')',
                    => {
                    // zig fmt: on
                        result.id = .number;
                        break;
                    },
                    else => {},
                },
                .literal_or_quoted_wide_string => switch (c) {
                    // zig fmt: off
                    ' ', '\t', '\x05'...'\x08', '\x0B'...'\x0C', '\x0E'...'\x1F',
                    '\r', '\n', ',', '{', '}',
                    // zig fmt: on
                    => {
                        result.id = .literal;
                        break;
                    },
                    '"' => {
                        state = .quoted_wide_string;
                        string_literal_collapsing_whitespace = false;
                        string_literal_length = 0;
                    },
                    else => {
                        state = .literal;
                    },
                },
                .literal => switch (c) {
                    // zig fmt: off
                    ' ', '\t', '\x05'...'\x08', '\x0B'...'\x0C', '\x0E'...'\x1F',
                    '\r', '\n', '"', ',', '{', '}',
                    => {
                    // zig fmt: on
                        result.id = .literal;
                        break;
                    },
                    else => {},
                },
                .quoted_ascii_string, .quoted_wide_string => switch (c) {
                    '"' => {
                        state = if (state == .quoted_ascii_string) .quoted_ascii_string_maybe_end else .quoted_wide_string_maybe_end;
                    },
                    '\r' => {}, // \r doesn't count towards string literal length
                    '\n' => {
                        // first \n expands to <space><\n>
                        if (!string_literal_collapsing_whitespace) {
                            string_literal_length += 2; //
                            string_literal_collapsing_whitespace = true;
                        }
                        // the rest are collapsed into the <space><\n>
                    },
                    // only \t, space, Vertical Tab, and Form Feed count as whitespace when collapsing
                    '\t', ' ', '\x0b', '\x0c' => {
                        if (!string_literal_collapsing_whitespace) {
                            if (c == '\t') {
                                // Literal tab characters are counted as the number of space characters
                                // needed to reach the next 8-column tab stop.
                                //
                                // This implemention is ineffecient but hopefully it's enough of an
                                // edge case that it doesn't matter too much. Literal tab characters in
                                // string literals being replaced by a variable number of spaces depending
                                // on which column the tab character is located in the source .rc file seems
                                // like it has extremely limited use-cases, so it seems unlikely that it's used
                                // in real .rc files.
                                var dummy_token = Token{
                                    .start = self.index,
                                    .end = self.index,
                                    .line_number = self.line_number,
                                    .id = .invalid,
                                };
                                dummy_token.start = self.index;
                                const current_column = dummy_token.calculateColumn(self.buffer, 8, null);
                                string_literal_length += columnsUntilTabStop(current_column, 8);
                            } else {
                                string_literal_length += 1;
                            }
                        }
                    },
                    else => {
                        string_literal_collapsing_whitespace = false;
                        string_literal_length += 1;
                    },
                },
                .quoted_ascii_string_maybe_end, .quoted_wide_string_maybe_end => switch (c) {
                    '"' => {
                        state = if (state == .quoted_ascii_string_maybe_end) .quoted_ascii_string else .quoted_wide_string;
                        // Escaped quotes only count as 1 char for string literal length checks,
                        // so we don't increment string_literal_length here.
                    },
                    else => {
                        result.id = if (state == .quoted_ascii_string_maybe_end) .quoted_ascii_string else .quoted_wide_string;
                        break;
                    },
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
                    result.id = .number;
                },
                .quoted_ascii_string_maybe_end, .quoted_wide_string_maybe_end => {
                    result.id = if (state == .quoted_ascii_string_maybe_end) .quoted_ascii_string else .quoted_wide_string;
                },
                .quoted_ascii_string,
                .quoted_wide_string,
                => {
                    self.error_context_token = .{
                        .id = .eof,
                        .start = self.index,
                        .end = self.index,
                        .line_number = self.line_number,
                    };
                    return LexError.UnfinishedStringLiteral;
                },
            }
        }

        if (result.id == .quoted_ascii_string or result.id == .quoted_wide_string) {
            if (string_literal_length > string_literal_length_limit) {
                self.error_context_token = result;
                return LexError.StringLiteralTooLong;
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

    fn checkForIllegalByte(self: *Self, byte: u8, in_string_literal: bool) LexError!void {
        const err = switch (byte) {
            // 0x00 = NUL
            // 0x1A = Substitute (treated as EOF)
            // NOTE: 0x1A gets treated as EOF by the clang preprocessor so after a .rc file
            //       is run through the clang preprocessor it will no longer have 0x1A characters in it.
            // 0x7F = DEL (treated as a context-specific terminator by the Windows RC compiler)
            0x00, 0x1A, 0x7F => error.IllegalByte,
            // 0x01...0x03 result in strange 'macro definition too big' errors when used outside of string literals
            // 0x04 is valid but behaves strangely (sort of acts as a 'skip the next character' instruction)
            // TODO: re-evaluate 0x04
            0x01...0x04 => if (!in_string_literal) error.IllegalByteOutsideStringLiterals else return,
            else => return,
        };
        self.error_context_token = .{
            .id = .invalid,
            .start = self.index,
            .end = self.index + 1,
            .line_number = self.line_number,
        };
        return err;
    }

    pub fn getErrorDetails(self: Self, lex_err: LexError) ErrorDetails {
        const err = switch (lex_err) {
            error.UnfinishedStringLiteral => ErrorDetails.Error.unfinished_string_literal,
            error.StringLiteralTooLong => ErrorDetails.Error.string_literal_too_long,
            error.IllegalByte => ErrorDetails.Error.illegal_byte,
            error.IllegalByteOutsideStringLiterals => ErrorDetails.Error.illegal_byte_outside_string_literals,
        };
        return .{
            .err = err,
            .token = self.error_context_token.?,
        };
    }
};

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

fn expectLexError(expected: LexError, actual: anytype) !void {
    try std.testing.expectError(expected, actual);
    if (dumpTokensDuringTests) std.debug.print("{!}\n", .{actual});
}

test "normal: numbers" {
    try testLexNormal("1", &.{.number});
    try testLexNormal("-1", &.{.number});
    try testLexNormal("- 1", &.{ .number, .number });
    try testLexNormal("-a", &.{.number});
}

test "normal: string literals" {
    try testLexNormal("\"\"", &.{.quoted_ascii_string});
    // "" is an escaped "
    try testLexNormal("\" \"\" \"", &.{.quoted_ascii_string});
}
