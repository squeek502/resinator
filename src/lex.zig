//! Expects to be run after the C preprocessor and after `removeComments`.
//! This means that the lexer assumes that:
//! - Splices (\ at the end of a line) have been handled/collapsed.
//! - Preprocessor directives and macros have been expanded (any remaing should be skipped with the exception of `#pragma code_page`).
//! - All comments have been removed.

const std = @import("std");
const ErrorDetails = @import("errors.zig").ErrorDetails;
const columnsUntilTabStop = @import("literals.zig").columnsUntilTabStop;
const code_pages = @import("code_pages.zig");
const CodePage = code_pages.CodePage;

const dumpTokensDuringTests = false;

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
        begin,
        end,
        comma,
        open_paren,
        close_paren,
        /// This Id is only used for errors, the Lexer will never return one
        /// of these from a `next` call.
        preprocessor_command,
        invalid,
        eof,

        pub fn nameForErrorDisplay(self: Id) []const u8 {
            return switch (self) {
                .literal => "<literal>",
                .number => "<number>",
                .quoted_ascii_string => "<quoted ascii string>",
                .quoted_wide_string => "<quoted wide string>",
                .operator => "<operator>",
                .begin => "<'{' or BEGIN>",
                .end => "<'}' or END>",
                .comma => ",",
                .open_paren => "(",
                .close_paren => ")",
                .preprocessor_command => "<preprocessor command>",
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

    pub fn isStringLiteral(token: Token) bool {
        return token.id == .quoted_ascii_string or token.id == .quoted_wide_string;
    }
};

pub const LexError = error{
    UnfinishedStringLiteral,
    StringLiteralTooLong,
    IllegalByte,
    IllegalByteOutsideStringLiterals,
    IllegalByteOrderMark,
    IllegalPrivateUseCharacter,
    FoundCStyleEscapedQuote,
    CodePagePragmaMissingLeftParen,
    CodePagePragmaMissingRightParen,
    CodePagePragmaInvalidCodePage,
    CodePagePragmaUnsupportedCodePage,
};

pub const Lexer = struct {
    const Self = @This();

    buffer: []const u8,
    index: usize,
    line_number: usize = 1,
    at_start_of_line: bool = true,
    error_context_token: ?Token = null,
    current_code_page: CodePage,

    pub const string_literal_length_limit = 4097;

    pub const Error = LexError;

    pub fn init(buffer: []const u8, default_code_page: CodePage) Self {
        return Self{
            .buffer = buffer,
            .index = 0,
            .current_code_page = default_code_page,
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
        semicolon,
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
        while (self.current_code_page.codepointAt(self.index, self.buffer)) |codepoint| : (self.index += codepoint.byte_len) {
            const c = codepoint.value;
            try self.checkForIllegalCodepoint(codepoint, false);
            switch (state) {
                .start => switch (c) {
                    '\r', '\n' => {
                        result.start = self.index + 1;
                        result.line_number = self.incrementLineNumber(&last_line_ending_index);
                    },
                    ' ', '\t', '\x05'...'\x08', '\x0B'...'\x0C', '\x0E'...'\x1F' => {
                        result.start = self.index + 1;
                    },
                    // NBSP only counts as whitespace at the start of a line (but
                    // can be intermixed with other whitespace). Who knows why.
                    '\xA0' => if (self.at_start_of_line) {
                        result.start = self.index + codepoint.byte_len;
                    } else {
                        state = .literal;
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
                    // Semi-colon acts as a line-terminator, but in this lexing mode
                    // that's only true if it's at the start of a line.
                    ';' => {
                        if (self.at_start_of_line) {
                            state = .semicolon;
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
                        try self.evaluatePreprocessorCommand(result.start, self.index);
                        result.start = self.index + 1;
                        state = .start;
                        result.line_number = self.incrementLineNumber(&last_line_ending_index);
                    },
                    else => {},
                },
                .semicolon => switch (c) {
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
                .start, .semicolon => {},
                .literal => {
                    result.id = .literal;
                },
                .preprocessor => {
                    try self.evaluatePreprocessorCommand(result.start, self.index);
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
        quoted_ascii_string_escape,
        quoted_wide_string_escape,
        quoted_ascii_string_maybe_end,
        quoted_wide_string_maybe_end,
        literal,
        number_literal,
        preprocessor,
        semicolon,
        // end
        e,
        en,
        // begin
        b,
        be,
        beg,
        begi,
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
        while (self.current_code_page.codepointAt(self.index, self.buffer)) |codepoint| : (self.index += codepoint.byte_len) {
            const c = codepoint.value;
            const in_string_literal = switch (state) {
                .quoted_ascii_string,
                .quoted_wide_string,
                .quoted_ascii_string_escape,
                .quoted_wide_string_escape,
                .quoted_ascii_string_maybe_end,
                .quoted_wide_string_maybe_end,
                => true,
                else => false,
            };
            try self.checkForIllegalCodepoint(codepoint, in_string_literal);
            switch (state) {
                .start => switch (c) {
                    '\r', '\n' => {
                        result.start = self.index + 1;
                        result.line_number = self.incrementLineNumber(&last_line_ending_index);
                    },
                    ' ', '\t', '\x05'...'\x08', '\x0B'...'\x0C', '\x0E'...'\x1F' => {
                        result.start = self.index + 1;
                    },
                    // NBSP only counts as whitespace at the start of a line (but
                    // can be intermixed with other whitespace). Who knows why.
                    '\xA0' => if (self.at_start_of_line) {
                        result.start = self.index + codepoint.byte_len;
                    } else {
                        state = .literal;
                        self.at_start_of_line = false;
                    },
                    'L', 'l' => {
                        state = .literal_or_quoted_wide_string;
                        self.at_start_of_line = false;
                    },
                    'E', 'e' => {
                        state = .e;
                        self.at_start_of_line = false;
                    },
                    'B', 'b' => {
                        state = .b;
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
                    '0'...'9', '~', '²', '³', '¹' => {
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
                    ';' => {
                        state = .semicolon;
                        self.at_start_of_line = false;
                    },
                    '{', '}' => {
                        self.index += 1;
                        result.id = if (c == '{') .begin else .end;
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
                        try self.evaluatePreprocessorCommand(result.start, self.index);
                        result.start = self.index + 1;
                        state = .start;
                        result.line_number = self.incrementLineNumber(&last_line_ending_index);
                    },
                    else => {},
                },
                // Semi-colon acts as a line-terminator--everything is skipped until
                // the next line.
                .semicolon => switch (c) {
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
                    '\'', ';', '=',
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
                    '\r', '\n', ',', '{', '}', '+', '-', '|', '&', '~', '(', ')',
                    '\'', ';', '=',
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
                    '\r', '\n', '"', ',', '{', '}', '+', '-', '|', '&', '~', '(', ')',
                    '\'', ';', '=',
                    => {
                    // zig fmt: on
                        result.id = .literal;
                        break;
                    },
                    else => {},
                },
                .e => switch (c) {
                    'N', 'n' => {
                        state = .en;
                    },
                    else => {
                        state = .literal;
                        self.index -= 1;
                    },
                },
                .en => switch (c) {
                    'D', 'd' => {
                        result.id = .end;
                        self.index += 1;
                        break;
                    },
                    else => {
                        state = .literal;
                        self.index -= 1;
                    },
                },
                .b => switch (c) {
                    'E', 'e' => {
                        state = .be;
                    },
                    else => {
                        state = .literal;
                        self.index -= 1;
                    },
                },
                .be => switch (c) {
                    'G', 'g' => {
                        state = .beg;
                    },
                    else => {
                        state = .literal;
                        self.index -= 1;
                    },
                },
                .beg => switch (c) {
                    'I', 'i' => {
                        state = .begi;
                    },
                    else => {
                        state = .literal;
                        self.index -= 1;
                    },
                },
                .begi => switch (c) {
                    'N', 'n' => {
                        result.id = .begin;
                        self.index += 1;
                        break;
                    },
                    else => {
                        state = .literal;
                        self.index -= 1;
                    },
                },
                .quoted_ascii_string, .quoted_wide_string => switch (c) {
                    '"' => {
                        state = if (state == .quoted_ascii_string) .quoted_ascii_string_maybe_end else .quoted_wide_string_maybe_end;
                    },
                    '\\' => {
                        state = if (state == .quoted_ascii_string) .quoted_ascii_string_escape else .quoted_wide_string_escape;
                    },
                    '\r' => {}, // \r doesn't count towards string literal length
                    '\n' => {
                        // first \n expands to <space><\n>
                        if (!string_literal_collapsing_whitespace) {
                            string_literal_length += 2;
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
                .quoted_ascii_string_escape, .quoted_wide_string_escape => switch (c) {
                    '"' => {
                        self.error_context_token = .{
                            .id = .invalid,
                            .start = self.index - 1,
                            .end = self.index + 1,
                            .line_number = self.line_number,
                        };
                        return error.FoundCStyleEscapedQuote;
                    },
                    else => {
                        state = if (state == .quoted_ascii_string_escape) .quoted_ascii_string else .quoted_wide_string;
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
                .start, .semicolon => {},
                .literal_or_quoted_wide_string, .literal, .e, .en, .b, .be, .beg, .begi => {
                    result.id = .literal;
                },
                .preprocessor => {
                    try self.evaluatePreprocessorCommand(result.start, self.index);
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
                .quoted_ascii_string_escape,
                .quoted_wide_string_escape,
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

    fn checkForIllegalCodepoint(self: *Self, codepoint: code_pages.Codepoint, in_string_literal: bool) LexError!void {
        const err = switch (codepoint.value) {
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
            // @ and ` both result in error RC2018: unknown character '0x60' (and subsequently
            // fatal error RC1116: RC terminating after preprocessor errors) if they are ever used
            // outside of string literals. Not exactly sure why this would be the case, though.
            // TODO: Make sure there aren't any exceptions
            '@', '`' => if (!in_string_literal) error.IllegalByteOutsideStringLiterals else return,
            // The Byte Order Mark is mostly skipped over by the Windows RC compiler, but
            // there are edge cases where it leads to cryptic 'compiler limit : macro definition too big'
            // errors (e.g. a BOM within a number literal). By making this illegal we avoid having to
            // deal with a lot of edge cases and remove the potential footgun of the bytes of a BOM
            // being 'missing' when included in a string literal (the Windows RC compiler acts as
            // if the codepoint was never part of the string literal).
            '\u{FEFF}' => error.IllegalByteOrderMark,
            // Similar deal with this private use codepoint, it gets skipped/ignored by the
            // RC compiler (but without the cryptic errors). Silently dropping bytes still seems like
            // enough of a footgun with no real use-cases that it's still worth erroring instead of
            // emulating the RC compiler's behavior, though.
            '\u{E000}' => error.IllegalPrivateUseCharacter,
            // These codepoints lead to strange errors when used outside of string literals,
            // and miscompilations when used within string literals. We avoid the miscompilation
            // within string literals and emit a warning, but outside of string literals it makes
            // more sense to just disallow these codepoints.
            // TODO: Error message that can handle codepoints instead of single bytes
            0x900, 0xA00, 0xA0D, 0x2000, 0xFFFE, 0xD00 => if (!in_string_literal) error.IllegalByteOutsideStringLiterals else return,
            else => return,
        };
        self.error_context_token = .{
            .id = .invalid,
            .start = self.index,
            // TODO: Not all codepoints are 1 byte long, e.g. the BOM, but
            //       for display purposes pointing to one byte makes sense at the moment.
            .end = self.index + 1,
            .line_number = self.line_number,
        };
        return err;
    }

    fn evaluatePreprocessorCommand(self: *Self, start: usize, end: usize) !void {
        const error_context = Token{
            .id = .preprocessor_command,
            .start = start,
            .end = end,
            .line_number = self.line_number,
        };
        const full_command = self.buffer[start..end];
        var command = full_command;

        // Anything besides exactly this is ignored by the Windows RC implementation
        const expected_directive = "#pragma";
        if (!std.mem.startsWith(u8, command, expected_directive)) return;
        command = command[expected_directive.len..];

        if (command.len == 0 or !std.ascii.isWhitespace(command[0])) return;
        while (command.len > 0 and std.ascii.isWhitespace(command[0])) {
            command = command[1..];
        }

        // Note: CoDe_PaGeZ is also treated as "code_page" by the Windows RC implementation,
        //       and it will error with 'Missing left parenthesis in code_page #pragma'
        const expected_extension = "code_page";
        if (!std.ascii.startsWithIgnoreCase(command, expected_extension)) return;
        command = command[expected_extension.len..];

        while (command.len > 0 and std.ascii.isWhitespace(command[0])) {
            command = command[1..];
        }

        if (command.len == 0 or command[0] != '(') {
            self.error_context_token = error_context;
            return error.CodePagePragmaMissingLeftParen;
        }
        command = command[1..];

        while (command.len > 0 and std.ascii.isWhitespace(command[0])) {
            command = command[1..];
        }

        var num_str: []u8 = command[0..0];
        while (command.len > 0 and std.ascii.isDigit(command[0])) {
            command = command[1..];
            num_str.len += 1;
        }

        if (num_str.len == 0 or num_str[0] == '0') {
            self.error_context_token = error_context;
            return error.CodePagePragmaInvalidCodePage;
        }

        while (command.len > 0 and std.ascii.isWhitespace(command[0])) {
            command = command[1..];
        }

        if (command.len == 0 or command[0] != ')') {
            self.error_context_token = error_context;
            return error.CodePagePragmaMissingRightParen;
        }

        const num = std.fmt.parseUnsigned(u16, num_str, 10) catch {
            self.error_context_token = error_context;
            return error.CodePagePragmaInvalidCodePage;
        };

        const code_page = code_pages.CodePage.getByIdentifier(num) catch |err| switch (err) {
            error.InvalidCodePage => {
                self.error_context_token = error_context;
                return error.CodePagePragmaInvalidCodePage;
            },
            error.UnsupportedCodePage => {
                self.error_context_token = error_context;
                return error.CodePagePragmaUnsupportedCodePage;
            },
        };

        self.current_code_page = code_page;
    }

    pub fn getErrorDetails(self: Self, lex_err: LexError) ErrorDetails {
        const err = switch (lex_err) {
            error.UnfinishedStringLiteral => ErrorDetails.Error.unfinished_string_literal,
            error.StringLiteralTooLong => ErrorDetails.Error.string_literal_too_long,
            error.IllegalByte => ErrorDetails.Error.illegal_byte,
            error.IllegalByteOutsideStringLiterals => ErrorDetails.Error.illegal_byte_outside_string_literals,
            error.IllegalByteOrderMark => ErrorDetails.Error.illegal_byte_order_mark,
            error.IllegalPrivateUseCharacter => ErrorDetails.Error.illegal_private_use_character,
            error.FoundCStyleEscapedQuote => ErrorDetails.Error.found_c_style_escaped_quote,
            error.CodePagePragmaMissingLeftParen => ErrorDetails.Error.code_page_pragma_missing_left_paren,
            error.CodePagePragmaMissingRightParen => ErrorDetails.Error.code_page_pragma_missing_right_paren,
            error.CodePagePragmaInvalidCodePage => ErrorDetails.Error.code_page_pragma_invalid_code_page,
            error.CodePagePragmaUnsupportedCodePage => ErrorDetails.Error.code_page_pragma_unsupported_code_page,
        };
        return .{
            .err = err,
            .token = self.error_context_token.?,
        };
    }
};

fn testLexNormal(source: []const u8, expected_tokens: []const Token.Id) !void {
    var lexer = Lexer.init(source, CodePage.windows1252);
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

test "superscript chars and code pages" {
    const firstToken = struct {
        pub fn firstToken(source: []const u8, default_code_page: CodePage, comptime lex_method: Lexer.LexMethod) LexError!Token {
            var lexer = Lexer.init(source, default_code_page);
            return lexer.next(lex_method);
        }
    }.firstToken;
    const utf8_source = "²";
    const windows1252_source = "\xB2";

    const windows1252_encoded_as_windows1252 = try firstToken(windows1252_source, .windows1252, .normal);
    try std.testing.expectEqual(Token{
        .id = .number,
        .start = 0,
        .end = 1,
        .line_number = 1,
    }, windows1252_encoded_as_windows1252);

    const utf8_encoded_as_windows1252 = try firstToken(utf8_source, .windows1252, .normal);
    try std.testing.expectEqual(Token{
        .id = .literal,
        .start = 0,
        .end = 2,
        .line_number = 1,
    }, utf8_encoded_as_windows1252);

    const utf8_encoded_as_utf8 = try firstToken(utf8_source, .utf8, .normal);
    try std.testing.expectEqual(Token{
        .id = .number,
        .start = 0,
        .end = 2,
        .line_number = 1,
    }, utf8_encoded_as_utf8);

    const windows1252_encoded_as_utf8 = try firstToken(windows1252_source, .utf8, .normal);
    try std.testing.expectEqual(Token{
        .id = .literal,
        .start = 0,
        .end = 1,
        .line_number = 1,
    }, windows1252_encoded_as_utf8);
}
