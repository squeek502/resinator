//! Expects to be run after the C preprocessor and after `removeComments`.
//! This means that the lexer assumes that:
//! - Splices (\ at the end of a line) have been handled/collapsed.
//! - Preprocessor directives and macros have been expanded (any remaing should be skipped with the exception of `#pragma code_page`).
//! - All comments have been removed.

const std = @import("std");
const Resource = @import("types.zig").Resource;
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
        open_brace,
        close_brace,
        comma,
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
                    'L' => {
                        state = .literal_or_quoted_wide_string;
                        self.at_start_of_line = false;
                    },
                    '"' => {
                        state = .quoted_ascii_string;
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
                .quoted_ascii_string,
                .quoted_wide_string,
                => return LexError.UnfinishedStringLiteral,
            }
        }

        result.end = self.index;
        return result;
    }

    pub fn next(self: *Self) LexError!Token {
        switch (self.state_modifier) {
            .none => {
                const token = try self.nextWhitespaceDelimeterOnly();
                if (token.id == .literal) {
                    if (std.mem.eql(u8, "LANGUAGE", token.slice(self.buffer))) {
                        self.state_modifier = .language;
                    } else {
                        self.state_modifier = .seen_id;
                    }
                }
                return token;
            },
            .language => {
                @panic("TODO: top-level LANGUAGE statements");
            },
            .seen_id => {
                const token = try self.nextWhitespaceDelimeterOnly();
                if (token.id == .literal) {
                    self.state_modifier = .seen_type;
                }
                return token;
            },
            .seen_type => {
                const token = try self.nextNormal();
                switch (token.id) {
                    .quoted_ascii_string, .quoted_wide_string => {
                        // definite filename
                        self.state_modifier = .none;
                    },
                    .literal => {
                        // if it's not a common resource attribute, it will be treated as a filename
                        if (!common_resource_attributes_set.has(token.slice(self.buffer))) {
                            self.state_modifier = .none;
                        }
                        // TODO: check for BEGIN/END
                    },
                    .open_brace => {
                        self.state_modifier = .scope_data;
                    },
                    .comma, .close_brace => {
                        // TODO: This seemingly forces the previous token to be reintrepreted
                        //       as a filename, even if that token is already something else
                        // e.g. foo RCDATA } causes RCDATA to be both the type and the filename
                        self.state_modifier = .none;
                    },
                    .eof => {},
                }
                if (self.state_modifier == .none) {
                    std.debug.print("filename: ", .{});
                }
                return token;
            },
            .scope_data => {
                const token = try self.nextNormal();
                switch (token.id) {
                    .quoted_ascii_string, .quoted_wide_string => {},
                    .literal => {
                        if (!isValidNumberDataLiteral(token.slice(self.buffer))) {
                            // TODO: `rc` has two separate errors depending on whether or not the
                            //       literal is a keyword or not.
                            // error RC2104 : undefined keyword or key name: foo
                            // error RC2164 : unexpected value in RCDATA
                            self.state_modifier = .none;
                        }
                        // TODO: Check for END
                    },
                    .comma => {
                        // TODO: Only allow if there's a valid data type preceding it, otherwise
                        //       "emit Unexpected value in <TYPE>" error
                    },
                    .open_brace => {
                        // TODO: "Unexpected value in <TYPE>" error
                        self.state_modifier = .none;
                    },
                    .close_brace => {
                        self.state_modifier = .none;
                    },
                    .eof => {},
                }
                return token;
            },
        }
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

fn testLex(source: []const u8, expected_tokens: []const Token.Id) !void {
    // remove comments
    const source_without_comments = try @import("comments.zig").removeCommentsAlloc(std.testing.allocator, source);
    defer std.testing.allocator.free(source_without_comments);
    var lexer = Lexer.init(source_without_comments);
    return testLexInitialized(&lexer, expected_tokens);
}

fn testLexInitialized(lexer: *Lexer, expected_tokens: []const Token.Id) !void {
    if (dumpTokensDuringTests) std.debug.print("\n----------------------\n{s}\n----------------------\n", .{lexer.buffer});
    for (expected_tokens) |expected_token_id| {
        const token = try lexer.next();
        if (dumpTokensDuringTests) lexer.dump(&token);
        try std.testing.expectEqual(expected_token_id, token.id);
    }
    const last_token = try lexer.next();
    try std.testing.expectEqual(Token.Id.eof, last_token.id);
}

fn expectLexError(expected: LexError, actual: anytype) !void {
    try std.testing.expectError(expected, actual);
    if (dumpTokensDuringTests) std.debug.print("{!}\n", .{actual});
}

test "basic" {
    try testLex("id ICON \"string\"", &[_]Token.Id{ .literal, .literal, .quoted_ascii_string });
    try testLex("id ICON MOVEABLE filename.ico", &[_]Token.Id{ .literal, .literal, .literal, .literal });
}

// TODO: re-enable this maybe
// test "double quote terminates literals" {
//     try testLex("id\"string\"", &[_]Token.Id{ .literal, .quoted_ascii_string });
// }

test "comments" {
    // NOTE: Comments are meant to be removed before lexing; the testLex function
    //       does this for us.
    try testLex(
        \\id // comment
        \\/*
        \\ multiline
        \\*/
        \\ICON
    ,
        &[_]Token.Id{
            .literal,
            .literal,
        },
    );
    try testLex(
        \\IC/*test*/ON
    ,
        &[_]Token.Id{
            .literal,
        },
    );
    try testLex(
        \\L/*test*/
    ,
        &[_]Token.Id{
            .literal,
        },
    );
}

test "quoted strings" {
    try testLex(
        \\foo RCDATA "/*test*/"
    ,
        &[_]Token.Id{ .literal, .literal, .quoted_ascii_string },
    );
}

test "braces" {
    try testLex("foo RCDATA {}", &[_]Token.Id{ .literal, .literal, .open_brace, .close_brace });
}

test "preprocessor" {
    try testLex("#define blah 1", &[_]Token.Id{});
    try testLex("something #define blah", &[_]Token.Id{ .literal, .literal, .literal });
    try testLex("  /* whitespace and comments */\t#define blah 1", &[_]Token.Id{});
    try testLex(
        \\  # define blah 1
        \\#define blah 1
        \\something
        \\#define blah 1
    ,
        &[_]Token.Id{
            .literal,
        },
    );
}

test "user-defined resource example" {
    try testLex(
        \\array   MYRES   data.res
        \\14      300     custom.res
        \\18 MYRES2
        \\{
        \\   "Here is an ANSI string\0",    // explicitly null-terminated 
        \\   L"Here is a Unicode string\0", // explicitly null-terminated 
        \\   1024,                          // integer, stored as WORD 
        \\   7L,                            // integer, stored as DWORD 
        \\   0x029a,                        // hex integer 
        \\   0o733,                         // octal integer 
        \\}
    ,
    // zig fmt: off
        &[_]Token.Id{
            .literal, .literal, .literal,
            .literal, .literal, .literal,
            .literal, .literal,
            .open_brace,
            .quoted_ascii_string, .comma,
            .quoted_wide_string, .comma,
            .literal, .comma,
            .literal, .comma,
            .literal, .comma,
            .literal, .comma,
            .close_brace,
        },
    // zig fmt: on
    );
}

test "sample resource-definition file" {
    return error.SkipZigTest;

    // try testLex(
    //     \\#include "shapes.h"
    //     \\
    //     \\ShapesCursor  CURSOR  SHAPES.CUR
    //     \\ShapesIcon    ICON    SHAPES.ICO
    //     \\
    //     \\ShapesMenu MENU
    //     \\{
    //     \\    POPUP "&Shape"
    //     \\    {
    //     \\        MENUITEM "&Clear", ID_CLEAR
    //     \\        MENUITEM "&Rectangle", ID_RECT
    //     \\        MENUITEM "&Triangle", ID_TRIANGLE
    //     \\        MENUITEM "&Star", ID_STAR
    //     \\        MENUITEM "&Ellipse", ID_ELLIPSE
    //     \\    }
    //     \\}
    // ,
    // // zig fmt: off
    //     &[_]Token.Id{
    //         .literal, .literal, .literal,
    //         .literal, .literal, .literal,
    //         .literal, .literal,
    //         .open_brace,
    //             .literal, .quoted_ascii_string,
    //             .open_brace,
    //                 .literal, .quoted_ascii_string, .comma, .literal,
    //                 .literal, .quoted_ascii_string, .comma, .literal,
    //                 .literal, .quoted_ascii_string, .comma, .literal,
    //                 .literal, .quoted_ascii_string, .comma, .literal,
    //                 .literal, .quoted_ascii_string, .comma, .literal,
    //             .close_brace,
    //         .close_brace,
    //     },
    // // zig fmt: on
    // );
}
