//! Expects to run after a C preprocessor step that preserves comments.
//!
//! `rc` has a peculiar quirk where something like `blah/**/blah` will be
//! transformed into `blahblah` during parsing. However, `clang -E` will
//! transform it into `blah blah`, so in order to match `rc`, we need
//! to remove comments ourselves after the preprocessor runs.
//! Note: Multiline comments that actually span more than one line do
//!       get translated to a space character by `rc`.
//!
//! Removing comments before lexing also allows the lexer to not have to
//! deal with comments which would complicate its implementation (this is something
//! of a tradeoff, as removing comments in a separate pass means that we'll
//! need to iterate the source twice instead of once, but having to deal with
//! comments when lexing would be a pain).

const std = @import("std");
const Allocator = std.mem.Allocator;
const UncheckedSliceWriter = @import("utils.zig").UncheckedSliceWriter;

/// `buf` must be at least as long as `source`
/// In-place transformation is supported (i.e. `source` and `buf` can be the same slice)
pub fn removeComments(source: []const u8, buf: []u8) []u8 {
    std.debug.assert(buf.len >= source.len);
    var result = UncheckedSliceWriter{ .slice = buf };
    const State = enum {
        start,
        forward_slash,
        line_comment,
        multiline_comment,
        multiline_comment_end,
        single_quoted,
        single_quoted_escape,
        double_quoted,
        double_quoted_escape,
    };
    var state: State = .start;
    var index: usize = 0;
    var pending_start: ?usize = null;
    var multiline_comment_contains_newline = false;
    while (index < source.len) : (index += 1) {
        const c = source[index];
        switch (state) {
            .start => switch (c) {
                '/' => {
                    state = .forward_slash;
                    pending_start = index;
                },
                else => {
                    switch (c) {
                        '"' => state = .double_quoted,
                        '\'' => state = .single_quoted,
                        else => {},
                    }
                    result.write(c);
                },
            },
            .forward_slash => switch (c) {
                '/' => state = .line_comment,
                '*' => {
                    multiline_comment_contains_newline = false;
                    state = .multiline_comment;
                },
                else => {
                    result.writeSlice(source[pending_start.? .. index + 1]);
                    pending_start = null;
                    state = .start;
                },
            },
            .line_comment => switch (c) {
                '\r', '\n' => {
                    if (c == '\r' and index + 1 < source.len and source[index + 1] == '\n') {
                        index += 1;
                    }
                    state = .start;
                },
                else => {},
            },
            .multiline_comment => switch (c) {
                '\r', '\n' => multiline_comment_contains_newline = true,
                '*' => state = .multiline_comment_end,
                else => {},
            },
            .multiline_comment_end => switch (c) {
                '\r', '\n' => multiline_comment_contains_newline = true,
                '/' => {
                    if (multiline_comment_contains_newline) {
                        result.write(' ');
                    }
                    multiline_comment_contains_newline = false;
                    state = .start;
                },
                else => {},
            },
            .single_quoted => switch (c) {
                '\r', '\n' => {
                    state = .start;
                    result.write(c);
                },
                '\\' => {
                    state = .single_quoted_escape;
                    result.write(c);
                },
                '\'' => {
                    state = .start;
                    result.write(c);
                },
                else => {
                    result.write(c);
                },
            },
            .single_quoted_escape => switch (c) {
                '\r', '\n' => {
                    state = .start;
                    result.write(c);
                },
                else => {
                    state = .single_quoted;
                    result.write(c);
                },
            },
            .double_quoted => switch (c) {
                '\r', '\n' => {
                    state = .start;
                    result.write(c);
                },
                '\\' => {
                    state = .double_quoted_escape;
                    result.write(c);
                },
                '"' => {
                    state = .start;
                    result.write(c);
                },
                else => {
                    result.write(c);
                },
            },
            .double_quoted_escape => switch (c) {
                '\r', '\n' => {
                    state = .start;
                    result.write(c);
                },
                else => {
                    state = .double_quoted;
                    result.write(c);
                },
            },
        }
    }
    return result.getWritten();
}

pub fn removeCommentsAlloc(allocator: Allocator, source: []const u8) ![]u8 {
    var buf = try allocator.alloc(u8, source.len);
    var result = removeComments(source, buf);
    return allocator.shrink(buf, result.len);
}

fn testRemoveComments(expected: []const u8, source: []const u8) !void {
    const result = try removeCommentsAlloc(std.testing.allocator, source);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings(expected, result);
}

test "basic" {
    try testRemoveComments("", "// comment");
    try testRemoveComments("", "/* comment */");
}

test "mixed" {
    try testRemoveComments("hello", "hello// comment");
    try testRemoveComments("hello", "hel/* comment */lo");
}

test "crazy" {
    try testRemoveComments(
        \\blah"/*som*/\""BLAH
    ,
        \\blah"/*som*/\""/*ething*/BLAH
    );

    try testRemoveComments(
        \\blah"/*som*/"BLAH "RCDATA BEGIN END
        \\"
    ,
        \\blah"/*som*/"/*ething*/BLAH "RCDATA BEGIN END
        \\// only valid in windres
        \\//"blah blah" RCDATA {}
        \\"
    );
}

test "multiline comment with newlines" {
    try testRemoveComments(
        \\blah blah
    ,
        \\blah/*some
        \\thing*/blah
    );
}

test "comments appended to a line" {
    // TODO
    return error.SkipZigTest;

    // try testRemoveComments(
    //     \\blah
    //     \\blah
    // ,
    //     \\blah // line comment
    //     \\blah
    // );
}

test "in place" {
    var mut_source = "blah /* comment */ blah".*;
    var result = removeComments(&mut_source, &mut_source);
    try std.testing.expectEqualStrings("blah  blah", result);
}
