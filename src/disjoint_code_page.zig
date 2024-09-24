const std = @import("std");
const lex = @import("lex.zig");
const SourceMappings = @import("source_mapping.zig").SourceMappings;
const SupportedCodePage = @import("code_pages.zig").SupportedCodePage;

pub fn hasDisjointCodePage(source: []const u8, source_mappings: ?*const SourceMappings, default_code_page: SupportedCodePage) bool {
    var line_handler = lex.LineHandler{ .buffer = source };
    var i: usize = 0;
    while (i < source.len) {
        const codepoint = default_code_page.codepointAt(i, source) orelse break;
        const c = codepoint.value;
        switch (c) {
            '\r', '\n' => {
                _ = line_handler.incrementLineNumber(i);
                // Any lines that are not from the root file interrupt the disjoint code page
                if (source_mappings != null and !source_mappings.?.isRootFile(line_handler.line_number)) return false;
            },
            // whitespace is ignored
            ' ',
            '\t',
            '\u{A0}', // NBSP
            => {},
            '#' => {
                if (source_mappings != null and !source_mappings.?.isRootFile(line_handler.line_number)) {
                    return false;
                }
                const start_i = i;
                while (i < source.len and source[i] != '\r' and source[i] != '\n') : (i += 1) {}
                const line = source[start_i..i];
                _ = (lex.parsePragmaCodePage(line) catch |err| switch (err) {
                    error.NotPragma => return false,
                    error.NotCodePagePragma => continue,
                    error.CodePagePragmaUnsupportedCodePage => continue,
                    else => continue,
                }) orelse return false; // DEFAULT interrupts disjoint code page

                // If we got a code page, then it is a disjoint code page pragma
                return true;
            },
            else => {
                // Any other character interrupts the disjoint code page
                return false;
            },
        }

        i += codepoint.byte_len;
    }
    return false;
}

test hasDisjointCodePage {
    try std.testing.expect(hasDisjointCodePage("\xA0\n#pragma code_page(65001)\n", null, .windows1252));
    try std.testing.expect(hasDisjointCodePage("\u{A0}\n#pragma code_page(1252)\n", null, .utf8));
}
