const std = @import("std");

pub const ani = @import("ani.zig");
pub const ast = @import("ast.zig");
pub const bmp = @import("bmp.zig");
pub const cli = @import("cli.zig");
pub const code_pages = @import("code_pages.zig");
pub const comments = @import("comments.zig");
pub const compile = @import("compile.zig");
pub const disjoint_code_page = @import("disjoint_code_page.zig");
pub const errors = @import("errors.zig");
pub const fnt = @import("fnt.zig");
pub const ico = @import("ico.zig");
pub const lang = @import("lang.zig");
pub const lex = @import("lex.zig");
pub const literals = @import("literals.zig");
pub const parse = @import("parse.zig");
pub const preprocess = @import("preprocess.zig");
pub const rc = @import("rc.zig");
pub const res = @import("res.zig");
pub const source_mapping = @import("source_mapping.zig");
pub const utils = @import("utils.zig");
pub const windows1252 = @import("windows1252.zig");
pub const cvtres = @import("cvtres.zig");
pub const subcommands = @import("subcommands.zig");

test {
    _ = std.testing.refAllDecls(@This());
}
