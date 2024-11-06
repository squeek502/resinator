const std = @import("std");

pub const targets = @import("subcommands/targets.zig");

test {
    _ = std.testing.refAllDecls(@This());
}
