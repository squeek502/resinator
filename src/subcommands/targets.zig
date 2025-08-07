const std = @import("std");
const supported_targets = @import("../cvtres.zig").supported_targets;

pub fn run() !void {
    var buf: [128]u8 = undefined;
    var buffered_stdout = std.fs.File.stdout().writer(&buf);
    const w = &buffered_stdout.interface;

    for (supported_targets.Arch.ordered_for_display) |arch| {
        try w.print("{s: <" ++ std.fmt.comptimePrint("{}", .{supported_targets.Arch.longest_name + 2}) ++ "} {s}\n", .{ @tagName(arch), arch.description() });
    }

    try w.writeAll(
        \\
        \\Note: 'arm' is an alias for 'armnt' to match how the /MACHINE option works in cvtres.exe.
        \\      This means that there is currently no way to target 32-bit ARM without Thumb-2.
    );

    try buffered_stdout.interface.flush();
}
