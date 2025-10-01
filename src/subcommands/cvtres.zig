const std = @import("std");
const cli = @import("../cli.zig");
const Diagnostics = cli.Diagnostics;
const Arg = cli.Arg;
const Options = cli.Options;
const Allocator = std.mem.Allocator;

pub const usage_string_after_command_name =
    \\ [options] [--] <INPUT(S)>
    \\
    \\The sequence -- can be used to signify when to stop parsing options.
    \\This is necessary when the first input path begins with a forward slash.
    \\
    \\Supported option prefixes are /, -, and --, so e.g. /h, -h, and --h all work.
    \\
    \\Supported Win32 CVTRES Options:
    \\  /?, /h               Print this help and exit.
    \\  /v, /verbose         Verbose (print progress messages).
    \\  /define:<symbol>     Define an external symbol with this name.
    \\                       The symbol will have the storage class EXTERNAL.
    \\  /readonly            Do not set the MEM_WRITE flag in the .rsrc section header.
    \\  /out:<filename>      Specify output file path.
    \\  /folddups            Re-use data locations for resources that have identical
    \\                       data.
    \\  /machine:<arch>      Set the target machine for the COFF object file.
    \\                       Can be one of: ARM|ARM64|ARM64EC|ARM64X|EBC|IA64|X64|X86.
    \\
    \\No-op Win32 CVTRES Options:
    \\  /nologo              Options that are recognized but do nothing.
    \\
;

pub fn writeUsage(writer: *std.Io.Writer, command_name: []const u8) !void {
    try writer.writeAll("Usage: ");
    try writer.writeAll(command_name);
    try writer.writeAll(usage_string_after_command_name);
}

pub fn parseCli(allocator: Allocator, args: []const []const u8, diagnostics: *Diagnostics) cli.ParseError!Options {
    var options = Options{
        .allocator = allocator,
        .input_format = .res,
        .output_format = .coff,
        .ignore_include_env_var = true,
        .preprocess = .no,
        .auto_includes = .none,
        .subcommand = .cvtres,
    };
    errdefer options.deinit();

    var output_filename: ?[]const u8 = null;
    var target_specified: bool = false;

    var arg_i: usize = 0;
    next_arg: while (arg_i < args.len) {
        var arg = Arg.fromString(args[arg_i]) orelse break;
        if (arg.name().len == 0) {
            switch (arg.prefix) {
                // -- on its own ends arg parsing
                .long => {
                    arg_i += 1;
                    break;
                },
                // - or / on its own is an error
                else => {
                    var err_details = Diagnostics.ErrorDetails{ .arg_index = arg_i, .arg_span = arg.optionAndAfterSpan() };
                    try err_details.msg.print(allocator, "invalid option: {s}", .{arg.prefixSlice()});
                    try diagnostics.append(err_details);
                    arg_i += 1;
                    continue :next_arg;
                },
            }
        }

        while (arg.name().len > 0) {
            const arg_name = arg.name();

            if (std.ascii.eqlIgnoreCase(arg_name, "nologo")) {
                arg_i += 1;
                continue :next_arg;
            } else if (std.ascii.eqlIgnoreCase(arg_name, "readonly")) {
                options.coff_options.read_only = true;
                arg_i += 1;
                continue :next_arg;
            } else if (std.ascii.eqlIgnoreCase(arg_name, "verbose") or std.ascii.eqlIgnoreCase(arg_name, "v")) {
                options.verbose = true;
                arg_i += 1;
                continue :next_arg;
            } else if (std.ascii.eqlIgnoreCase(arg_name, "folddups")) {
                options.coff_options.fold_duplicate_data = true;
                arg_i += 1;
                continue :next_arg;
            } else if (std.ascii.startsWithIgnoreCase(arg_name, "machine:")) {
                const name_len = ("machine:").len;
                const value = arg.full[arg.name_offset + name_len ..];
                if (value.len == 0) {
                    var err_details = Diagnostics.ErrorDetails{ .arg_index = arg_i, .arg_span = .{
                        .name_offset = arg.name_offset,
                        .prefix_len = arg.prefixSlice().len,
                        .value_offset = arg.name_offset + name_len,
                    } };
                    try err_details.msg.print(allocator, "missing value for {s}{s} option", .{ arg.prefixSlice(), arg.optionWithoutPrefix(name_len) });
                    try diagnostics.append(err_details);
                    arg_i += 1;
                    continue :next_arg;
                }
                const machine = machine_options.get(value) orelse {
                    var err_details = Diagnostics.ErrorDetails{ .arg_index = arg_i, .arg_span = .{
                        .name_offset = arg.name_offset,
                        .prefix_len = arg.prefixSlice().len,
                        .value_offset = arg.name_offset + name_len,
                    } };
                    try err_details.msg.print(allocator, "invalid target architecture: {s}", .{value});
                    try diagnostics.append(err_details);
                    arg_i += 1;
                    continue :next_arg;
                };
                target_specified = true;
                options.coff_options.target = machine;
                arg_i += 1;
                continue :next_arg;
            } else if (std.ascii.startsWithIgnoreCase(arg_name, "define:")) {
                const name_len = ("define:").len;
                const value = arg.full[arg.name_offset + name_len ..];
                if (value.len == 0) {
                    var err_details = Diagnostics.ErrorDetails{ .arg_index = arg_i, .arg_span = .{
                        .name_offset = arg.name_offset,
                        .prefix_len = arg.prefixSlice().len,
                        .value_offset = arg.name_offset + name_len,
                    } };
                    try err_details.msg.print(allocator, "missing value for {s}{s} option", .{ arg.prefixSlice(), arg.optionWithoutPrefix(name_len) });
                    try diagnostics.append(err_details);
                    arg_i += 1;
                    continue :next_arg;
                }
                if (options.coff_options.define_external_symbol) |overwritten_value| {
                    allocator.free(overwritten_value);
                    options.coff_options.define_external_symbol = null;
                }
                options.coff_options.define_external_symbol = try allocator.dupe(u8, value);
                arg_i += 1;
                continue :next_arg;
            } else if (std.ascii.startsWithIgnoreCase(arg_name, "out:")) {
                const name_len = 4;
                const value = arg.full[arg.name_offset + name_len ..];
                if (value.len == 0) {
                    var err_details = Diagnostics.ErrorDetails{ .arg_index = arg_i, .arg_span = .{
                        .name_offset = arg.name_offset,
                        .prefix_len = arg.prefixSlice().len,
                        .value_offset = arg.name_offset + name_len,
                    } };
                    try err_details.msg.print(allocator, "missing value for {s}{s} option", .{ arg.prefixSlice(), arg.optionWithoutPrefix(name_len) });
                    try diagnostics.append(err_details);
                    arg_i += 1;
                    continue :next_arg;
                }
                output_filename = value;
                arg_i += 1;
                continue :next_arg;
            } else if (std.ascii.startsWithIgnoreCase(arg_name, "h") or std.mem.startsWith(u8, arg_name, "?")) {
                options.print_help_and_exit = true;
                // If there's been an error to this point, then we still want to fail
                if (diagnostics.hasError()) return error.ParseError;
                return options;
            } else {
                var err_details = Diagnostics.ErrorDetails{ .arg_index = arg_i, .arg_span = arg.optionAndAfterSpan() };
                try err_details.msg.print(allocator, "invalid option: {s}{s}", .{ arg.prefixSlice(), arg.name() });
                try diagnostics.append(err_details);
                arg_i += 1;
                continue :next_arg;
            }
        } else {
            // The while loop exited via its conditional, meaning we are done with
            // the current arg and can move on the the next
            arg_i += 1;
            continue;
        }
    }

    const positionals = args[arg_i..];

    if (positionals.len == 0) {
        var err_details = Diagnostics.ErrorDetails{ .print_args = false, .arg_index = arg_i };
        try err_details.msg.appendSlice(allocator, "missing input filename");
        try diagnostics.append(err_details);

        if (args.len > 0) {
            const last_arg = args[args.len - 1];
            if (arg_i > 0 and last_arg.len > 0 and last_arg[0] == '/' and std.ascii.eqlIgnoreCase(std.fs.path.extension(last_arg), ".res")) {
                var note_details = Diagnostics.ErrorDetails{ .type = .note, .print_args = true, .arg_index = arg_i - 1 };
                try note_details.msg.appendSlice(allocator, "if this argument was intended to be an input filename, adding -- in front of it will exclude it from option parsing");
                try diagnostics.append(note_details);
            }
        }

        // This is a fatal enough problem to justify an early return, since
        // things after this rely on the value of the input filename.
        return error.ParseError;
    }

    for (positionals, 0..) |positional, i| {
        if (i == 0) {
            options.input_source = .{ .filename = try allocator.dupe(u8, positional) };
        } else {
            const duped = try allocator.dupe(u8, positional);
            errdefer allocator.free(duped);
            try options.additional_inputs.append(allocator, duped);
        }
    }

    if (output_filename == null) {
        options.output_source = .{ .filename = try cli.filepathWithExtension(allocator, options.input_source.filename, options.output_format.extension()) };
    } else {
        options.output_source = .{ .filename = try allocator.dupe(u8, output_filename.?) };
    }

    if (diagnostics.hasError()) {
        return error.ParseError;
    }

    if (!target_specified) {
        var warning_details = Diagnostics.ErrorDetails{ .type = .warning, .arg_index = 0, .print_args = false };
        try warning_details.msg.appendSlice(allocator, "machine type not specified, assuming /MACHINE:X64");
        try diagnostics.append(warning_details);
    }

    return options;
}

pub const machine_options = std.StaticStringMapWithEql(
    std.coff.MachineType,
    std.static_string_map.eqlAsciiIgnoreCase,
).initComptime(.{
    .{ "ARM", .ARMNT },
    .{ "ARM64", .ARM64 },
    .{ "ARM64EC", .ARM64EC },
    .{ "ARM64X", .ARM64X },
    .{ "EBC", .EBC },
    .{ "IA64", .IA64 },
    .{ "X64", .X64 },
    .{ "X86", .I386 },
});

comptime {
    const kvs_len = machine_options.kvs.len;
    for (machine_options.kvs.keys[0..kvs_len], machine_options.kvs.values[0..kvs_len]) |k, v| {
        if (!@import("../cvtres.zig").supported_targets.isSupported(v)) {
            @compileError(std.fmt.comptimePrint("unsupported target {s} (from option {s})", .{ @tagName(v), k }));
        }
    }
}
