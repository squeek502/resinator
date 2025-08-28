const std = @import("std");
const cli = @import("../cli.zig");
const Diagnostics = cli.Diagnostics;
const Arg = cli.Arg;
const Options = cli.Options;
const Allocator = std.mem.Allocator;
const code_pages = @import("../code_pages.zig");
const lang = @import("../lang.zig");

pub const usage_string_after_command_name =
    \\ [options] [--] <INPUT> <OUTPUT>
    \\
    \\The sequence -- can be used to signify when to stop parsing options.
    \\
    \\Supported GNU windres Options:
    \\  -i --input=<file>             Name input file
    \\  -o --output=<file>            Name output file
    \\  -J --input-format=<format>    Specify input format
    \\  -O --output-format=<format>   Specify output format
    \\  -F --target=<target>          Specify COFF target
    \\         pe-x86-64              (default) X86 (64-bit)
    \\         pe-i386                X86 (32-bit)
    \\  -I --include-dir=<dir>        Add an include path
    \\  -D --define <name>[=<value>]  Define a symbol (during preprocessing)
    \\  -U --undefine <name>          Undefine a symbol (during preprocessing)
    \\  -v --verbose                  Verbose (print progress messages)
    \\  -c --codepage=<codepage>      Specify default codepage
    \\  -l --language=<value>         Set default language using hexadecimal id (ex: 409)
    \\  -h --help                     Print this help and exit
    \\  -V --version                  Printing version information and exit
    \\
    \\No-op GNU windres Options:
    \\     --preprocessor=<program>   The built-in preprocessor is used instead
    \\     --preprocessor-arg=<arg>   The built-in preprocessor is used instead
    \\     --use-temp-file            The built-in preprocessor is used instead
    \\     --no-use-temp-file         The built-in preprocessor is used instead
    \\  -r                            Ignored for compatibility with rc
    \\
    \\Unsupported GNU windres Options:
    \\  @<file>                 Reading the options from a file is unsupported
    \\
    \\Custom Options (resinator-specific):
    \\  --:auto-includes <value>  Set the automatic include path detection behavior.
    \\    any                     (default) Use MSVC if available, fall back to MinGW
    \\    msvc                    Use MSVC include paths (must be present on the system)
    \\    gnu                     Use MinGW include paths
    \\    none                    Do not use any autodetected include paths
    \\
    \\Note: For compatibility reasons, all custom options start with :
    \\
;

pub fn writeUsage(writer: *std.io.Writer, command_name: []const u8) !void {
    try writer.writeAll("Usage: ");
    try writer.writeAll(command_name);
    try writer.writeAll(usage_string_after_command_name);
}

pub fn parseCli(allocator: Allocator, args: []const []const u8, diagnostics: *Diagnostics) cli.ParseError!Options {
    var options = Options{
        .allocator = allocator,
        .input_format = .res,
        .output_format = .coff,
        .input_source = .{ .stdio = std.fs.File.stdin() },
        .output_source = .{ .stdio = std.fs.File.stdout() },
        .ignore_include_env_var = true,
        .subcommand = .windres,
    };
    errdefer options.deinit();

    var input_filename: ?[]const u8 = null;
    var input_filename_context: union(enum) {
        unspecified: void,
        positional: usize,
        arg: Arg.Context,
    } = .{ .unspecified = {} };
    var output_filename: ?[]const u8 = null;
    var output_filename_context: union(enum) {
        unspecified: void,
        positional: usize,
        arg: Arg.Context,
    } = .{ .unspecified = {} };
    var output_format: ?Options.OutputFormat = null;
    var output_format_context: Arg.Context = undefined;
    var input_format: ?Options.InputFormat = null;
    var input_format_context: Arg.Context = undefined;

    var arg_i: usize = 0;
    next_arg: while (arg_i < args.len) {
        var arg = Arg.fromStringPosix(args[arg_i]) orelse break;
        if (arg.name().len == 0) {
            switch (arg.prefix) {
                // -- on its own ends arg parsing
                .long => {
                    arg_i += 1;
                    break;
                },
                // - on its own is an error
                else => {
                    var err_details = Diagnostics.ErrorDetails{ .arg_index = arg_i, .arg_span = arg.optionAndAfterSpan() };
                    try err_details.msg.print(allocator, "invalid option: {s}", .{arg.prefixSlice()});
                    try diagnostics.append(err_details);
                    arg_i += 1;
                    continue :next_arg;
                },
            }
        }

        next_option: while (arg.name().len > 0) {
            const option_and_len = Option.fromArg(arg) catch |err| switch (err) {
                error.InvalidOption => {
                    var err_details = Diagnostics.ErrorDetails{ .arg_index = arg_i, .arg_span = arg.optionAndAfterSpan() };
                    const option_len: u1 = switch (arg.prefix) {
                        .short => 1,
                        .long => 0, // means the full arg
                        .slash => unreachable,
                    };
                    try err_details.msg.print(allocator, "invalid option: {s}{s}", .{ arg.prefixSlice(), arg.optionWithoutPrefix(option_len) });
                    try diagnostics.append(err_details);
                    if (arg.prefix == .short) {
                        arg.name_offset += 1;
                        continue :next_option;
                    } else {
                        arg_i += 1;
                        continue :next_arg;
                    }
                },
            };

            const maybe_value: ?Arg.Value = value: {
                if (option_and_len.option.hasValue()) {
                    const value = arg.valuePosix(option_and_len.length, arg_i, args) catch {
                        var err_details = Diagnostics.ErrorDetails{ .arg_index = arg_i, .arg_span = arg.missingSpan() };
                        try err_details.msg.print(allocator, "missing value after {s}{s} option", .{ arg.prefixSlice(), arg.optionWithoutPrefix(option_and_len.length) });
                        try diagnostics.append(err_details);
                        arg_i += 1;
                        break :next_arg;
                    };
                    break :value value;
                } else {
                    break :value null;
                }
            };
            switch (option_and_len.option) {
                .input => {
                    const value = maybe_value.?;
                    input_filename_context = .{ .arg = .{ .index = arg_i, .option_len = option_and_len.length, .arg = arg, .value = value } };
                    input_filename = value.slice;
                    arg_i += value.index_increment;
                    continue :next_arg;
                },
                .output => {
                    const value = maybe_value.?;
                    output_filename_context = .{ .arg = .{ .index = arg_i, .option_len = option_and_len.length, .arg = arg, .value = value } };
                    output_filename = value.slice;
                    arg_i += value.index_increment;
                    continue :next_arg;
                },
                .input_format => {
                    const value = maybe_value.?;
                    input_format = std.meta.stringToEnum(Options.InputFormat, value.slice) orelse blk: {
                        var err_details = Diagnostics.ErrorDetails{ .arg_index = arg_i, .arg_span = value.argSpan(arg) };
                        try err_details.msg.print(allocator, "invalid input format setting: {s} ", .{value.slice});
                        try diagnostics.append(err_details);
                        break :blk input_format;
                    };
                    input_format_context = .{ .index = arg_i, .option_len = option_and_len.length, .arg = arg, .value = value };
                    arg_i += value.index_increment;
                    continue :next_arg;
                },
                .output_format => {
                    const value = maybe_value.?;
                    output_format = std.meta.stringToEnum(Options.OutputFormat, value.slice) orelse blk: {
                        var err_details = Diagnostics.ErrorDetails{ .arg_index = arg_i, .arg_span = value.argSpan(arg) };
                        try err_details.msg.print(allocator, "invalid output format setting: {s} ", .{value.slice});
                        try diagnostics.append(err_details);
                        break :blk output_format;
                    };
                    output_format_context = .{ .index = arg_i, .option_len = option_and_len.length, .arg = arg, .value = value };
                    arg_i += value.index_increment;
                    continue :next_arg;
                },
                .target => {
                    const value = maybe_value.?;
                    const windres_target = std.meta.stringToEnum(Target, value.slice) orelse {
                        var err_details = Diagnostics.ErrorDetails{ .arg_index = arg_i, .arg_span = value.argSpan(arg) };
                        try err_details.msg.print(allocator, "invalid or unsupported target: {s} ", .{value.slice});
                        try diagnostics.append(err_details);
                        arg_i += value.index_increment;
                        continue :next_arg;
                    };
                    options.coff_options.target = windres_target.toCoffMachine();
                    arg_i += value.index_increment;
                    continue :next_arg;
                },
                .include_dir => {
                    const value = maybe_value.?;
                    const path = value.slice;
                    const duped = try allocator.dupe(u8, path);
                    errdefer allocator.free(duped);
                    try options.extra_include_paths.append(options.allocator, duped);
                    arg_i += value.index_increment;
                },
                .define => {
                    const value = maybe_value.?;
                    var tokenizer = std.mem.tokenizeScalar(u8, value.slice, '=');
                    // guaranteed to exist since an empty value.slice would invoke
                    // the 'missing symbol to define' branch above
                    const symbol = tokenizer.next().?;
                    const symbol_value = tokenizer.next() orelse "1";
                    if (cli.isValidIdentifier(symbol)) {
                        try options.define(symbol, symbol_value);
                    } else {
                        var err_details = Diagnostics.ErrorDetails{ .type = .warning, .arg_index = arg_i, .arg_span = value.argSpan(arg) };
                        try err_details.msg.print(allocator, "symbol \"{s}\" is not a valid identifier and therefore cannot be defined", .{symbol});
                        try diagnostics.append(err_details);
                    }
                    arg_i += value.index_increment;
                    continue :next_arg;
                },
                .undefine => {
                    const value = maybe_value.?;
                    const symbol = value.slice;
                    if (cli.isValidIdentifier(symbol)) {
                        try options.undefine(symbol);
                    } else {
                        var err_details = Diagnostics.ErrorDetails{ .type = .warning, .arg_index = arg_i, .arg_span = value.argSpan(arg) };
                        try err_details.msg.print(allocator, "symbol \"{s}\" is not a valid identifier and therefore cannot be undefined", .{symbol});
                        try diagnostics.append(err_details);
                    }
                    arg_i += value.index_increment;
                    continue :next_arg;
                },
                .verbose => {
                    options.verbose = true;
                    if (arg.prefix == .short) {
                        arg.name_offset += 1;
                        continue :next_option;
                    } else {
                        arg_i += 1;
                        continue :next_arg;
                    }
                },
                .codepage => {
                    const value = maybe_value.?;
                    const num_str = value.slice;
                    // Note: The base of 0 is intentional to support optionally specifying the codepage as hex.
                    //       This also allows 0b and 0o prefixes which windres doesn't support, but there's no
                    //       harm in having support for those as well.
                    const code_page_id = std.fmt.parseUnsigned(u16, num_str, 0) catch {
                        var err_details = Diagnostics.ErrorDetails{ .arg_index = arg_i, .arg_span = value.argSpan(arg) };
                        try err_details.msg.print(allocator, "invalid code page ID: {s}", .{num_str});
                        try diagnostics.append(err_details);
                        arg_i += value.index_increment;
                        continue :next_arg;
                    };
                    options.default_code_page = code_pages.getByIdentifierEnsureSupported(code_page_id) catch |err| switch (err) {
                        error.InvalidCodePage => {
                            var err_details = Diagnostics.ErrorDetails{ .arg_index = arg_i, .arg_span = value.argSpan(arg) };
                            try err_details.msg.print(allocator, "invalid or unknown code page ID: {}", .{code_page_id});
                            try diagnostics.append(err_details);
                            arg_i += value.index_increment;
                            continue :next_arg;
                        },
                        error.UnsupportedCodePage => {
                            var err_details = Diagnostics.ErrorDetails{ .arg_index = arg_i, .arg_span = value.argSpan(arg) };
                            try err_details.msg.print(allocator, "unsupported code page: {s} (id={})", .{
                                @tagName(code_pages.getByIdentifier(code_page_id) catch unreachable),
                                code_page_id,
                            });
                            try diagnostics.append(err_details);
                            arg_i += value.index_increment;
                            continue :next_arg;
                        },
                    };
                    arg_i += value.index_increment;
                    continue :next_arg;
                },
                .language => {
                    const value = maybe_value.?;
                    const num_str = value.slice;
                    // Note: This uses the rc.exe behavior which may be more permissive than the windres behavior
                    options.default_language_id = lang.parseInt(num_str) catch {
                        var err_details = Diagnostics.ErrorDetails{ .arg_index = arg_i, .arg_span = value.argSpan(arg) };
                        try err_details.msg.print(allocator, "invalid language ID: {s}", .{num_str});
                        try diagnostics.append(err_details);
                        arg_i += value.index_increment;
                        continue :next_arg;
                    };
                    arg_i += value.index_increment;
                    continue :next_arg;
                },
                .help => {
                    options.print_help_and_exit = true;
                    // If there's been an error to this point, then we still want to fail
                    if (diagnostics.hasError()) return error.ParseError;
                    return options;
                },
                .preprocessor, .preprocessor_arg => {
                    const value = maybe_value.?;
                    arg_i += value.index_increment;
                    continue :next_arg;
                },
                .use_temp_file, .no_use_temp_file => {
                    if (arg.prefix == .short) {
                        arg.name_offset += 1;
                        continue :next_option;
                    } else {
                        arg_i += 1;
                        continue :next_arg;
                    }
                },
                .version => {
                    // This is only here to trick meson into thinking we're windres
                    // TODO: Formalize this, probably should be handled similarly to --help,
                    //       i.e. set something in options and return, print at the callsite.
                    std.fs.File.stdout().writeAll("Drop-in compatible with GNU windres.\n") catch {
                        std.process.exit(1);
                    };
                    std.process.exit(0);
                },
                .r => arg.name_offset += option_and_len.length,
                .auto_includes => {
                    const value = maybe_value.?;
                    options.auto_includes = std.meta.stringToEnum(Options.AutoIncludes, value.slice) orelse blk: {
                        var err_details = Diagnostics.ErrorDetails{ .arg_index = arg_i, .arg_span = value.argSpan(arg) };
                        try err_details.msg.print(allocator, "invalid auto includes setting: {s} ", .{value.slice});
                        try diagnostics.append(err_details);
                        break :blk options.auto_includes;
                    };
                    arg_i += value.index_increment;
                    continue :next_arg;
                },
            }
        } else {
            // The while loop exited via its conditional, meaning we are done with
            // the current arg and can move on the the next
            arg_i += 1;
            continue;
        }
    }

    var positionals = args[arg_i..];

    if (input_filename != null and output_filename != null and positionals.len > 0) {
        var err_details = Diagnostics.ErrorDetails{ .arg_index = arg_i + 1 };
        try err_details.msg.appendSlice(allocator, "input and output filenames already specified");
        try diagnostics.append(err_details);
        {
            var note_details = Diagnostics.ErrorDetails{
                .type = .note,
                .arg_index = input_filename_context.arg.index,
                .arg_span = input_filename_context.arg.value.argSpan(input_filename_context.arg.arg),
            };
            try note_details.msg.appendSlice(allocator, "input filename previously specified here");
            try diagnostics.append(note_details);
        }
        {
            var note_details = Diagnostics.ErrorDetails{
                .type = .note,
                .arg_index = output_filename_context.arg.index,
                .arg_span = output_filename_context.arg.value.argSpan(output_filename_context.arg.arg),
            };
            try note_details.msg.appendSlice(allocator, "output filename previously specified here");
            try diagnostics.append(note_details);
        }
    }

    if (input_filename == null and positionals.len > 0) {
        input_filename = positionals[0];
        input_filename_context = .{ .positional = arg_i };
        arg_i += 1;
    }

    positionals = args[arg_i..];

    if (output_filename == null and positionals.len > 0) {
        output_filename = positionals[0];
        output_filename_context = .{ .positional = arg_i };
        arg_i += 1;
    }

    if (input_format == null) {
        if (input_filename) |filename| {
            const inferred_format = formatFromFilename(filename, .input) orelse {
                var err_details: Diagnostics.ErrorDetails = switch (input_filename_context) {
                    .positional => |i| .{ .arg_index = i },
                    .arg => |ctx| .{ .arg_index = ctx.index, .arg_span = ctx.value.argSpan(ctx.arg) },
                    .unspecified => unreachable,
                };
                try err_details.msg.appendSlice(allocator, "unable to infer input format from input filename");
                try diagnostics.append(err_details);

                return error.ParseError;
            };
            input_format = switch (inferred_format) {
                .coff => {
                    var err_details: Diagnostics.ErrorDetails = switch (input_filename_context) {
                        .positional => |i| .{ .arg_index = i, .print_args = false },
                        .arg => |ctx| .{ .arg_index = ctx.index, .arg_span = ctx.value.argSpan(ctx.arg), .print_args = false },
                        .unspecified => unreachable,
                    };
                    try err_details.msg.print(allocator, "the input format '{s}' is unsupported", .{@tagName(inferred_format)});
                    try diagnostics.append(err_details);

                    var note_details: Diagnostics.ErrorDetails = switch (input_filename_context) {
                        .positional => |i| .{ .type = .note, .arg_index = i },
                        .arg => |ctx| .{ .type = .note, .arg_index = ctx.index, .arg_span = ctx.value.argSpan(ctx.arg) },
                        .unspecified => unreachable,
                    };
                    try note_details.msg.appendSlice(allocator, "the input format was inferred from the input file");
                    try diagnostics.append(note_details);

                    return error.ParseError;
                },
                .rc => .rc,
                .res => .res,
            };
        } else {
            input_format = .rc;
        }
    }

    if (output_format == null) {
        if (output_filename) |filename| {
            const inferred_format = formatFromFilename(filename, .output) orelse {
                var err_details: Diagnostics.ErrorDetails = switch (output_filename_context) {
                    .positional => |i| .{ .arg_index = i },
                    .arg => |ctx| .{ .arg_index = ctx.index, .arg_span = ctx.value.argSpan(ctx.arg) },
                    .unspecified => unreachable,
                };
                try err_details.msg.appendSlice(allocator, "unable to infer output format from output filename");
                try diagnostics.append(err_details);

                return error.ParseError;
            };
            output_format = switch (inferred_format) {
                .rc => {
                    var err_details: Diagnostics.ErrorDetails = switch (output_filename_context) {
                        .positional => |i| .{ .arg_index = i, .print_args = false },
                        .arg => |ctx| .{ .arg_index = ctx.index, .arg_span = ctx.value.argSpan(ctx.arg), .print_args = false },
                        .unspecified => unreachable,
                    };
                    try err_details.msg.print(allocator, "the output format '{s}' is unsupported", .{@tagName(inferred_format)});
                    try diagnostics.append(err_details);

                    var note_details: Diagnostics.ErrorDetails = switch (output_filename_context) {
                        .positional => |i| .{ .type = .note, .arg_index = i },
                        .arg => |ctx| .{ .type = .note, .arg_index = ctx.index, .arg_span = ctx.value.argSpan(ctx.arg) },
                        .unspecified => unreachable,
                    };
                    try note_details.msg.appendSlice(allocator, "the output format was inferred from the output filename");
                    try diagnostics.append(note_details);

                    return error.ParseError;
                },
                .coff => .coff,
                .res => .res,
            };
        } else {
            var err_details: Diagnostics.ErrorDetails = .{ .arg_index = arg_i, .print_args = false };
            try err_details.msg.appendSlice(allocator, "either the output filename or output format must be specified");
            try diagnostics.append(err_details);

            var note_details: Diagnostics.ErrorDetails = .{ .type = .note, .arg_index = arg_i, .print_args = false };
            try note_details.msg.appendSlice(allocator, "the default output format 'rc' is not supported in this implementation");
            try diagnostics.append(note_details);

            return error.ParseError;
        }
    }

    options.input_format = input_format.?;
    options.output_format = output_format.?;

    // Check for incompatible options
    if (!cli.isSupportedTransformation(options.input_format, options.output_format)) {
        var err_details = Diagnostics.ErrorDetails{ .arg_index = arg_i, .print_args = false };
        try err_details.msg.print(allocator, "converting input format '{s}' to output format '{s}' is unsupported", .{ @tagName(options.input_format), @tagName(options.output_format) });
        try diagnostics.append(err_details);
    }

    if (input_filename) |filename| {
        options.input_source = .{ .filename = try allocator.dupe(u8, filename) };
    }
    if (output_filename) |filename| {
        options.output_source = .{ .filename = try allocator.dupe(u8, filename) };
    }

    if (diagnostics.hasError()) {
        return error.ParseError;
    }

    return options;
}

/// Windres allows any of these as either input or output formats.
/// Resinator does not, but we use the windres formats here so that
/// we can give a nice error message for unsupported input/output formats.
const Format = enum {
    rc,
    res,
    coff,
};

const ext_to_format = std.StaticStringMapWithEql(Format, std.static_string_map.eqlAsciiIgnoreCase).initComptime(.{
    .{ ".rc", .rc },
    .{ ".res", .res },
    .{ ".exe", .coff },
    .{ ".obj", .coff },
    .{ ".o", .coff },
});

/// Intended to be equivalent to windres.c's format_from_filename.
/// `null` being returned means that the format could not be inferred.
pub fn formatFromFilename(filename: []const u8, io: enum { input, output }) ?Format {
    const extension = std.fs.path.extension(filename);
    if (ext_to_format.get(extension)) |format| {
        return format;
    }

    // Assume output files are coff if we can't infer the type from the name
    if (io == .output) return .coff;

    var buf: [5]u8 = undefined;
    const bytes = std.fs.cwd().readFile(filename, &buf) catch return null;
    if (bytes.len != buf.len) return null;

    // PE executable
    if (bytes[0] == 0x4D and bytes[1] == 0x5A) return .coff;

    // COFF object file
    const magic = std.mem.readInt(u16, bytes[0..2], .little);
    switch (magic) {
        0x14c, // i386
        0x166, // MIPS
        0x184, // Alpha
        0x268, // 68k
        0x1f0, // PowerPC
        0x290, // PA
        => return .coff,
        else => {},
    }

    // RES file
    if (std.mem.eql(u8, bytes, "\x00\x00\x00\x00\x20")) return .res;

    // RC file
    var all_print_or_whitespace = true;
    for (bytes) |byte| {
        all_print_or_whitespace = std.ascii.isPrint(byte) or std.ascii.isWhitespace(byte);
        if (!all_print_or_whitespace) break;
    }
    if (all_print_or_whitespace) return .rc;

    return null;
}

pub const Target = enum {
    @"pe-x86-64",
    @"pe-i386",

    pub fn toCoffMachine(self: Target) std.coff.MachineType {
        return switch (self) {
            .@"pe-x86-64" => .X64,
            .@"pe-i386" => .I386,
        };
    }
};

const Option = enum {
    input,
    output,
    input_format,
    output_format,
    target,
    include_dir,
    define,
    undefine,
    verbose,
    codepage,
    language,
    help,
    r,
    preprocessor,
    preprocessor_arg,
    use_temp_file,
    no_use_temp_file,
    version,
    auto_includes,

    const OptionAndLength = struct {
        option: Option,
        length: usize,
    };

    fn longArg(option: Option) ?[]const u8 {
        return switch (option) {
            .input => "input",
            .output => "output",
            .input_format => "input-format",
            .output_format => "output-format",
            .target => "target",
            .include_dir => "include_dir",
            .define => "define",
            .undefine => "undefine",
            .verbose => "verbose",
            .codepage => "codepage",
            .language => "language",
            .help => "help",
            .r => null,
            .preprocessor => "preprocessor",
            .preprocessor_arg => "preprocessor-arg",
            .use_temp_file => "use-temp-file",
            .no_use_temp_file => "no-use-temp-file",
            .version => "version",
            .auto_includes => ":auto-includes",
        };
    }

    fn hasValue(option: Option) bool {
        return switch (option) {
            .input,
            .output,
            .input_format,
            .output_format,
            .target,
            .include_dir,
            .define,
            .undefine,
            .codepage,
            .language,
            .preprocessor,
            .preprocessor_arg,
            .auto_includes,
            => true,
            .verbose,
            .help,
            .use_temp_file,
            .no_use_temp_file,
            .version,
            .r,
            => false,
        };
    }

    /// Only contains options that have a long arg
    const sorted_options_by_long_arg_len_desc = blk: {
        const fields = @typeInfo(Option).@"enum".fields;
        var long_args: [fields.len]Option = undefined;
        var i: usize = 0;
        for (fields) |enum_field| {
            const option = @field(Option, enum_field.name);
            if (option.longArg() == null) continue;
            long_args[i] = option;
            i += 1;
        }
        std.mem.sortUnstable(Option, long_args[0..i], {}, struct {
            fn lessThan(_: void, lhs: Option, rhs: Option) bool {
                return lhs.longArg().?.len > rhs.longArg().?.len;
            }
        }.lessThan);
        const sorted_long_args = (long_args[0..i]).*;
        break :blk sorted_long_args;
    };

    pub fn fromArg(arg: Arg) !OptionAndLength {
        const name = arg.name();
        std.debug.assert(name.len > 0);
        switch (arg.prefix) {
            .long => {
                for (sorted_options_by_long_arg_len_desc) |option| {
                    const long_arg = option.longArg().?;
                    if (std.mem.startsWith(u8, name, long_arg)) {
                        // Anything trailing must start with an equals sign for long options that have a value,
                        // or must be empty for options that don't have a value
                        const rest = name[long_arg.len..];
                        const invalid_value = option.hasValue() and rest.len > 0 and rest[0] != '=';
                        const invalid_trailing = !option.hasValue() and rest.len > 0;
                        if (invalid_value or invalid_trailing) {
                            return error.InvalidOption;
                        }
                        return .{
                            .option = option,
                            .length = long_arg.len,
                        };
                    }
                }
                return error.InvalidOption;
            },
            .short => {
                const option: Option = switch (name[0]) {
                    'i' => .input,
                    'o' => .output,
                    'J' => .input_format,
                    'O' => .output_format,
                    'F' => .target,
                    'I' => .include_dir,
                    'D' => .define,
                    'U' => .undefine,
                    'v' => .verbose,
                    'c' => .codepage,
                    'l' => .language,
                    'h' => .help,
                    'r' => .r,
                    'V' => .version,
                    else => return error.InvalidOption,
                };
                return .{
                    .option = option,
                    .length = 1,
                };
            },
            .slash => unreachable,
        }
    }
};
