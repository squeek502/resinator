const builtin = @import("builtin");

const std = @import("std");
const Io = std.Io;
const Allocator = std.mem.Allocator;

const removeComments = @import("comments.zig").removeComments;
const parseAndRemoveLineCommands = @import("source_mapping.zig").parseAndRemoveLineCommands;
const compile = @import("compile.zig").compile;
const Dependencies = @import("compile.zig").Dependencies;
const Diagnostics = @import("errors.zig").Diagnostics;
const cli = @import("cli.zig");
const parse = @import("parse.zig");
const lex = @import("lex.zig");
const preprocess = @import("preprocess.zig");
const renderErrorMessageToStderr = @import("utils.zig").renderErrorMessageToStderr;
const auto_includes = @import("auto_includes.zig");
const hasDisjointCodePage = @import("disjoint_code_page.zig").hasDisjointCodePage;
const cvtres = @import("cvtres.zig");
const aro = @import("aro");
const subcommands = @import("subcommands.zig");
const fmtResourceType = @import("res.zig").NameOrOrdinal.fmtResourceType;

pub fn main(init: std.process.Init.Minimal) !void {
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
    defer std.debug.assert(debug_allocator.deinit() == .ok);
    const gpa = debug_allocator.allocator();

    var environ_map = try init.environ.createMap(gpa);
    defer environ_map.deinit();

    var threaded: std.Io.Threaded = .init(gpa, .{
        .environ = init.environ,
        .argv0 = .init(init.args),
    });
    defer threaded.deinit();
    const io = threaded.io();

    var arena_state = std.heap.ArenaAllocator.init(gpa);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    // Set the codepage to UTF-8 unconditionally to ensure that everything renders okay
    // TODO: Reset codepage afterwards?
    if (builtin.os.tag == .windows) {
        _ = std.os.windows.kernel32.SetConsoleOutputCP(65001);
    }

    var stderr_buf: [512]u8 = undefined;

    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = Io.File.stdout().writer(io, &stdout_buffer);
    const stdout = &stdout_writer.interface;

    var options = options: {
        const all_args = try init.args.toSlice(arena);
        const args = all_args[1..]; // skip past the executable name

        if (args.len > 0 and std.mem.eql(u8, args[0], "targets")) {
            try subcommands.targets.run(io);
            return;
        } else if (args.len > 0 and std.mem.eql(u8, args[0], "cvtres")) {
            const subcommand_args = args[1..];
            var cli_diagnostics = cli.Diagnostics.init(gpa);
            defer cli_diagnostics.deinit();
            var options = subcommands.cvtres.parseCli(gpa, subcommand_args, &cli_diagnostics) catch |err| switch (err) {
                error.ParseError => {
                    try cli_diagnostics.renderToStderr(io, subcommand_args);
                    std.process.exit(1);
                },
                else => |e| return e,
            };
            errdefer options.deinit();

            // print any warnings/notes
            try cli_diagnostics.renderToStderr(io, subcommand_args);
            // If there was something printed, then add an extra newline separator
            // so that there is a clear separation between the cli diagnostics and whatever
            // gets printed after
            if (cli_diagnostics.errors.items.len > 0) {
                const stderr = try io.lockStderr(&stderr_buf, null);
                defer io.unlockStderr();
                try stderr.file_writer.interface.writeByte('\n');
            }

            if (options.print_help_and_exit) {
                try subcommands.cvtres.writeUsage(stdout, "resinator cvtres");
                try stdout.flush();
                return;
            }

            break :options options;
        } else if (args.len > 0 and std.mem.eql(u8, args[0], "windres")) {
            const subcommand_args = args[1..];
            var cli_diagnostics = cli.Diagnostics.init(gpa);
            defer cli_diagnostics.deinit();
            var options = subcommands.windres.parseCli(gpa, io, subcommand_args, &cli_diagnostics) catch |err| switch (err) {
                error.ParseError => {
                    try cli_diagnostics.renderToStderr(io, subcommand_args);
                    std.process.exit(1);
                },
                else => |e| return e,
            };
            errdefer options.deinit();

            // print any warnings/notes
            try cli_diagnostics.renderToStderr(io, subcommand_args);
            // If there was something printed, then add an extra newline separator
            // so that there is a clear separation between the cli diagnostics and whatever
            // gets printed after
            if (cli_diagnostics.errors.items.len > 0) {
                const stderr = try io.lockStderr(&stderr_buf, null);
                defer io.unlockStderr();
                try stderr.file_writer.interface.writeByte('\n');
            }

            if (options.print_help_and_exit) {
                try subcommands.windres.writeUsage(stdout, "resinator windres");
                try stdout.flush();
                return;
            }
            if (options.print_version_and_exit) {
                // This is only here to trick meson into thinking we're windres
                try stdout.writeAll("Drop-in compatible with GNU windres.\n");
                try stdout.flush();
                return;
            }

            break :options options;
        }

        var cli_diagnostics = cli.Diagnostics.init(gpa);
        defer cli_diagnostics.deinit();
        var options = cli.parse(gpa, io, args, &cli_diagnostics) catch |err| switch (err) {
            error.ParseError => {
                try cli_diagnostics.renderToStderr(io, args);
                std.process.exit(1);
            },
            else => |e| return e,
        };
        errdefer options.deinit();
        try options.maybeAppendRC(io, .cwd());

        // print any warnings/notes
        try cli_diagnostics.renderToStderr(io, args);
        // If there was something printed, then add an extra newline separator
        // so that there is a clear separation between the cli diagnostics and whatever
        // gets printed after
        if (cli_diagnostics.errors.items.len > 0) {
            const stderr = try io.lockStderr(&stderr_buf, null);
            defer io.unlockStderr();
            try stderr.file_writer.interface.writeByte('\n');
        }
        break :options options;
    };
    defer options.deinit();

    if (options.print_help_and_exit) {
        try cli.writeUsage(stdout, "zig rc");
        try stdout.flush();
        return;
    }

    if (options.verbose) {
        try options.dumpVerbose(stdout);
        try stdout.writeByte('\n');
        try stdout.flush();
    }

    var dependencies = Dependencies.init(gpa);
    defer dependencies.deinit();
    const maybe_dependencies: ?*Dependencies = if (options.depfile_path != null) &dependencies else null;

    const include_paths = getIncludePaths(arena, io, &environ_map, options.auto_includes) catch |err| switch (err) {
        error.OutOfMemory => |e| return e,
        else => {
            switch (err) {
                error.MsvcIncludesNotFound => {
                    try renderErrorMessageToStderr(io, .err, "MSVC include paths could not be automatically detected", .{});
                },
                error.CannotResolveCachePath => {
                    try renderErrorMessageToStderr(io, .err, "could not resolve global cache path (for MinGW auto includes)", .{});
                },
                // All other errors are related to MinGW includes
                else => {
                    try renderErrorMessageToStderr(io, .err, "failed to find / extract cached MinGW includes: {s}", .{@errorName(err)});
                },
            }
            try renderErrorMessageToStderr(io, .note, "to disable auto includes, use the option /:auto-includes none", .{});
            std.process.exit(1);
        },
    };

    const full_input = full_input: {
        if (options.input_format == .rc and options.preprocess != .no) {
            var preprocessed_buf: std.Io.Writer.Allocating = .init(gpa);
            errdefer preprocessed_buf.deinit();

            // We're going to throw away everything except the final preprocessed output anyway,
            // so we can use a scoped arena for everything else.
            var aro_arena_state = std.heap.ArenaAllocator.init(gpa);
            defer aro_arena_state.deinit();
            const aro_arena = aro_arena_state.allocator();

            var stderr = try io.lockStderr(&stderr_buf, null);
            defer io.unlockStderr();
            var diagnostics: aro.Diagnostics = .{ .output = .{
                .to_writer = stderr.terminal(),
            } };
            defer diagnostics.deinit();

            var comp = aro.Compilation.init(aro_arena, aro_arena, io, &diagnostics, Io.Dir.cwd());
            defer comp.deinit();

            var argv: std.ArrayList([]const u8) = .empty;
            defer argv.deinit(aro_arena);

            try argv.append(aro_arena, "arocc"); // dummy command name
            try preprocess.appendAroArgs(aro_arena, &argv, options, include_paths, environ_map.get("INCLUDE"));
            try argv.append(aro_arena, switch (options.input_source) {
                .stdio => "-",
                .filename => |filename| filename,
            });

            if (options.verbose) {
                try stdout.writeAll("Preprocessor: arocc (built-in)\n");
                for (argv.items[0 .. argv.items.len - 1]) |arg| {
                    try stdout.print("{s} ", .{arg});
                }
                try stdout.print("{s}\n\n", .{argv.items[argv.items.len - 1]});
                try stdout.flush();
            }

            preprocess.preprocess(&comp, &preprocessed_buf.writer, argv.items, maybe_dependencies) catch |err| switch (err) {
                error.GeneratedSourceError => {
                    try renderErrorMessageToStderr(io, .err, "failed during preprocessor setup (this is always a bug)", .{});
                    std.process.exit(1);
                },
                // ArgError can occur if e.g. the .rc file is not found
                error.ArgError, error.PreprocessError => {
                    try renderErrorMessageToStderr(io, .err, "failed during preprocessing", .{});
                    std.process.exit(1);
                },
                error.FileTooBig => {
                    try renderErrorMessageToStderr(io, .err, "failed during preprocessing: maximum file size exceeded", .{});
                    std.process.exit(1);
                },
                error.WriteFailed => {
                    try renderErrorMessageToStderr(io, .err, "failed during preprocessing: error writing the preprocessed output", .{});
                    std.process.exit(1);
                },
                error.OutOfMemory => |e| return e,
            };

            break :full_input try preprocessed_buf.toOwnedSlice();
        } else {
            switch (options.input_source) {
                .stdio => |file| {
                    var file_reader = file.reader(io, &.{});
                    break :full_input file_reader.interface.allocRemaining(gpa, .unlimited) catch |err| {
                        try renderErrorMessageToStderr(io, .err, "unable to read input from stdin: {t}", .{file_reader.err orelse err});
                        std.process.exit(1);
                    };
                },
                .filename => |input_filename| {
                    break :full_input Io.Dir.cwd().readFileAlloc(io, input_filename, gpa, .unlimited) catch |err| {
                        try renderErrorMessageToStderr(io, .err, "unable to read input file path '{s}': {t}", .{ input_filename, err });
                        std.process.exit(1);
                    };
                },
            }
        }
    };
    defer gpa.free(full_input);

    if (options.preprocess == .only) {
        switch (options.output_source) {
            .stdio => |output_file| {
                try output_file.writeStreamingAll(io, full_input);
            },
            .filename => |output_filename| {
                try Io.Dir.cwd().writeFile(io, .{ .sub_path = output_filename, .data = full_input });
            },
        }
        return;
    }

    var resources = resources: {
        const need_intermediate_res = options.output_format == .coff and options.input_format != .res;
        var res_stream = if (need_intermediate_res)
            IoStream{
                .name = "<in-memory intermediate res>",
                .intermediate = true,
                .source = .{ .memory = .empty },
            }
        else if (options.input_format == .res)
            IoStream.fromIoSource(io, options.input_source, .input) catch |err| {
                try renderErrorMessageToStderr(io, .err, "unable to read res file path '{s}': {t}", .{ options.input_source.filename, err });
                std.process.exit(1);
            }
        else
            IoStream.fromIoSource(io, options.output_source, .output) catch |err| {
                try renderErrorMessageToStderr(io, .err, "unable to create output file '{s}': {t}", .{ options.output_source.filename, err });
                std.process.exit(1);
            };
        defer res_stream.deinit(gpa, io);

        const res_data = res_data: {
            if (options.input_format != .res) {
                // Note: We still want to run this when no-preprocess is set because:
                //   1. We want to print accurate line numbers after removing multiline comments
                //   2. We want to be able to handle an already-preprocessed input with #line commands in it
                var mapping_results = parseAndRemoveLineCommands(gpa, full_input, full_input, .{ .initial_filename = options.input_source.filename }) catch |err| switch (err) {
                    error.InvalidLineCommand => {
                        // TODO: Maybe output the invalid line command
                        try renderErrorMessageToStderr(io, .err, "invalid line command in the preprocessed source", .{});
                        if (options.preprocess == .no) {
                            try renderErrorMessageToStderr(io, .note, "line commands must be of the format: #line <num> \"<path>\"", .{});
                        } else {
                            try renderErrorMessageToStderr(io, .note, "this is likely to be a bug, please report it", .{});
                        }
                        std.process.exit(1);
                    },
                    error.LineNumberOverflow => {
                        // TODO: Better error message
                        try renderErrorMessageToStderr(io, .err, "line number count exceeded maximum of {}", .{std.math.maxInt(usize)});
                        std.process.exit(1);
                    },
                    error.OutOfMemory => |e| return e,
                };
                defer mapping_results.mappings.deinit(gpa);

                const default_code_page = options.default_code_page orelse .windows1252;
                const has_disjoint_code_page = hasDisjointCodePage(mapping_results.result, &mapping_results.mappings, default_code_page);

                const final_input = try removeComments(mapping_results.result, mapping_results.result, &mapping_results.mappings);

                var diagnostics = Diagnostics.init(gpa);
                defer diagnostics.deinit();

                if (options.debug) {
                    std.debug.print("disjoint code page detected: {}\n", .{has_disjoint_code_page});
                    std.debug.print("after preprocessor:\n------------------\n{s}\n------------------\n", .{final_input});
                    std.debug.print("\nmappings:\n", .{});
                    var it = mapping_results.mappings.sources.inorderIterator();
                    while (it.next()) |node| {
                        const source = node.key;
                        const filename = mapping_results.mappings.files.get(source.filename_offset);
                        std.debug.print("{}: {s} : {}-{}\n", .{ source.start_line, filename, source.corresponding_start_line, source.corresponding_start_line + source.span });
                    }
                    std.debug.print("end line #: {}\n", .{mapping_results.mappings.end_line});
                    std.debug.print("\n", .{});

                    // Separately parse and dump the AST
                    ast: {
                        var parse_diagnostics = Diagnostics.init(gpa);
                        defer parse_diagnostics.deinit();
                        var lexer = lex.Lexer.init(final_input, .{});
                        var parser = parse.Parser.init(&lexer, .{});
                        var tree = parser.parse(gpa, &parse_diagnostics) catch {
                            std.debug.print("Failed to parse\n", .{});
                            break :ast;
                        };
                        defer tree.deinit();

                        var stderr = try io.lockStderr(&stderr_buf, null);
                        defer io.unlockStderr();
                        const t = stderr.terminal();
                        try tree.dump(t.writer);
                        try t.writer.writeByte('\n');
                    }
                }

                var output_buffer: [4096]u8 = undefined;
                var res_stream_writer = res_stream.source.writer(gpa, io, &output_buffer);
                defer res_stream_writer.deinit(&res_stream.source);
                const output_buffered_stream = res_stream_writer.interface();

                compile(gpa, io, final_input, output_buffered_stream, .{
                    .cwd = Io.Dir.cwd(),
                    .diagnostics = &diagnostics,
                    .source_mappings = &mapping_results.mappings,
                    .dependencies = maybe_dependencies,
                    .ignore_include_env_var = options.ignore_include_env_var,
                    .extra_include_paths = options.extra_include_paths.items,
                    .system_include_paths = include_paths,
                    .default_language_id = options.default_language_id,
                    .default_code_page = default_code_page,
                    .disjoint_code_page = has_disjoint_code_page,
                    .verbose = options.verbose,
                    .null_terminate_string_table_strings = options.null_terminate_string_table_strings,
                    .max_string_literal_codepoints = options.max_string_literal_codepoints,
                    .silent_duplicate_control_ids = options.silent_duplicate_control_ids,
                    .warn_instead_of_error_on_invalid_code_page = options.warn_instead_of_error_on_invalid_code_page,
                    .include_env_value = environ_map.get("INCLUDE"),
                }) catch |err| switch (err) {
                    error.ParseError, error.CompileError => {
                        // Delete the output file on error
                        res_stream.cleanupAfterError(io);
                        try diagnostics.renderToStderr(io, .cwd(), final_input, mapping_results.mappings);
                        std.process.exit(1);
                    },
                    else => |e| return e,
                };

                try output_buffered_stream.flush();

                if (options.debug) {
                    std.debug.print("dependencies list:\n", .{});
                    for (dependencies.list.items) |path| {
                        std.debug.print(" {s}\n", .{path});
                    }
                    std.debug.print("\n", .{});
                }

                // print any warnings/notes
                try diagnostics.renderToStderr(io, .cwd(), final_input, mapping_results.mappings);

                // write the depfile
                if (options.depfile_path) |depfile_path| {
                    var depfile = Io.Dir.cwd().createFile(io, depfile_path, .{}) catch |err| {
                        try renderErrorMessageToStderr(io, .err, "unable to create depfile '{s}': {t}", .{ depfile_path, err });
                        std.process.exit(1);
                    };
                    defer depfile.close(io);

                    var depfile_buffer: [1024]u8 = undefined;
                    var depfile_writer = depfile.writer(io, &depfile_buffer);
                    switch (options.depfile_fmt) {
                        .json => {
                            var write_stream: std.json.Stringify = .{
                                .writer = &depfile_writer.interface,
                                .options = .{ .whitespace = .indent_2 },
                            };

                            try write_stream.beginArray();
                            for (dependencies.list.items) |dep_path| {
                                try write_stream.write(dep_path);
                            }
                            try write_stream.endArray();
                        },
                    }
                    try depfile_writer.interface.flush();
                }
            }

            if (options.output_format != .coff) return;

            break :res_data res_stream.source.readAll(gpa, io) catch |err| {
                try renderErrorMessageToStderr(io, .err, "unable to read res from '{s}': {t}", .{ res_stream.name, err });
                std.process.exit(1);
            };
        };
        // No need to keep the res_data around after parsing the resources from it
        defer res_data.deinit(gpa);

        std.debug.assert(options.output_format == .coff);

        // TODO: Maybe use a buffered file reader instead of reading file into memory -> fbs
        var res_reader: std.Io.Reader = .fixed(res_data.bytes);
        break :resources cvtres.parseRes(gpa, &res_reader, .{ .max_size = res_data.bytes.len }) catch |err| {
            // TODO: Better errors
            try renderErrorMessageToStderr(io, .err, "unable to parse res from '{s}': {t}", .{ res_stream.name, err });
            std.process.exit(1);
        };
    };
    defer resources.deinit();

    for (options.additional_inputs.items) |additional_res| {
        const additional_file = Io.Dir.cwd().openFile(io, additional_res, .{ .allow_directory = false }) catch |err| {
            try renderErrorMessageToStderr(io, .err, "unable to read res file path '{s}': {t}", .{ additional_res, err });
            std.process.exit(1);
        };
        defer additional_file.close(io);

        const file_len = additional_file.length(io) catch |err| {
            try renderErrorMessageToStderr(io, .err, "unable to determine size of res file '{s}': {t}", .{ additional_res, err });
            std.process.exit(1);
        };

        var buf: [256]u8 = undefined;
        var reader = additional_file.reader(io, &buf);
        cvtres.parseResInto(&resources, &reader.interface, .{ .max_size = file_len }) catch |err| {
            // TODO: Better errors
            try renderErrorMessageToStderr(io, .err, "unable to parse res file '{s}': {t}", .{ additional_res, err });
            std.process.exit(1);
        };
    }

    var coff_stream = IoStream.fromIoSource(io, options.output_source, .output) catch |err| {
        try renderErrorMessageToStderr(io, .err, "unable to create output file '{s}': {t}", .{ options.output_source.filename, err });
        std.process.exit(1);
    };
    defer coff_stream.deinit(gpa, io);

    var coff_output_buffer: [4096]u8 = undefined;
    var coff_output_buffered_stream = coff_stream.source.writer(gpa, io, &coff_output_buffer);

    var cvtres_diagnostics: cvtres.Diagnostics = .{ .none = {} };
    cvtres.writeCoff(gpa, coff_output_buffered_stream.interface(), resources.list.items, options.coff_options, &cvtres_diagnostics) catch |err| {
        switch (err) {
            error.DuplicateResource => {
                const duplicate_resource = resources.list.items[cvtres_diagnostics.duplicate_resource];
                try renderErrorMessageToStderr(io, .err, "duplicate resource [id: {f}, type: {f}, language: {f}]", .{
                    duplicate_resource.name_value,
                    fmtResourceType(duplicate_resource.type_value),
                    duplicate_resource.language,
                });
            },
            error.ResourceDataTooLong => {
                const overflow_resource = resources.list.items[cvtres_diagnostics.duplicate_resource];
                try renderErrorMessageToStderr(io, .err, "resource has a data length that is too large to be written into a coff section", .{});
                try renderErrorMessageToStderr(io, .note, "the resource with the invalid size is [id: {f}, type: {f}, language: {f}]", .{
                    overflow_resource.name_value,
                    fmtResourceType(overflow_resource.type_value),
                    overflow_resource.language,
                });
            },
            error.TotalResourceDataTooLong => {
                const overflow_resource = resources.list.items[cvtres_diagnostics.duplicate_resource];
                try renderErrorMessageToStderr(io, .err, "total resource data exceeds the maximum of the coff 'size of raw data' field", .{});
                try renderErrorMessageToStderr(io, .note, "size overflow occurred when attempting to write this resource: [id: {f}, type: {f}, language: {f}]", .{
                    overflow_resource.name_value,
                    fmtResourceType(overflow_resource.type_value),
                    overflow_resource.language,
                });
            },
            else => {
                try renderErrorMessageToStderr(io, .err, "unable to write coff output file '{s}': {t}", .{ coff_stream.name, coff_output_buffered_stream.err() orelse err });
            },
        }
        // Delete the output file on error
        coff_stream.cleanupAfterError(io);
        std.process.exit(1);
    };

    try coff_output_buffered_stream.interface().flush();
}

const IoStream = struct {
    name: []const u8,
    intermediate: bool,
    source: Source,

    pub const IoDirection = enum { input, output };

    pub fn fromIoSource(io: Io, source: cli.Options.IoSource, io_direction: IoDirection) !IoStream {
        return .{
            .name = switch (source) {
                .filename => |filename| filename,
                .stdio => switch (io_direction) {
                    .input => "<stdin>",
                    .output => "<stdout>",
                },
            },
            .intermediate = false,
            .source = try Source.fromIoSource(io, source, io_direction),
        };
    }

    pub fn deinit(self: *IoStream, allocator: Allocator, io: Io) void {
        self.source.deinit(allocator, io);
    }

    pub fn cleanupAfterError(self: *IoStream, io: Io) void {
        switch (self.source) {
            .file => |file| {
                // Delete the output file on error
                file.close(io);
                // Failing to delete is not really a big deal, so swallow any errors
                Io.Dir.cwd().deleteFile(io, self.name) catch {};
            },
            .stdio, .memory, .closed => return,
        }
    }

    pub const Source = union(enum) {
        file: Io.File,
        stdio: Io.File,
        memory: std.ArrayList(u8),
        /// The source has been closed and any usage of the Source in this state is illegal (except deinit).
        closed: void,

        pub fn fromIoSource(io: Io, source: cli.Options.IoSource, io_direction: IoDirection) !Source {
            switch (source) {
                .filename => |filename| return .{
                    .file = switch (io_direction) {
                        .input => try Io.Dir.cwd().openFile(io, filename, .{ .allow_directory = false }),
                        .output => try Io.Dir.cwd().createFile(io, filename, .{}),
                    },
                },
                .stdio => |file| return .{ .stdio = file },
            }
        }

        pub fn deinit(self: *Source, allocator: Allocator, io: Io) void {
            switch (self.*) {
                .file => |file| file.close(io),
                .stdio => {},
                .memory => |*list| list.deinit(allocator),
                .closed => {},
            }
        }

        pub const Data = struct {
            bytes: []const u8,
            needs_free: bool,

            pub fn deinit(self: Data, allocator: Allocator) void {
                if (self.needs_free) {
                    allocator.free(self.bytes);
                }
            }
        };

        pub fn readAll(self: Source, allocator: Allocator, io: Io) !Data {
            return switch (self) {
                inline .file, .stdio => |file| .{
                    .bytes = b: {
                        var file_reader = file.reader(io, &.{});
                        break :b try file_reader.interface.allocRemaining(allocator, .unlimited);
                    },
                    .needs_free = true,
                },
                .memory => |list| .{ .bytes = list.items, .needs_free = false },
                .closed => unreachable,
            };
        }

        pub const Writer = union(enum) {
            file: Io.File.Writer,
            allocating: std.Io.Writer.Allocating,

            pub const Error = Allocator.Error || Io.File.WriteError;

            pub fn interface(this: *@This()) *std.Io.Writer {
                return switch (this.*) {
                    .file => |*fw| &fw.interface,
                    .allocating => |*a| &a.writer,
                };
            }

            pub fn deinit(this: *@This(), source: *Source) void {
                switch (this.*) {
                    .file => {},
                    .allocating => |*a| source.memory = a.toArrayList(),
                }
                this.* = undefined;
            }

            pub fn err(this: *const @This()) ?Io.File.Writer.Error {
                return switch (this.*) {
                    .file => |*fw| fw.err,
                    .allocating => null,
                };
            }
        };

        pub fn writer(source: *Source, allocator: Allocator, io: Io, buffer: []u8) Writer {
            return switch (source.*) {
                .file, .stdio => |file| .{ .file = file.writer(io, buffer) },
                .memory => |*list| .{ .allocating = .fromArrayList(allocator, list) },
                .closed => unreachable,
            };
        }
    };
};

fn getIncludePaths(allocator: std.mem.Allocator, io: Io, env_map: *std.process.Environ.Map, auto_includes_option: cli.Options.AutoIncludes) ![]const []const u8 {
    var includes = auto_includes_option;
    if (builtin.target.os.tag != .windows) {
        switch (includes) {
            // MSVC can't be found when the host isn't Windows, so short-circuit.
            .msvc => return error.MsvcIncludesNotFound,
            // Skip straight to gnu since we won't be able to detect MSVC on non-Windows hosts.
            .any => includes = .gnu,
            .none, .gnu => {},
        }
    }

    while (true) {
        switch (includes) {
            .none => return &[_][]const u8{},
            .any, .msvc => {
                // MSVC is only detectable on Windows targets. This unreachable is to signify
                // that .any and .msvc should be dealt with on non-Windows targets before this point,
                // since getting MSVC include paths uses Windows-only APIs.
                if (builtin.target.os.tag != .windows) unreachable;
                return auto_includes.getMsvcIncludePaths(allocator, io) catch |err| switch (err) {
                    error.OutOfMemory => |e| return e,
                    error.MsvcIncludesNotFound => {
                        if (includes == .any) {
                            // fall back to MinGW
                            includes = .gnu;
                            continue;
                        }
                        return err;
                    },
                };
            },
            .gnu => {
                const include_path = include_path: {
                    const root_node = std.Progress.start(io, .{
                        .root_name = "auto includes",
                    });
                    break :include_path try auto_includes.extractMingwIncludes(allocator, io, env_map, root_node);
                };
                errdefer allocator.free(include_path);

                var include_paths = try allocator.alloc([]const u8, 1);
                include_paths[0] = include_path;
                return include_paths;
            },
        }
    }
}
