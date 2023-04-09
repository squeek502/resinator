const std = @import("std");
const resinator = @import("resinator");

pub const log_level: std.log.Level = .warn;

pub export fn main() void {
    zigMain() catch unreachable;
}

pub fn zigMain() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == false);
    const allocator = gpa.allocator();

    const stdin = std.io.getStdIn();
    var data = try stdin.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(data);

    const dummy_filename = "fuzz.rc";

    var mapping_results = try resinator.source_mapping.parseAndRemoveLineCommands(allocator, data, data, .{ .initial_filename = dummy_filename });
    defer mapping_results.mappings.deinit(allocator);

    // Set the root file
    const root_filename_offset = mapping_results.mappings.files.getOffset(dummy_filename).?;
    mapping_results.mappings.root_filename_offset = root_filename_offset;

    var final_input = resinator.comments.removeComments(mapping_results.result, mapping_results.result, &mapping_results.mappings);

    var diagnostics = resinator.errors.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var output_buf = std.ArrayList(u8).init(allocator);
    defer output_buf.deinit();

    resinator.compile.compile(allocator, final_input, output_buf.writer(), .{
        .cwd = std.fs.cwd(),
        .diagnostics = &diagnostics,
        .source_mappings = &mapping_results.mappings,
        .ignore_include_env_var = true,
        // TODO: Randomize this?
        //.default_language_id = options.default_language_id,
        // TODO: Randomize this
        //.default_code_page = options.default_code_page orelse .windows1252,
        // TODO: Randomize this?
        //.null_terminate_string_table_strings = options.null_terminate_string_table_strings,
        // TODO: Randomize this?
        //.max_string_literal_codepoints = options.max_string_literal_codepoints,
        // TODO: Randomize this?
        //.warn_instead_of_error_on_invalid_code_page = options.warn_instead_of_error_on_invalid_code_page,
    }) catch |err| switch (err) {
        error.ParseError, error.CompileError => {
            diagnostics.renderToStdErr(std.fs.cwd(), final_input, mapping_results.mappings);
            return;
        },
        else => |e| return e,
    };

    // print any warnings/notes
    diagnostics.renderToStdErr(std.fs.cwd(), final_input, mapping_results.mappings);
}
