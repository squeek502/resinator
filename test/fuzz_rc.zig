const std = @import("std");
const resinator = @import("resinator");

pub const log_level: std.log.Level = .warn;

pub export fn main() void {
    zigMain() catch unreachable;
}

pub fn zigMain() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    const stdin = std.fs.File.stdin();
    const data = try stdin.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(data);

    const dummy_filename = "fuzz.rc";

    var mapping_results = resinator.source_mapping.parseAndRemoveLineCommands(allocator, data, data, .{ .initial_filename = dummy_filename }) catch |err| switch (err) {
        error.InvalidLineCommand => return,
        error.LineNumberOverflow => return,
        else => |e| return e,
    };
    defer mapping_results.mappings.deinit(allocator);

    const final_input = try resinator.comments.removeComments(mapping_results.result, mapping_results.result, &mapping_results.mappings);

    var diagnostics = resinator.errors.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var output_buf: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output_buf.deinit();

    // TODO: Better seed, maybe taking the first few bytes and interpretting as u64
    const prng_seed = data.len;
    var prng = std.Random.DefaultPrng.init(prng_seed);
    const rand = prng.random();

    const stderr_config = std.io.tty.detectConfig(std.io.getStdErr());

    const language_id = rand.int(u16);
    const code_page: resinator.code_pages.SupportedCodePage = if (rand.boolean()) .utf8 else .windows1252;
    const null_terminate_string_table_strings = rand.boolean();
    const max_string_literal_codepoints = rand.int(u15);
    const warn_instead_of_error_on_invalid_code_page = rand.boolean();

    std.debug.print("language_id: 0x{X}\ncode_page: {}\nnull_terminate: {}\nmax_string_literal: {}\nwarn_invalid_code_page: {}\n", .{
        language_id,
        code_page,
        null_terminate_string_table_strings,
        max_string_literal_codepoints,
        warn_instead_of_error_on_invalid_code_page,
    });

    resinator.compile.compile(allocator, final_input, &output_buf.writer, .{
        .cwd = std.fs.cwd(),
        .diagnostics = &diagnostics,
        .source_mappings = &mapping_results.mappings,
        .ignore_include_env_var = true,
        .default_language_id = language_id,
        .default_code_page = code_page,
        .null_terminate_string_table_strings = null_terminate_string_table_strings,
        .max_string_literal_codepoints = max_string_literal_codepoints,
        .warn_instead_of_error_on_invalid_code_page = warn_instead_of_error_on_invalid_code_page,
    }) catch |err| switch (err) {
        error.ParseError, error.CompileError => {
            diagnostics.renderToStdErr(std.fs.cwd(), final_input, stderr_config, mapping_results.mappings);
            return;
        },
        else => |e| return e,
    };

    // print any warnings/notes
    diagnostics.renderToStdErr(std.fs.cwd(), final_input, stderr_config, mapping_results.mappings);
}
