const std = @import("std");
const resinator = @import("resinator");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

var tmp_buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
var tmp_dir_len: usize = 0;

// minimalist panic handler that avoids the @breakpoint within
// std.os.abort in .Debug mode, since that seems to cause the program
// to hang for a bit before actually exiting. Also removes the
// error return trace/stack trace printing since that can take some
// time as well (and it's not terribly useful anyway for this particular
// fuzzing setup).
pub fn panic(msg: []const u8, error_return_trace: ?*std.builtin.StackTrace, ret_addr: ?usize) noreturn {
    @setCold(true);
    _ = error_return_trace;
    _ = ret_addr;

    const stderr = std.io.getStdErr().writer();
    stderr.print("panic: ", .{}) catch abort();
    stderr.print("{s}\n", .{msg}) catch abort();
    abort();
}

// Windows-only abort without the @breakpoint in .Debug mode
fn abort() noreturn {
    std.os.windows.kernel32.ExitProcess(3);
}

pub fn main() !void {
    // This is commented out since doing so seemed to make the fuzzer
    // run a bit faster and checking for leaks can be handled by the
    // AFL++ fuzzer instead of the winafl fuzzer.
    // defer std.debug.assert(gpa.deinit() == false);

    // The memory here is intentionally leaked.
    // It seems like if there is a `defer std.process.argsFree` within this
    // function, then when afl-fuzz re-runs fuzzMain the defer seemingly gets called and
    // the usage of input_filepath becomes a use-after-free (this is an assumption
    // on my part but removing the defer did avoid ACCESS_VIOLATION errors when
    // running with afl-fuzz)
    const args = try std.process.argsAlloc(std.heap.page_allocator);

    if (args.len < 2) {
        std.debug.print("need input file path as first arg\n", .{});
        std.os.exit(1);
    }

    {
        var tmp_dir = try std.process.getEnvVarOwned(allocator, "TEMP");
        defer allocator.free(tmp_dir);

        std.mem.copy(u8, &tmp_buf, tmp_dir);
        tmp_dir_len = tmp_dir.len;
        tmp_buf[tmp_dir_len] = std.fs.path.sep;
    }

    fuzzMain(args[1].ptr, args[1].len);
}

// This is the function that will be continually called by winafl
pub export fn fuzzMain(input_filename_ptr: [*]const u8, len: usize) void {
    fuzz(input_filename_ptr[0..len]) catch unreachable;
}

pub fn fuzz(input_filepath: []const u8) !void {
    var resinator_result = try getResinatorResult(input_filepath);
    defer resinator_result.deinit();

    var win32_result = try getWin32Result(input_filepath);
    defer win32_result.deinit();

    try compare(&win32_result, &resinator_result);
}

pub fn compare(win32_result: *Win32Result, resinator_result: *ResinatorResult) !void {
    // The preprocessor may reject things that the win32 compiler does not.
    // This is not something we can do anything about unless we write our own compliant
    // preprocessor, so instead we just treat it as okay and move on.
    if (resinator_result.didPreproccessorError()) {
        return;
    }
    // Both erroring is fine
    if (win32_result.res == null and resinator_result.res == null) {
        return;
    }

    const source = resinator_result.preprocessed.?;

    if (win32_result.res == null and resinator_result.res != null) {
        if (resinator_result.diagnostics.containsAny(&.{
            .rc_would_error_on_bitmap_version,
            .rc_would_error_on_icon_dir,
        })) {
            return;
        }
        return error.ExpectedErrorButDidntGetOne;
    }
    if (win32_result.res != null and resinator_result.res == null) {
        if (resinator_result.diagnostics.containsAny(&.{
            .illegal_byte_order_mark,
            .illegal_private_use_character,
            .illegal_byte_outside_string_literals,
            .illegal_byte,
            .close_paren_expression,
        })) {
            return;
        }
        resinator_result.diagnostics.renderToStdErr(std.fs.cwd(), source, null);
        return error.DidNotExpectErrorButGotOne;
    }

    std.testing.expectEqualSlices(u8, win32_result.res.?, resinator_result.res.?) catch |err| {
        if (resinator_result.diagnostics.containsAny(&.{
            .rc_would_miscompile_version_value_padding,
            .rc_would_miscompile_control_padding,
            .rc_would_miscompile_control_class_ordinal,
            .rc_would_miscompile_bmp_palette_padding,
            .rc_would_miscompile_codepoint_byte_swap,
            .rc_would_miscompile_codepoint_skip,
        })) {
            return;
        }
        resinator_result.diagnostics.renderToStdErr(std.fs.cwd(), source, null);
        return err;
    };
}

const ResinatorResult = struct {
    res: ?[]const u8 = null,
    diagnostics: resinator.errors.Diagnostics,
    preprocessor: std.ChildProcess.ExecResult = undefined,
    /// This is a slice of preprocessor.stdout so it doesn't need to be freed separately
    preprocessed: ?[]const u8 = null,

    pub fn deinit(self: *ResinatorResult) void {
        self.diagnostics.deinit();
        if (self.res) |res| {
            allocator.free(res);
        }
        allocator.free(self.preprocessor.stdout);
        allocator.free(self.preprocessor.stderr);
    }

    pub fn didPreproccessorError(self: *const ResinatorResult) bool {
        return self.preprocessor.term != .Exited or self.preprocessor.term.Exited != 0;
    }
};

pub fn getResinatorResult(input_filepath: []const u8) !ResinatorResult {
    var result = ResinatorResult{
        .diagnostics = resinator.errors.Diagnostics.init(allocator),
    };
    errdefer result.deinit();

    var data = try std.fs.cwd().readFileAlloc(allocator, input_filepath, std.math.maxInt(usize));
    defer allocator.free(data);

    var argv = std.ArrayList([]const u8).init(allocator);
    defer argv.deinit();

    try argv.appendSlice(&[_][]const u8{
        "clang",
        "-E", // preprocessor only
        "--comments",
        "-fuse-line-directives", // #line <num> instead of # <num>
        // TODO: could use --trace-includes to give info about what's included from where
        "-xc", // output c
        // TODO: Turn this off, check the warnings, and convert the spaces back to NUL
        "-Werror=null-character", // error on null characters instead of converting them to spaces
        // TODO: could remove -Werror=null-character and instead parse warnings looking for 'warning: null character ignored'
        //       since the only real problem is when clang doesn't preserve null characters
        //"-Werror=invalid-pp-token", // will error on unfinished string literals
        // TODO: could use -Werror instead
        // https://learn.microsoft.com/en-us/windows/win32/menurc/predefined-macros
        "-DRC_INVOKED",
    });
    try argv.append(input_filepath);

    result.preprocessor = try std.ChildProcess.exec(.{
        .allocator = allocator,
        .argv = argv.items,
        .max_output_bytes = std.math.maxInt(u32),
    });
    errdefer allocator.free(result.preprocessor.stdout);
    errdefer allocator.free(result.preprocessor.stderr);

    if (result.preprocessor.term != .Exited or result.preprocessor.term.Exited != 0) {
        return result;
    }

    var mapping_results = try resinator.source_mapping.parseAndRemoveLineCommands(
        allocator,
        result.preprocessor.stdout,
        result.preprocessor.stdout,
        .{ .initial_filename = input_filepath },
    );
    defer mapping_results.mappings.deinit(allocator);

    var final_input = resinator.comments.removeComments(mapping_results.result, mapping_results.result, &mapping_results.mappings);
    result.preprocessed = final_input;

    var output_buf = std.ArrayList(u8).init(allocator);
    errdefer output_buf.deinit();

    const compile_options = resinator.compile.CompileOptions{
        .cwd = std.fs.cwd(),
        .diagnostics = &result.diagnostics,
        .source_mappings = &mapping_results.mappings,
        .ignore_include_env_var = true,
    };

    var did_error = false;
    resinator.compile.compile(allocator, final_input, output_buf.writer(), compile_options) catch |err| switch (err) {
        error.ParseError, error.CompileError => {
            did_error = true;
        },
        else => |e| return e,
    };

    if (!did_error) {
        result.res = try output_buf.toOwnedSlice();
    }
    return result;
}

const Win32Result = struct {
    res: ?[]const u8 = null,
    exec: std.ChildProcess.ExecResult,

    pub fn deinit(self: *Win32Result) void {
        if (self.res) |res| {
            allocator.free(res);
        }
        allocator.free(self.exec.stdout);
        allocator.free(self.exec.stderr);
    }
};

pub fn getWin32Result(input_path: []const u8) !Win32Result {
    const rand_int = std.crypto.random.int(u64);
    const tmp_filename = std.fmt.bufPrint(tmp_buf[tmp_dir_len + 1 ..], "{x}.res", .{rand_int}) catch unreachable;
    const tmp_filepath = tmp_buf[0 .. tmp_dir_len + 1 + tmp_filename.len];
    defer std.fs.cwd().deleteFile(tmp_filepath) catch {};

    var exec_result = try std.ChildProcess.exec(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            // Note: This relies on `rc.exe` being in the PATH
            "rc.exe",
            "/x", // ignore INCLUDE env var
            "/fo",
            tmp_filepath,
            input_path,
        },
    });
    errdefer allocator.free(exec_result.stdout);
    errdefer allocator.free(exec_result.stderr);

    var result = Win32Result{ .exec = exec_result };
    if (exec_result.term == .Exited and exec_result.term.Exited == 0) {
        result.res = std.fs.cwd().readFileAlloc(allocator, tmp_filepath, std.math.maxInt(usize)) catch |err| blk: {
            std.debug.print("expected file at {s} but got: {}", .{ tmp_filepath, err });
            break :blk null;
        };
    }
    return result;
}
