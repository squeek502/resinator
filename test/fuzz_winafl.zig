const std = @import("std");
const utils = @import("utils.zig");

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
    // defer std.debug.assert(gpa.deinit() == .ok);

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
    var resinator_result = try utils.getResinatorResultFromFile(allocator, input_filepath, .{
        .cwd = std.fs.cwd(),
        .cwd_path = "",
        .run_preprocessor = true,
    });
    defer resinator_result.deinit(allocator);

    if (resinator_result.pre.?.known_preprocessor_difference) {
        std.debug.print("known preprocessor difference, skipping\n", .{});
        return;
    }

    var win32_result = try getWin32Result(input_filepath);
    defer win32_result.deinit(allocator);

    try utils.compare(&win32_result, &resinator_result);
}

pub fn getWin32Result(input_path: []const u8) !utils.Win32Result {
    const rand_int = std.crypto.random.int(u64);
    const tmp_filename = std.fmt.bufPrint(tmp_buf[tmp_dir_len + 1 ..], "{x}.res", .{rand_int}) catch unreachable;
    const tmp_filepath = tmp_buf[0 .. tmp_dir_len + 1 + tmp_filename.len];
    defer std.fs.cwd().deleteFile(tmp_filepath) catch {};

    return utils.getWin32ResultFromFile(allocator, input_path, .{
        .cwd = std.fs.cwd(),
        .cwd_path = "",
        .run_preprocessor = true,
        .output_path = tmp_filepath,
    });
}
