const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardOptimizeOption(.{});

    const resinator = b.addModule("resinator", .{
        .source_file = .{ .path = "src/resinator.zig" },
    });

    const exe = b.addExecutable(.{
        .name = "resinator",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = mode,
    });
    b.installArtifact(exe);

    const test_filter = b.option([]const u8, "test-filter", "Skip tests that do not match filter");
    const exe_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/resinator.zig" },
        .target = target,
        .optimize = mode,
        .filter = test_filter,
    });
    const run_exe_tests = b.addRunArtifact(exe_tests);

    const reference_tests = b.addTest(.{
        .name = "reference",
        .root_source_file = .{ .path = "test/reference.zig" },
        .target = target,
        .optimize = mode,
        .filter = test_filter,
    });
    reference_tests.addModule("resinator", resinator);
    const run_reference_tests = b.addRunArtifact(reference_tests);

    const parser_tests = b.addTest(.{
        .name = "parse",
        .root_source_file = .{ .path = "test/parse.zig" },
        .target = target,
        .optimize = mode,
        .filter = test_filter,
    });
    parser_tests.addModule("resinator", resinator);
    const run_parser_tests = b.addRunArtifact(parser_tests);

    const compiler_tests = b.addTest(.{
        .name = "compile",
        .root_source_file = .{ .path = "test/compile.zig" },
        .target = target,
        .optimize = mode,
        .filter = test_filter,
    });
    compiler_tests.addModule("resinator", resinator);
    const run_compiler_tests = b.addRunArtifact(compiler_tests);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&run_exe_tests.step);
    test_step.dependOn(&run_reference_tests.step);
    test_step.dependOn(&run_parser_tests.step);
    test_step.dependOn(&run_compiler_tests.step);

    // TODO: coverage across all test steps?
    const coverage = b.option(bool, "test-coverage", "Generate test coverage") orelse false;
    if (coverage) {
        // with kcov
        exe_tests.setExecCmd(&[_]?[]const u8{
            "kcov",
            //"--path-strip-level=3", // any kcov flags can be specified here
            "--include-pattern=resinator",
            "kcov-output", // output dir for kcov
            null, // to get zig to use the --test-cmd-bin flag
        });
    }

    const fuzzy_max_iterations = b.option(u64, "fuzzy-iterations", "The max iterations for fuzzy tests (default: 1000)") orelse 1000;

    const test_options = b.addOptions();
    test_options.addOption(u64, "max_iterations", fuzzy_max_iterations);

    const all_fuzzy_tests_step = b.step("test_fuzzy", "Run all fuzz/property-testing-like tests with a max number of iterations for each");
    _ = addFuzzyTest(b, "numbers", mode, target, resinator, all_fuzzy_tests_step, test_options);
    _ = addFuzzyTest(b, "number_expressions", mode, target, resinator, all_fuzzy_tests_step, test_options);
    _ = addFuzzyTest(b, "ascii_strings", mode, target, resinator, all_fuzzy_tests_step, test_options);
    _ = addFuzzyTest(b, "numeric_types", mode, target, resinator, all_fuzzy_tests_step, test_options);
    _ = addFuzzyTest(b, "common_resource_attributes", mode, target, resinator, all_fuzzy_tests_step, test_options);
    _ = addFuzzyTest(b, "raw_data", mode, target, resinator, all_fuzzy_tests_step, test_options);
    _ = addFuzzyTest(b, "name_or_ordinal", mode, target, resinator, all_fuzzy_tests_step, test_options);
    _ = addFuzzyTest(b, "code_pages", mode, target, resinator, all_fuzzy_tests_step, test_options);
    _ = addFuzzyTest(b, "icons", mode, target, resinator, all_fuzzy_tests_step, test_options);
    _ = addFuzzyTest(b, "bitmaps", mode, target, resinator, all_fuzzy_tests_step, test_options);
    _ = addFuzzyTest(b, "stringtable", mode, target, resinator, all_fuzzy_tests_step, test_options);
    _ = addFuzzyTest(b, "fonts", mode, target, resinator, all_fuzzy_tests_step, test_options);

    _ = addFuzzer(b, "fuzz_rc", &.{}, resinator, target);

    const fuzz_winafl_exe = b.addExecutable(.{
        .name = "fuzz_winafl",
        .root_source_file = .{ .path = "test/fuzz_winafl.zig" },
        .target = target,
        .optimize = mode,
    });
    fuzz_winafl_exe.addModule("resinator", resinator);
    const fuzz_winafl_compile = b.step("fuzz_winafl", "Build/install fuzz_winafl exe");
    const install_fuzz_winafl = b.addInstallArtifact(fuzz_winafl_exe, .{});
    fuzz_winafl_compile.dependOn(&install_fuzz_winafl.step);
}

fn addFuzzyTest(
    b: *std.build.Builder,
    comptime name: []const u8,
    mode: std.builtin.Mode,
    target: std.zig.CrossTarget,
    resinator: *std.build.Module,
    all_fuzzy_tests_step: *std.build.Step,
    fuzzy_options: *std.build.OptionsStep,
) *std.build.LibExeObjStep {
    var test_step = b.addTest(.{
        .root_source_file = .{ .path = "test/fuzzy_" ++ name ++ ".zig" },
        .target = target,
        .optimize = mode,
    });
    test_step.addModule("resinator", resinator);
    test_step.addOptions("fuzzy_options", fuzzy_options);

    const run_test = b.addRunArtifact(test_step);

    var test_run_step = b.step("test_fuzzy_" ++ name, "Some fuzz/property-testing-like tests for " ++ name);
    test_run_step.dependOn(&run_test.step);

    all_fuzzy_tests_step.dependOn(test_run_step);

    return test_step;
}

fn addFuzzer(
    b: *std.build.Builder,
    comptime name: []const u8,
    afl_clang_args: []const []const u8,
    resinator: *std.build.Module,
    target: std.zig.CrossTarget,
) FuzzerSteps {
    // The library
    const fuzz_lib = b.addStaticLibrary(.{
        .name = name ++ "-lib",
        .root_source_file = .{ .path = "test/" ++ name ++ ".zig" },
        .target = target,
        .optimize = .Debug,
    });
    fuzz_lib.addModule("resinator", resinator);
    fuzz_lib.want_lto = true;
    fuzz_lib.bundle_compiler_rt = true;
    fuzz_lib.force_pic = true;

    // Setup the output name
    const fuzz_executable_name = name;
    const fuzz_exe_path = std.fs.path.join(b.allocator, &.{ b.cache_root.path.?, fuzz_executable_name }) catch unreachable;

    // We want `afl-clang-lto -o path/to/output path/to/library`
    const fuzz_compile = b.addSystemCommand(&.{ "afl-clang-lto", "-o", fuzz_exe_path });
    // Add the path to the library file to afl-clang-lto's args
    fuzz_compile.addArtifactArg(fuzz_lib);
    // Custom args
    fuzz_compile.addArgs(afl_clang_args);

    // Install the cached output to the install 'bin' path
    const fuzz_install = b.addInstallBinFile(.{ .path = fuzz_exe_path }, fuzz_executable_name);
    fuzz_install.step.dependOn(&fuzz_compile.step);

    // Add a top-level step that compiles and installs the fuzz executable
    const fuzz_compile_run = b.step(name, "Build executable for fuzz testing '" ++ name ++ "' using afl-clang-lto");
    fuzz_compile_run.dependOn(&fuzz_compile.step);
    fuzz_compile_run.dependOn(&fuzz_install.step);

    // Compile a companion exe for debugging crashes
    const fuzz_debug_exe = b.addExecutable(.{
        .name = name ++ "-debug",
        .root_source_file = .{ .path = "test/" ++ name ++ ".zig" },
        .target = target,
        .optimize = .Debug,
    });
    fuzz_debug_exe.addModule("resinator", resinator);

    // Only install fuzz-debug when the fuzz step is run
    const install_fuzz_debug_exe = b.addInstallArtifact(fuzz_debug_exe, .{});
    fuzz_compile_run.dependOn(&install_fuzz_debug_exe.step);

    return FuzzerSteps{
        .lib = fuzz_lib,
        .debug_exe = fuzz_debug_exe,
    };
}

const FuzzerSteps = struct {
    lib: *std.build.LibExeObjStep,
    debug_exe: *std.build.LibExeObjStep,

    pub fn libExes(self: *const FuzzerSteps) [2]*std.build.LibExeObjStep {
        return [_]*std.build.LibExeObjStep{ self.lib, self.debug_exe };
    }
};
