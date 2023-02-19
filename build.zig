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

    const exe = b.addExecutable(.{
        .name = "resinator",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = mode,
    });
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/resinator.zig" },
        .target = target,
        .optimize = mode,
    });

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&exe_tests.step);

    const try_all_rcs_exe = b.addExecutable(.{
        .name = "try_all_rcs",
        .root_source_file = .{ .path = "test/try_all_rcs.zig" },
        .target = target,
        .optimize = mode,
    });
    const try_all_rcs_compile = b.step("try_all_rcs", "Build/install try_all_rcs exe");
    const install_try_all_rcs_exe = b.addInstallArtifact(try_all_rcs_exe);
    try_all_rcs_compile.dependOn(&install_try_all_rcs_exe.step);

    b.addModule(.{
        .name = "resinator",
        .source_file = .{ .path = "src/resinator.zig" },
    });
    const resinator = b.modules.get("resinator").?;

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
    const test_run_step = b.step("test_fuzzy_" ++ name, "Some fuzz/property-testing-like tests for " ++ name);
    test_run_step.dependOn(&test_step.step);

    all_fuzzy_tests_step.dependOn(&test_step.step);

    return test_step;
}
