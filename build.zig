const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("resinator", "src/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_tests = b.addTest("src/resinator.zig");
    exe_tests.setTarget(target);
    exe_tests.setBuildMode(mode);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&exe_tests.step);

    const resinator = std.build.Pkg{
        .name = "resinator",
        .source = .{ .path = "src/resinator.zig" },
    };

    var fuzzy_numbers = b.addTest("test/fuzzy_numbers.zig");
    fuzzy_numbers.setBuildMode(mode);
    fuzzy_numbers.setTarget(target);
    fuzzy_numbers.addPackage(resinator);
    const fuzzy_numbers_step = b.step("test_fuzzy_numbers", "Simple fuzz testing for number literals");
    fuzzy_numbers_step.dependOn(&fuzzy_numbers.step);

    var fuzzy_number_expressions = b.addTest("test/fuzzy_number_expressions.zig");
    fuzzy_number_expressions.setBuildMode(mode);
    fuzzy_number_expressions.setTarget(target);
    fuzzy_number_expressions.addPackage(resinator);
    const fuzzy_number_expressions_step = b.step("test_fuzzy_number_expressions", "Simple fuzz testing for number expressions");
    fuzzy_number_expressions_step.dependOn(&fuzzy_number_expressions.step);

    var fuzzy_ascii_strings = b.addTest("test/fuzzy_ascii_strings.zig");
    fuzzy_ascii_strings.setBuildMode(mode);
    fuzzy_ascii_strings.setTarget(target);
    fuzzy_ascii_strings.addPackage(resinator);
    const fuzzy_ascii_strings_step = b.step("test_fuzzy_ascii_strings", "Simple fuzz testing for ascii string literals");
    fuzzy_ascii_strings_step.dependOn(&fuzzy_ascii_strings.step);
}
