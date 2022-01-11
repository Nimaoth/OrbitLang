const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    var mode = b.standardReleaseOptions();

    const use_fibers = b.option(bool, "use_fibers", "If true use fibers as ") orelse false;

    const exe = b.addExecutable("OrbitLang", "src/main.zig");
    exe.addBuildOption(bool, "use_fibers", use_fibers);
    exe.addCSourceFile("src/coro.c", &.{});
    exe.linkLibC();
    exe.addIncludeDir("src");
    exe.addPackage(.{ .name = "clap", .path = "../zig-clap-0.3.0/zig-clap-0.3.0/clap.zig" });
    exe.addPackage(.{ .name = "ansi-term", .path = "./deps/ansi-term/src/main.zig" });
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
}
