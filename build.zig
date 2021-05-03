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

    var exe_name = std.ArrayList(u8).init(b.allocator);
    exe_name.writer().writeAll("OrbitLang") catch unreachable;

    const use_fibers = b.option(bool, "use_fibers", "If true use fibers as ") orelse false;
    if (use_fibers) {
        std.log.info("[build] using fibers", .{});
        exe_name.writer().writeAll("_fibers") catch unreachable;
    } else {
        std.log.info("[build] using async", .{});
        exe_name.writer().writeAll("_async") catch unreachable;
    }

    const exe = b.addExecutable(exe_name.items, "src/test.zig");

    exe.addBuildOption(bool, "use_fibers", use_fibers);
    exe.addBuildOption(?u64, "fib_of", b.option(u64, "fib_of", "Calculate Fibonnaci number of this."));
    exe.addBuildOption(?u64, "n_iter", b.option(u64, "n_iter", ""));
    exe.addBuildOption(?u64, "n_coros", b.option(u64, "n_coros", ""));
    exe.addBuildOption(bool, "log", b.option(bool, "log", "") orelse false);
    exe.addBuildOption(bool, "arena", b.option(bool, "arena", "") orelse false);
    exe.addCSourceFile("src/coro.c", &.{});
    exe.linkLibC();
    exe.addIncludeDir("src");
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
