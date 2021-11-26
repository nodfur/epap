const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const freetype = b.addSharedLibrary("freetype", null, .unversioned);
    freetype.linkSystemLibrary("c");
    freetype.addIncludeDir("vendor/freetype/include");
    freetype.addIncludeDir("vendor/freetype/src");
    freetype.addIncludeDir(".");
    freetype.addCSourceFile("freetype.c", &.{});

    const harfbuzz = b.addSharedLibrary("harfbuzz", null, .unversioned);
    harfbuzz.linkSystemLibrary("c");
    harfbuzz.linkSystemLibrary("c++");
    harfbuzz.addIncludeDir("vendor/harfbuzz/src");
    harfbuzz.addCSourceFile("vendor/harfbuzz/src/harfbuzz.cc", &.{});

    const exe = b.addExecutable("epap", "epap.zig");

    exe.addIncludeDir("vendor/bcm2835-1.70/src");
    exe.addCSourceFile("vendor/bcm2835-1.70/src/bcm2835.c", &.{"-fno-sanitize=undefined"});
    exe.linkLibrary(freetype);
    exe.linkLibrary(harfbuzz);
    exe.linkSystemLibrary("c");
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
