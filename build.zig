const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const bcm2835 = b.addStaticLibrary("bcm2835", null);
    const freetype = b.addStaticLibrary("freetype", null);
    const epap = b.addExecutable("epap", "epap.zig");
    const epap_ft = b.addExecutable("epap-ft", "epap-ft.zig");
    const epap_reset = b.addExecutable("epap-reset", "epap-reset.zig");

    freetype.linkSystemLibrary("c");
    freetype.linkSystemLibrary("c++");
    freetype.addIncludeDir("vendor/freetype/src");
    freetype.addIncludeDir("vendor/harfbuzz/src");
    freetype.addIncludeDir(".");
    freetype.addCSourceFile("freetype.c", &.{"-fno-sanitize=undefined"});
    freetype.addCSourceFile(
        "vendor/harfbuzz/src/harfbuzz.cc",
        &.{ "-DHAVE_FREETYPE", "-fno-sanitize=undefined" },
    );

    bcm2835.linkSystemLibrary("c");
    bcm2835.addCSourceFile("vendor/bcm2835-1.70/src/bcm2835.c", &.{"-fno-sanitize=undefined"});

    bcm2835.addIncludeDir("vendor/bcm2835-1.70/src");
    epap.addIncludeDir("vendor/bcm2835-1.70/src");
    epap_reset.addIncludeDir("vendor/bcm2835-1.70/src");

    freetype.addIncludeDir("vendor/freetype/include");
    epap.addIncludeDir("vendor/freetype/include");
    epap_ft.addIncludeDir("vendor/freetype/include");

    epap.addIncludeDir("vendor/harfbuzz/src");
    epap_ft.addIncludeDir("vendor/harfbuzz/src");

    bcm2835.setTarget(target);
    freetype.setTarget(target);
    epap.setTarget(target);
    epap_ft.setTarget(target);
    epap_reset.setTarget(target);

    bcm2835.setBuildMode(mode);
    freetype.setBuildMode(mode);
    epap.setBuildMode(mode);
    epap_ft.setBuildMode(mode);
    epap_reset.setBuildMode(mode);

    epap.linkLibrary(bcm2835);
    epap_reset.linkLibrary(bcm2835);
    epap.linkLibrary(freetype);

    epap_ft.linkLibrary(freetype);

    bcm2835.install();
    freetype.install();
    epap.install();
    epap_ft.install();
    epap_reset.install();
}
