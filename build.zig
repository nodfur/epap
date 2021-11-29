const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const freetype = b.addStaticLibrary("freetype", null);
    freetype.linkSystemLibrary("c");
    freetype.addIncludeDir("vendor/freetype/include");
    freetype.addIncludeDir("vendor/freetype/src");
    freetype.addIncludeDir(".");
    freetype.addCSourceFile("freetype.c", &.{"-fno-sanitize=undefined"});

    const harfbuzz = b.addStaticLibrary("harfbuzz", null);
    harfbuzz.linkSystemLibrary("c");
    harfbuzz.linkSystemLibrary("c++");
    harfbuzz.addIncludeDir("vendor/freetype/include");
    harfbuzz.addIncludeDir("vendor/harfbuzz/src");
    harfbuzz.addCSourceFile(
        "vendor/harfbuzz/src/harfbuzz.cc",
        &.{ "-DHAVE_FREETYPE", "-fno-sanitize=undefined" },
    );

    const epap = b.addExecutable("epap", "epap.zig");
    const epap_ft = b.addExecutable("epap-ft", "freetype.zig");

    freetype.setTarget(target);
    harfbuzz.setTarget(target);
    epap.setTarget(target);
    epap_ft.setTarget(target);

    epap.addIncludeDir("vendor/bcm2835-1.70/src");
    epap.addCSourceFile("vendor/bcm2835-1.70/src/bcm2835.c", &.{"-fno-sanitize=undefined"});

    epap.addIncludeDir("vendor/freetype/include");
    epap.addIncludeDir("vendor/harfbuzz/src");
    epap.linkLibrary(freetype);
    epap.linkLibrary(harfbuzz);

    epap.linkSystemLibrary("c");
    epap.setBuildMode(mode);
    epap.install();

    epap_ft.addIncludeDir("vendor/freetype/include");
    epap_ft.addIncludeDir("vendor/harfbuzz/src");
    epap_ft.linkLibrary(freetype);
    epap_ft.linkLibrary(harfbuzz);

    epap_ft.linkSystemLibrary("c");
    epap_ft.setBuildMode(mode);
    epap_ft.install();

    freetype.install();
    harfbuzz.install();
}
