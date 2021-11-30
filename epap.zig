const std = @import("std");
const epd = @import("./epd.zig");
const text = @import("./freetype.zig");

const c_allocator = std.heap.c_allocator;

fn readCharacterFromStdin() !u8 {
    var reader = std.io.getStdIn().reader();
    return reader.readByte();
}

const term = @cImport({
    @cInclude("termios.h");
    @cInclude("unistd.h");
    @cInclude("stdlib.h");
});

var orig_termios: term.termios = undefined;

pub fn enableRawMode() void {
    _ = term.tcgetattr(term.STDIN_FILENO, &orig_termios);
    _ = term.atexit(disableRawMode);

    var raw: term.termios = undefined;
    raw.c_lflag &= ~(@as(u8, term.ECHO) | @as(u8, term.ICANON));

    _ = term.tcsetattr(term.STDIN_FILENO, term.TCSAFLUSH, &raw);
}

pub fn disableRawMode() callconv(.C) void {
    _ = term.tcsetattr(term.STDIN_FILENO, term.TCSAFLUSH, &orig_termios);
}

pub fn main() !void {
    try epd.initializeBroadcomChip();
    defer {
        epd.finalizeBroadcomChip() catch |err| std.log.err("BCM2835 exit failed", .{});
    }

    try text.init();

    var fontPath = "fonts/DMMono-Regular.ttf";
    var fontHeight: u32 = 36;

    var info = try epd.initializeDisplay(-1.73);

    var height = fontHeight * 2;

    std.log.info("allocating full-screen bitmap", .{});

    var frame: []u8 =
        try std.heap.c_allocator.alloc(u8, info.panelHeight * @as(u32, info.panelWidth) / 8);

    defer std.heap.c_allocator.free(frame);

    std.log.info("setting bitmap to 0xf", .{});

    std.mem.set(u8, frame, 0xff);
    
    try epd.clearScreen(info, 0xff, 0);

    std.log.info("drawing text in A2 mode", .{});

    var font = try text.loadFont(fontPath, fontHeight);
    var rectangle = epd.Rectangle{
        .x = 0,
        .y = 0,
        .w = info.panelWidth,
        .h = @intCast(u16, height),
    };

    var string = try std.heap.c_allocator.alloc(u8, 100);
    defer std.heap.c_allocator.free(string);
    std.mem.set(u8, string, 0);

    var i: usize = 0;

    enableRawMode();
    defer disableRawMode();

    while (true) {
        var c = readCharacterFromStdin() catch |err| break;
        string[i] = c;
        i += 1;
        try text.renderText(0, font, @ptrCast([*:0]u8, string), frame, info.panelWidth, 20, 0);
        try epd.drawBitmap(rectangle, @ptrCast([*]const u8, frame), info.memoryAddress);
    }

    try epd.clearScreen(info, 0xff, 0);
    epd.delayMs(200);

    try epd.sleep();
    try text.done();
}

