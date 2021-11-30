const std = @import("std");
const epd = @import("./epd.zig");
const text = @import("./freetype.zig");

const c_allocator = std.heap.c_allocator;

pub fn main() !void {
    try epd.initializeBroadcomChip();
    defer {
        epd.finalizeBroadcomChip() catch |err| std.log.err("BCM2835 exit failed", .{});
    }

    try text.init();

    var fontPath = "fonts/DMMono-Regular.ttf";
    var fontHeight: u32 = 36;

    var info = try epd.epdInit(-1.73);

    var height = fontHeight * 2;

    std.log.info("allocating full-screen bitmap", .{});

    var frame: []u8 =
        try std.heap.c_allocator.alloc(u8, info.panelHeight * @as(u32, info.panelWidth) / 8);

    defer std.heap.c_allocator.free(frame);

    std.log.info("setting bitmap to 0xf", .{});

    std.mem.set(u8, frame, 0xff);
    
    var font = try text.loadFont(fontPath, fontHeight);

    try epd.epdClear(info, 0xff, 0);

    std.log.info("drawing text in A2 mode", .{});

    var rectangle = epd.Rectangle{
        .x = 0,
        .y = 0,
        .w = info.panelWidth,
        .h = @intCast(u16, height),
    };

    try text.renderText(0, font, "foo", frame, info.panelWidth, 100, 0);
    try epd.epdDrawBitmap(rectangle, @ptrCast([*]const u8, frame), height);
    
    try text.renderText(0, font, "bar", frame, info.panelWidth, 300, 0);
    try epd.epdDrawBitmap(rectangle, @ptrCast([*]const u8, frame), height);

    try text.renderText(0, font, "baz", frame, info.panelWidth, 500, 0);
    try epd.epdDrawBitmap(rectangle, @ptrCast([*]const u8, frame), height);

    epd.delayMs(5000);

    try epd.epdClear(info, 0xff, 0);
    epd.delayMs(200);

    try epd.epdSleep();
    try text.done();
}

