const epd = @import("./epd.zig");
const text = @import("./freetype.zig");

export fn epap_start_broadcom() u32 {
    epd.initializeBroadcomChip() catch |err| return 1;
    return 0;
}

export fn epap_stop_broadcom() u32 {
    epd.finalizeBroadcomChip() catch |err| return 1;
    return 0;
}

export fn epap_start_text() u32 {
    text.init() catch |err| return 1;
    return 0;
}

export fn epap_start_display(vcom: f64, info: *epd.SystemInfo) u32 {
    info.* = epd.initializeDisplay(vcom) catch |err| return 1;
    return 0;
}

export fn epap_sleep() u32 {
    epd.sleep() catch |err| return 1;
    return 0;
}

export fn epap_clear(info: *epd.SystemInfo, byte: u8, mode: u8) u32 {
    epd.clearScreen(info.*, byte, mode) catch |err| return 1;
    return 0;
}

export fn epap_load_font(path: [*:0]const u8, height: u32, font: *text.Font) u32 {
    font.* = text.loadFont(path, height) catch |err| return 1;
    return 0;
}

export fn epap_render_text(
    font: *text.Font,
    string: [*:0]const u8,
    bitmap: [*]u8,
    screen_width: u32,
    x: i32,
    y: i32,
) u32 {
    text.renderText(0, font.*, string, bitmap, screen_width, x, y) catch |err| return 1;
    return 0;
}
