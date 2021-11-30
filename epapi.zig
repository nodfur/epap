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
