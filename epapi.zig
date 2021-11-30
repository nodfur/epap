const epd = @import("./epd.zig");

export fn epap_start_broadcom() u32 {
    epd.initializeBroadcomChip() catch |err| return 1;
    return 0;
}

export fn epap_stop_broadcom() u32 {
    epd.finalizeBroadcomChip() catch |err| return 1;
    return 0;
}
