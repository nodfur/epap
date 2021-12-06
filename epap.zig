const std = @import("std");
const c = @import("./c.zig");

extern fn epap_set_rawio_capability() u32 {
    var caps = c.cap_get_proc();
    if (caps == null) {
        return 1;
    }

    if (c.cap_set_flag(caps, c.CAP_EFFECTIVE, 1, .{c.CAP_SYS_RAWIO}, c.CAP_SET) == -1) {
        return 2;
    }

    if (c.cap_set_proc(caps) == -1) {
        return 3;
    }

    if (c.cap_free(caps) == -1) {
        return 4;
    }

    return 0;
}
