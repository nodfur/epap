const c = @cImport(@cInclude("bcm2835.h"));
const std = @import("std");

const epdRstPin: u8 = 17;
const epdCsPin: u8 = 8;
const epdBusyPin: u8 = 24;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Hello, {s}!\n", .{"world"});

    try init();
}

fn init() !void {
    if (c.bcm2835_init() == 0) {
        return error.BCM2835_InitFailed;
    }

    if (c.bcm2835_spi_begin() == 0) {
        return error.BCM2835_SPI_Failed;
    }

    c.bcm2835_spi_setBitOrder(c.BCM2835_SPI_BIT_ORDER_MSBFIRST);
    c.bcm2835_spi_setDataMode(c.BCM2835_SPI_MODE0);
    c.bcm2835_spi_setClockDivider(c.BCM2835_SPI_CLOCK_DIVIDER_32);

    gpioInit();
}

fn exit() void {
    gpioWriteBit(epdCsPin, c.LOW);
    gpioWriteBit(epdRstPin, c.LOW);

    c.bcm2835_spi_end();
    c.bcm2835_close();
}

fn gpioMode(pin: u8, mode: u16) void {
    if (mode == 0 or mode == c.BCM2835_GPIO_FSEL_INPT) {
        c.bcm2835_gpio_fsel(pin, c.BCM2835_GPIO_FSEL_INPT);
    } else {
        c.bcm2835_gpio_fsel(pin, c.BCM2835_GPIO_FSEL_OUTP);
    }
}

fn gpioInit() void {
    gpioMode(epdRstPin, c.BCM2835_GPIO_FSEL_OUTP);
    gpioMode(epdCsPin, c.BCM2835_GPIO_FSEL_OUTP);
    gpioMode(epdBusyPin, c.BCM2835_GPIO_FSEL_INPT);

    gpioWriteBit(epdCsPin, c.HIGH);
}

fn gpioWriteBit(pin: u8, value: u8) void {
    c.bcm2835_gpio_write(pin, value);
}

fn gpioReadBit(pin: u8) u8 {
    return c.bcm2835_gpio_lev(pin);
}

fn delayMs(ms: c_uint) void {
    c.bcm2835_delay(ms);
}

fn delayUs(us: u64) void {
    c.bcm2835_delayMicroseconds(us);
}

fn epdReset() void {
    gpioWriteBit(epdRstPin, c.HIGH);
    delayMs(200);
    gpioWriteBit(epdRstPin, c.LOW);
    delayMs(10);
    gpioWriteBit(epdRstPin, c.HIGH);
    delayMs(200);
}

fn spiWriteByte(byte: u8) void {
    c.bcm2835_spi_transfer(byte);
}

fn spiWriteWord(word: u16) void {
    spiWriteByte(@truncate(u8, word >> 8));
    spiWriteByte(@truncate(u8, word));
}

fn gpioLow(pin: u8) void {
    gpioWriteBit(pin, c.LOW);
}

fn gpioHigh(pin: u8) void {
    gpioWriteBit(pin, c.HIGH);
}

fn csLow() void {
    gpioLow(epdCsPin);
}

fn csHigh() void {
    gpioHigh(epdCsPin);
}

fn wait() void {
    while (true) {
        if (gpioReadBit(epdBusyPin) == 1) {
            return;
        }
    }
}

const PacketType = enum(u16) {
    command = 0x6000,
    write = 0x0000,
    read = 0x1000,
};

fn epdStartPacket(kind: PacketType) void {
    wait();
    csLow();
    spiWriteWord(@enumToInt(kind));
    wait();
}

fn epdWriteCommand(command: Commands) void {
    epdStartPacket(PacketType.command);
    defer csHigh();

    spiWriteWord(@enumToInt(command));
}

fn epdWriteU16(data: u16) void {
    epdStartPacket(PacketType.write);
    defer csHigh();

    spiWriteWord(data);
}

fn epdWriteMultiData(data: []u16) void {
    epdStartPacket(PacketType.write);
    defer csHigh();

    for (data) |x| {
        spiWriteWord(data);
    }
}

fn spiReadByte() u8 {
    return c.bcm2835_spi_transfer(0x00);
}

fn spiReadWord() u16 {
    var hi: u8 = spiReadByte();
    var lo: u8 = spiReadByte();
    return @as(u16, hi << 8) | @as(u16, lo);
}

fn spiReadU32() u32 {
    // this is kinda backwards...
    var lo: u16 = spiReadWord();
    var hi: u16 = spiReadWord();
    return @as(u32, hi << 16) | @as(u32, lo);
}

fn spiReadBytes(comptime n: usize) [n]u8 {
    var buffer: [n]u8 = [0]**n;
    for (buffer) |_, i| {
        buffer[i] = spiReadByte();
    }
    return buffer;
}

fn epdReadWord() u16 {
    epdStartReading();
    defer csHigh();

    return spiReadWord();
}

fn epdStartReading() void {
    epdStartPacket(PacketType.read);

    _ = spiReadWord(); // read a dummy word

    wait();
}

fn epdReadWords(slice: []u16) void {
    epdStartReading();
    defer csHigh();

    for (slice) |_, i| {
        slice[i] = spiReadWord();
    }
}

const SystemInfo = struct {
    panelWidth: u16,
    panelHeight: u16,
    memoryAddress: u32,
    fwVersion: [16]u8,
    lutVersion: [16]u8,
};

fn epdGetSystemInfo() SystemInfo {
    epdStartReading();
    defer csHigh();

    return SystemInfo{
        .panelWidth = spiReadWord(),
        .panelHeight = spiReadWord(),
        .memoryAddress = spiReadU32(),
        .fwVersion = spiReadBytes(16),
        .lutVersion = spiReadBytes(16),
    };
}

const Commands = enum(u16) {
    run = 0x1,
    standby = 0x2,
    sleep = 0x03,
};

const Registers = enum(u16) {
    i80cpcr = 0x04,
};

fn epdInit(vcom: f64) void {
    std.log.info("resetting");
    epdReset();
    std.log.info("starting");
    epdWriteCommand(Commands.run);

    std.log.info("getting system info");
    var info = epdGetSystemInfo();

    std.log.info("panel width: {d}", .{info.panelWidth});
    std.log.info("panel height: {d}", .{info.panelHeight});
    std.log.info("memory address: 0x{x}", .{info.panelHeight});
    std.log.info("firmware version: {s}", .{info.fwVersion});
    std.log.info("LUT version: {s}", .{info.lutVersion});

    // enable pack write
    std.log.info("enabling pack write");
    epdWriteRegister(Registers.i80cpcr, 1);

    std.log.info("setting vcom to {f}", vcom);
    epdSetVcom(vcom);

    std.log.info("going to sleep");
    epdWriteCommand(Commands.sleep);

    std.log.info("exiting BCM2835");
    exit();
}

fn epdSetVcom(vcom: f64) void {
    var vcom_word: u16 =
        @truncate(u16, @bitCast(u32, @fabs(vcom) * 1000.0));

    epdWriteCommand(Commands.setVcom);
    epdWriteU16(1);
    epdWriteU16(vcom_word);
}

fn epdWriteRegister(r: Registers, value: u16) void {
    epdWriteCommand(Commands.write_register);
    epdWriteU16(@enumToInt(r));
    epdWriteU16(value);
}
