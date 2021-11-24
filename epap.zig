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

fn epdWriteCommand(command: u16) void {
    epdReadBusy();
    gpioLow(epdCsPin);
    spiWriteWord(0x6000);
    epdReadBusy();
    spiWriteWord(command);
    gpioHigh(epdCsPin);
}

fn epdReadBusy() void {
    while (true) {
        if (gpioReadBit(epdBusyPin) == 1) {
            return;
        }
    }
}

fn epdWriteData(data: u16) void {
    epdReadBusy();
    gpioLow(epdCsPin);
    spiWriteWord(0x6000);
    epdReadBusy();
}
