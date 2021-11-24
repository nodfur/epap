const c = @cImport(@cInclude("bcm2835.h"));
const std = @import("std");

const epd_rst_pin: u8 = 17;
const epd_cs_pin: u8 = 8;
const epd_busy_pin: u8 = 24;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Hello, {s}!\n", .{"world"});

    try epap_init();
}

fn epap_init() !void {
    if (c.bcm2835_init() == 0) {
        return error.BCM2835_InitFailed;
    }

    if (c.bcm2835_spi_begin() == 0) {
        return error.BCM2835_SPI_Failed;
    }

    c.bcm2835_spi_setBitOrder(c.BCM2835_SPI_BIT_ORDER_MSBFIRST);
    c.bcm2835_spi_setDataMode(c.BCM2835_SPI_MODE0);
    c.bcm2835_spi_setClockDivider(c.BCM2835_SPI_CLOCK_DIVIDER_32);

    gpio_init();
}

fn epap_exit() void {
    gpio_write_bit(epd_cs_pin, c.LOW);
    gpio_write_bit(epd_rst_pin, c.LOW);

    c.bcm2835_spi_end();
    c.bcm2835_close();
}

fn gpio_mode(pin: u8, mode: u16) void {
    if (mode == 0 or mode == c.BCM2835_GPIO_FSEL_INPT) {
        c.bcm2835_gpio_fsel(pin, c.BCM2835_GPIO_FSEL_INPT);
    } else {
        c.bcm2835_gpio_fsel(pin, c.BCM2835_GPIO_FSEL_OUTP);
    }
}

fn gpio_init() void {
    gpio_mode(epd_rst_pin, c.BCM2835_GPIO_FSEL_OUTP);
    gpio_mode(epd_cs_pin, c.BCM2835_GPIO_FSEL_OUTP);
    gpio_mode(epd_busy_pin, c.BCM2835_GPIO_FSEL_INPT);

    gpio_write_bit(epd_cs_pin, c.HIGH);
}

fn gpio_write_bit(pin: u8, value: u8) void {
    c.bcm2835_gpio_write(pin, value);
}

fn gpio_read_bit(pin: u8) u8 {
    return c.bcm2835_gpio_lev(pin);
}
