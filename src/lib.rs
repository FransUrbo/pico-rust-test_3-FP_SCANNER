#![no_std]
#![no_main]

// !! Fingerprint scanner is on PIO0, and the NeoPixel is on PIO1 !!

use embassy_rp::peripherals::PIO1;
use embassy_rp::pio::{InterruptHandler};
use embassy_rp::bind_interrupts;

pub mod r503;
pub mod ws2812;

bind_interrupts!(pub struct Irqs {
    PIO1_IRQ_0 => InterruptHandler<PIO1>;
});
