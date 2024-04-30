#![no_std]
#![no_main]

// !! Fingerprint scanner is on PIO0, and the NeoPixel is on PIO1 !!

use defmt::{debug, info, error};

use embassy_executor::Spawner;
use embassy_rp::peripherals::PIO1;
use embassy_rp::pio::{InterruptHandler, Pio};
use embassy_rp::bind_interrupts;
use embassy_time::Timer;

use {defmt_rtt as _, panic_probe as _};

use r503::r503::R503;
use r503::ws2812::Ws2812 as Ws2812;

bind_interrupts!(pub struct Irqs {
    PIO1_IRQ_0 => InterruptHandler<PIO1>;
});

// ================================================================================

#[embassy_executor::main]
async fn main(_spawner: Spawner) {
    info!("Start");

    let p = embassy_rp::init(Default::default());

    // Initialize the fingerprint scanner.
    let mut r503 = R503::new(p.UART0, p.PIN_16, p.DMA_CH0, p.PIN_17, p.DMA_CH1, p.PIN_13.into());

    // Initialize the multi-colour LED.
    let Pio { mut common, sm0, .. } = Pio::new(p.PIO1, Irqs);
    let mut ws2812 = Ws2812::new(&mut common, sm0, p.DMA_CH3, p.PIN_15);

    debug!("NeoPixel OFF");
    ws2812.write(&[(0,0,0).into()]).await;
    Timer::after_secs(1).await;

    debug!("NeoPixel ON");
    ws2812.write(&[(0,0,255).into()]).await; // BLUE
    Timer::after_secs(1).await;

    // =====

    if r503.Wrapper_Enrole_Fingerprint().await {
	error!("Can't enrole fingerprint");

	debug!("NeoPixel RED");
	ws2812.write(&[(0,255,0).into()]).await; // RED
    } else {
	info!("Fingerprint enrolled");

	debug!("NeoPixel GREEN");
	ws2812.write(&[(255,0,0).into()]).await; // GREEN
    }
}
