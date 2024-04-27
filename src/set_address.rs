#![no_std]
#![no_main]

// !! Fingerprint scanner is on PIO0, and the NeoPixel is on PIO1 !!

use defmt::{debug, info};

use embassy_executor::Spawner;
use embassy_rp::peripherals::PIO1;
use embassy_rp::pio::{InterruptHandler, Pio};
use embassy_rp::bind_interrupts;
use embassy_time::{Timer};

use {defmt_rtt as _, panic_probe as _};

use r503::r503::{R503, Status};
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
    let mut r503 = R503::new(p.PIO0, p.DMA_CH0, p.PIN_26, p.PIN_27, p.PIN_28);

    // Initialize the multi-colour LED.
    let Pio { mut common, sm0, .. } = Pio::new(p.PIO1, Irqs);
    let mut ws2812 = Ws2812::new(&mut common, sm0, p.DMA_CH1, p.PIN_15);
    let mut data = (0,0,255).into(); // BLUE

    loop {
	debug!("NeoPixel OFF");
	ws2812.write(&[(0,0,0).into()]).await;
	Timer::after_secs(1).await;

	debug!("NeoPixel ON");
	ws2812.write(&[data]).await;
	Timer::after_secs(1).await;

	match r503.SetAdder(0xFFFFFF01).await {
	    Status::CmdExecComplete => {
		info!("Fingerprint scanner address set");
		data = (255,0,0).into(); // GREEN
	    },
	    Status::ErrorReceivePackage => {
		info!("ERROR: Fingerprint scanner address set - package receive");
		data = (0,255,0).into(); // RED
	    },
	    stat => {
		info!("ERROR: code='{=u8:#04x}'", stat as u8);
	    }
	}
    }
}
