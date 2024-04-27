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
    let mut r503 = R503::new(p.PIO0, p.DMA_CH0, p.PIN_26, p.PIN_27, p.PIN_22);

    // Initialize the multi-colour LED.
    let Pio { mut common, sm0, .. } = Pio::new(p.PIO1, Irqs);
    let mut ws2812 = Ws2812::new(&mut common, sm0, p.DMA_CH1, p.PIN_15);

    loop {
	debug!("NeoPixel off");
	ws2812.write(&[(0,0,0).into()]).await;
	Timer::after_secs(1).await;

	debug!("NeoPixel Blue");
	ws2812.write(&[(0,0,255).into()]).await;
	Timer::after_secs(1).await;

	match r503.VfyPwd(0x00000000).await {
	    Status::CmdExecComplete => {
		info!("Fingerprint scanner password correct");
	    },
	    Status::ErrorReceivePackage => {
		info!("ERROR: Fingerprint scanner password check - package receive");
	    },
	    Status::ErrorPassword => {
		info!("ERROR: Fingerprint scanner password check - wrong password");
	    },
	    stat => {
		info!("ERROR: code='{=u8:#04x}'", stat as u8);
	    }
	}
    }
}
