#![no_std]
#![no_main]

use defmt::*;

use embassy_executor::Spawner;
use embassy_rp::peripherals::PIO1;
use embassy_rp::pio::{InterruptHandler, Pio};
use embassy_rp::bind_interrupts;
use embassy_time::{Timer};

use {defmt_rtt as _, panic_probe as _};

pub mod r503;
pub mod ws2812;
use crate::ws2812::Ws2812;

bind_interrupts!(pub struct Irqs {
    PIO1_IRQ_0 => InterruptHandler<PIO1>;
});

// ================================================================================

#[embassy_executor::main]
async fn main(_spawner: Spawner) {
    info!("Start");

    let p = embassy_rp::init(Default::default());

    // Initialize the fingerprint scanner.
    let mut r503 = r503::R503::new(p.PIO0, p.DMA_CH0, p.PIN_26, p.PIN_27, p.PIN_22);

    // Initialize the multi-colour LED.
    let Pio { mut common, sm0, .. } = Pio::new(p.PIO1, Irqs);
    let mut ws2812 = Ws2812::new(&mut common, sm0, p.DMA_CH1, p.PIN_15);

    loop {
	info!("NeoPixel off");
	ws2812.write(&[(0,0,0).into()]).await;
	Timer::after_secs(1).await;

	info!("NeoPixel Blue");
	ws2812.write(&[(0,0,255).into()]).await;
	Timer::after_secs(1).await;

	match r503.VfyPwd(0x00000000).await {
	    r503::Status::CmdExecComplete => {
		info!("VfyPwd returned CmdExecComplete");
	    },
	    stat => {
		info!("VfyPwd returned UNKNOWN ({:?})", stat as u8);
	    }
	}
    }
}
