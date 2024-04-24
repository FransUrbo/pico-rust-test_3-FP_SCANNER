pub mod r503 {
    use defmt::*;

    use embassy_rp::dma::{AnyChannel, Channel};
    use embassy_rp::peripherals::PIO0;
    use embassy_rp::pio::{Direction, InterruptHandler, Pio, PioPin, StateMachine};
    use embassy_rp::{bind_interrupts, into_ref, Peripheral, PeripheralRef};

    #[derive(Clone, Copy)]
    #[repr(u8)]
    #[allow(unused)]
    pub enum Status {
	CmdExecComplete				= 0x00,
	ErrorReceivePackage			= 0x01,
	ErrorNoFingerOnSensor			= 0x02,
	ErrorEnroleFinger			= 0x03,
	ErrorGenCharFileDistortedImage		= 0x06,
	ErrorGenCharFileSmallImage		= 0x07,
	ErrorNoFingerMatch			= 0x08,
	ErrorNoMatchingFinger			= 0x09,
	ErrorCombineCharFiles			= 0x0a,
	ErrorPageIdBeyondLibrary		= 0x0b,
	ErrorReadingTemplateFromLibrary		= 0x0c,
	ErrorUploadTemplate			= 0x0d,
	ErrorReceiveData			= 0x0e,
	ErrorUploadImage			= 0x0f,
	ErrorDeleteTemplate			= 0x10,
	ErrorClearLibrary			= 0x11,
	ErrorPassword				= 0x13,
	ErrorMissingValidPrimaryImage		= 0x15,
	ErrorWriteFlash				= 0x18,
	ErrorNoDef				= 0x19,
	ErrorInvalidRegister			= 0x1a,
	ErrorIncorrectConfigRegister		= 0x1b,
	ErrorWrongNotepadNumber			= 0x1c,
	ErrorFailedOperateCommunicationPort	= 0x1d
    }

    // =====

    bind_interrupts!(pub struct Irqs {
	PIO0_IRQ_0 => InterruptHandler<PIO0>;
    });

    pub struct R503<'l> {
	dma: PeripheralRef<'l, AnyChannel>,
	sm: StateMachine<'l, PIO0, 0>,

	buf: [u8; 40]
    }

    impl<'l> R503<'l> {
	pub async fn new(
	    pio: impl Peripheral<P = PIO0> + 'l,
	    dma: impl Peripheral<P = impl Channel> + 'l,
	    pin_send: impl PioPin,
	    pin_receive: impl PioPin,
	    pin_wake: impl PioPin
	) -> R503<'l> {
	    into_ref!(dma);

	    let irq = Irqs;
	    let Pio {
		mut common,
//		mut irq0,
		mut sm0,
		..
	    } = Pio::new(pio, irq);

	    let tx = common.make_pio_pin(pin_send);
	    let rx = common.make_pio_pin(pin_receive);
	    let pw = common.make_pio_pin(pin_wake);

	    sm0.set_pin_dirs(Direction::Out, &[&tx]);
	    sm0.set_pin_dirs(Direction::In, &[&rx, &pw]);

	    Self {
		dma: dma.map_into(),
		sm: sm0,
		buf: [0x20; 40]
	    }
	}

	// ===== Internal functions

//	fn write_cmd_bytes(&mut self, bytes: &[u8]) {
//	}
//
//	fn write_header(&mut self, address: u32) {
//	    self.write_cmd_bytes(&[0xEF, 0x01]);
//	    self.write_cmd_bytes(&address.to_be_bytes()[..]);
//	}
//
//	fn compute_checksum(&self) -> u16 {
//	    let mut checksum = 0u16;
//	    let check_end = self.buf.len();
//	    let checked_bytes = &self.buf[6..check_end];
//	    for byte in checked_bytes {
//		checksum += (*byte) as u16;
//	    }
//	    return checksum;
//	}

	// ===== System-related instructions

	// Description: Verify Module’s handshaking password.
	// Input Parameter: PassWord (4 bytes)
	// Return Parameter: Confirmation code (1 byte)
	// Instruction code: 13H
	#[allow(non_snake_case)]
	pub async fn VfyPwd(&mut self, pass: u32) -> Status {
	    info!("Checking password: '{}'", pass);
	    return Status::CmdExecComplete
	}

//	// Description: Set Module’s handshaking password.
//	// Input Parameter: PassWord (4 bytes)
//	// Return Parameter: Confirmation code (1 byte)
//	// Instruction code: 12H
//	#[allow(non_snake_case)]
//	pub async fn SetPwd(&mut self, pass: char) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: Set Module address.
//	// Input Parameter: None.
//	// Return Parameter: Confirmation code (1 byte)
//	// Instruction code: 15H    
//	pub async fn SetAdder(&mut self) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: Operation parameter settings.
//	// Input Parameter: Parameter number.
//	// Return Parameter: Confirmation code (1 byte)
//	// Instruction code: 0eH
//	pub async fn SetSysPara(&mut self, param: u8) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description:
//	//   For UART protocol, it control the “on/off” of USB port;
//	//   For USB protocol, it control the “on/off” of UART port;
//	// Input Parameter: control code
//	//   Control code ”0” means turns off the port;
//	//   Control code ”1” means turns on the port;
//	// Return Parameter: Confirmation code (1 byte)
//	// Instruction code: 17H
//	pub async fn Control(&mut self, ctrl: bool) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: Read Module’s status register and system basic configuration parameters.
//	// Input Parameter: none
//	// Return Parameter: Confirmation code (1 byte) + basic parameter(16bytes)
//	// Instuction code: 0fH
//	pub async fn ReadSysPara(&mut self) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: read the current valid template number of the Module.
//	// Input Parameter: none
//	// Return Parameter: Confirmation code (1 byte) +template number:N
//	// Instuction code: 1dH
//	pub async fn TempleteNum(&mut self) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: Read the fingerprint template index table of the module,
//	//              read the index table of the fingerprint template up to 256 at a time (32 bytes).
//	// Input Parameter: Index page
//	// Return Parameter: Confirmation code + Fingerprint template index table
//	// Instuction code: 0x1F
//	pub async fn ReadIndexTable(&mut self, page: u8) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// ===== Fingerprint-processing instructions
//
//	// Description: Detecting finger and store the detected finger image in ImageBuffer while returning
//	//              successfull confirmation code; If there is no finger, returned confirmation code
//	//              would be “can’t detect finger”.
//	// Input Parameter: none
//	// Return Parameter: Confirmation code (1 byte)
//	// Instuction code: 01H
//	pub async fn GenImg(&mut self) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: to upload the image in Img_Buffer to upper computer.
//	// Input Parameter: none
//	// Return Parameter: Confirmation code (1 byte)
//	// Instuction code: 0aH
//	pub async fn UpImage(&mut self) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: Download image from upper computer to Img_Buffer.
//	// Input Parameter: none
//	// Return Parameter: Confirmation code (1 byte)
//	// Instuction code: 0bH
//	pub async fn DownImage(&mut self) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: Generate character file from the original finger image in ImageBuffer and store the
//	//              file in CharBuffer1 or CharBuffer2.
//	//              Note: BufferID of CharBuffer1 and CharBuffer2 are 1h and 2h respectively. Other values
//	//                    (except 1h, 2h) would be processed as CharBuffer2.
//	// Input Parameter: BufferID (character file buffer number)
//	// Return Parameter: Confirmation code (1 byte)
//	// Instuction code: 02H
//	pub async fn Img2Tz(&mut self, buff: u8) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: Combine information of character files from CharBuffer1 and CharBuffer2 and generate
//	//              a template which is stroed back in both CharBuffer1 and CharBuffer2.
//	// Input Parameter: none
//	// Return Parameter: Confirmation code (1 byte)
//	// Instuction code: 05H
//	pub async fn RegModel(&mut self) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: Upload the character file or template of CharBuffer1/CharBuffer2 to upper computer.
//	//              Note: BufferID of CharBuffer1 and CharBuffer2 are 1h and 2h respectively. Other values
//	//                    (except 1h, 2h) would be processed as CharBuffer2.
//	// Input Parameter: BufferID (Buffer number)
//	// Return Parameter: Confirmation code (1 byte)
//	// Instuction code: 08H
//	pub async fn UpChar(&mut self, buff: u8) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: Upper computer download template to module buffer.
//	// Input Parameter: CharBufferID (Buffer number)
//	// Return Parameter: Confirmation code (1 byte)
//	// Instuction code: 09H
//	pub async fn DownChar(&mut self, buff: u8) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: Store the template of specified buffer (Buffer1/Buffer2) at the designated location
//	//              of Flash library.
//	//              Note: BufferID of CharBuffer1 and CharBuffer2 are 1h and 2h respectively. Other values
//	//                    (except 1h, 2h) would be processed as CharBuffer2.
//	// Input Parameter:
//	//   BufferID(buffer number);
//	//   PageID(Flash location of the template, two bytes with high byte front and low byte behind)
//	// Return Parameter: Confirmation code (1 byte)
//	// Instuction code: 06H
//	pub async fn Store(&mut self, buff: u8, page: u16) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: Load template at the specified location (PageID) of Flash library to template buffer
//	//              CharBuffer1/CharBuffer2
//	// Input Parameter:
//	//   BufferID(buffer number);
//	//   PageID (Flash location of the template, two bytes with high byte front and low byte behind)。
//	// Return Parameter: Confirmation code (1 byte)
//	// Instuction code: 07H
//	pub async fn LoadChar(&mut self, buff: u8, page: u16) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: Delete a segment (N) of templates of Flash library started from the specified location
//	//              (or PageID);
//	// Input Parameter:
//	//   PageID (template number in Flash);
//	//   N (number of templates to be deleted)
//	// Return Parameter: Confirmation code (1 byte)
//	// Instuction code: 0cH
//	pub async fn DeletChar(&mut self, page: u16, n: u16) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: to delete all the templates in the Flash library.
//	// Input Parameter: none
//	// Return Parameter: Confirmation code (1 byte)
//	// Instuction code: 0dH
//	pub async fn Empty(&mut self) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: Carry out precise matching of templates from CharBuffer1 and CharBuffer2, providing
//	//              matching results.
//	// Input Parameter: none
//	// Return Parameter: Confirmation code (1 byte)，matching score.
//	// Instuction code: 03H
//	pub async fn Match(&mut self) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: Search the whole finger library for the template that matches the one in CharBuffer1
//	//              or CharBuffer2. When found, PageID will be returned.
//	// Input Parameter:
//	//   BufferID;
//	//   StartPage (searching start address);
//	//   PageNum (searching numbers)
//	// Return Parameter: Confirmation code (1 byte) + PageID (matching templates location)
//	// Instuction code: 04H
//	pub async fn Search(&mut self, buff: u8, start: u16, page: u16) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: Detect the finger, record the fingerprint image and store it in ImageBuffer, return
//	//              it and record the successful confirmation code;
//	//              If no finger is detected, return no finger confirmation code(the module responds
//	//              quickly to each instruction,therefore, for continuous detection, cycle processing
//	//              is required, which can be limited to the number of cycles or the total time).
//	//              Differences between GetImageEx and the GetImage:
//	//                GetImage: Return the confirmation code 0x00 when the image quality is too bad
//	//                          (image collection succeeded).
//	//                GetImageEx: Return the confirmation code 0x07 when the image quality is too bad
//	//                            (poor collection quality).
//	// Input Parameter: none
//	// Return Parameter: Confirmation code
//	// Instuction code: 0x28
//	pub async fn GetImageEx(&mut self) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: Cancel instruction
//	// Input Parameter: none
//	// Return Parameter: Confirmation code
//	// Instuction code: 0x30
//	pub async fn Cancel(&mut self) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: Send handshake instructions to the module. If the module works normally, the
//	//              confirmation code 0x00 will be returned. The upper computer can continue to
//	//              send instructions to the module.If the confirmation code is other or no reply,
//	//              it means that the device is abnormal.
//	// Input Parameter: none
//	// Return Parameter: Confirmation code
//	// Instuction code: 0x40
//	pub async fn HandShake(&mut self) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: Check whether the sensor is normal.
//	// Input Parameter: none
//	// Return Parameter: Confirmation code
//	// Instuction code: 0x36
//	pub async fn CheckSensor(&mut self) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: Get the algorithm library version.
//	// Input Parameter: none
//	// Return Parameter: Confirmation code (1 byte) + AlgVer (algorithm library version string)
//	// Instuction code: 0x39
//	pub async fn GetAlgVer(&mut self) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: Get the firmware version.
//	// Input Parameter: none
//	// Return Parameter: Confirmation code + FwVer (Firmware version string)
//	// Instuction code: 0x3A
//	pub async fn GetFwVer(&mut self) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: Read product information.
//	// Input Parameter: none
//	// Return Parameter: Confirmation code + ProdInfo (product information)
//	// Instuction code: 0x3C
//	pub async fn ReadProdInfo(&mut self) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: Send soft reset instruction to the module. If the module works normally, return
//	//              confirmation code 0x00, and then perform reset operation.
//	// Input Parameter: none
//	// Return Parameter: Confirmation code (1 byte)
//	// Instuction code: 0x3D
//	pub async fn SoftRst(&mut self) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: Aura LED control
//	// Input Parameter:
//	//   Control code;
//	//     0x01: Breathing light
//	//     0x02: Flashing light
//	//     0x03: Light Always on
//	//     0x04: Light Always off
//	//     0x05: Light gradually on
//	//     0x06: Light gradually off
//	//   Speed;
//	//     0x00-0xff, 256 gears, Minimum 5s cycle.
//	//   ColorIndex;
//	//     0x01: Red
//	//     0x02: Blue
//	//     0x03: Purple
//	//   Times
//	//     Number of cycles: 0- infinite, 1-255.
//	// Return Parameter: Confirmation code (1 byte)
//	// Instuction code: 0x35
//	pub async fn AuraLedConfig(&mut self, ctrl: u8, speed: u8, colour: u8, times: i8) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// ===== Other instructions
//	// Description: Command the Module to generate a random number and return it to upper computer.
//	// Input Parameter: none
//	// Return Parameter: Confirmation code (1 byte)
//	// Instuction code: 14H
//	pub async fn GetRandomCode(&mut self) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: read information page(512bytes)
//	// Input Parameter: none
//	// Return Parameter: Confirmation code (1 byte)
//	// Instuction code: 16H
//	pub async fn ReadInfPage(&mut self) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: Upper computer to write data to the specified Flash page. Also see ReadNotepad.
//	// Input Parameter: NotePageNum, user content (or data content)
//	// Return Parameter: Confirmation code (1 byte)
//	// Instuction code: 18H
//	pub async fn WriteNotepad(&mut self) -> Status {
//	    return Status::CmdExecComplete
//	}
//
//	// Description: Read the specified page’s data content. Also see WriteNotepad.
//	// Input Parameter: none
//	// Return Parameter: Confirmation code (1 byte) + data content
//	// Instuction code: 19H
//	pub async fn ReadNotepad(&mut self) -> Status {
//	    return Status::CmdExecComplete
//	}
    }
}
