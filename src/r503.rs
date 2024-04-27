#![allow(non_snake_case)]	// I want to keep with the manufacturers naming scheme.
#![allow(unused)]		// Not finished yet, so EVERYTHING is unused!! :D

use defmt::{debug, info};

use embassy_rp::dma::{AnyChannel, Channel};
use embassy_rp::gpio::Level;
use embassy_rp::pio::{
    Config, Direction, FifoJoin, InterruptHandler, Pio, PioPin,
    ShiftConfig, ShiftDirection, StateMachine
};
use embassy_rp::peripherals::PIO0;
use embassy_rp::{bind_interrupts, into_ref, Peripheral, PeripheralRef};

use chksum8;

bind_interrupts!(pub struct Irqs {
    PIO0_IRQ_0 => InterruptHandler<PIO0>;
});

// =====

const ADDRESS: [i32; 4] = [0xFF, 0xFF, 0xFF, 0xFF];

#[derive(Copy, Clone)]
#[repr(u8)]
pub enum Status {
    CmdExecComplete			= 0x00,
    ErrorReceivePackage			= 0x01,
    ErrorNoFingerOnSensor		= 0x02,
    ErrorEnroleFinger			= 0x03,
    ErrorGenCharFileDistortedImage	= 0x06,
    ErrorGenCharFileSmallImage		= 0x07,
    ErrorNoFingerMatch			= 0x08,
    ErrorNoMatchingFinger		= 0x09,
    ErrorCombineCharFiles		= 0x0a,
    ErrorPageIdBeyondLibrary		= 0x0b,
    ErrorReadingTemplateFromLibrary	= 0x0c,
    ErrorUploadTemplate			= 0x0d,
    ErrorReceiveData			= 0x0e,
    ErrorUploadImage			= 0x0f,
    ErrorDeleteTemplate			= 0x10,
    ErrorClearLibrary			= 0x11,
    ErrorPassword			= 0x13,
    ErrorMissingValidPrimaryImage	= 0x15,
    ErrorWriteFlash			= 0x18,
    ErrorNoDef				= 0x19,
    ErrorInvalidRegister		= 0x1a,
    ErrorIncorrectConfigRegister	= 0x1b,
    ErrorWrongNotepadNumber		= 0x1c,
    ErrorFailedOperateCommunicationPort	= 0x1d
}

// These are in Hex order. Further down, they're defined in the order they
// came in the documentation.
#[derive(Copy, Clone)]
#[repr(u8)]
pub enum Command {
    GenImg		= 0x01,
    Img2Tz		= 0x02,
    Match		= 0x03,
    Search		= 0x04,
    RegModel		= 0x05,
    Store		= 0x06,
    LoadChar		= 0x07,
    UpChar		= 0x08,
    DownChar		= 0x09,
    UpImage		= 0x0a,
    DownImage		= 0x0b,
    DeletChar		= 0x0c,
    Empty		= 0x0d,
    SetSysPara		= 0x0e,
    ReadSysPara		= 0x0f,
    SetPwd		= 0x12,
    VfyPwd		= 0x13,
    SetAdder		= 0x15,
    ReadInfPage		= 0x16,
    Control		= 0x17,
    ReadNotepad		= 0x19,
    TempleteNum		= 0x1d,
    GetImageEx		= 0x28,
    Cancel		= 0x30,
    CheckSensor		= 0x36,
    GetAlgVer		= 0x39,
    GetFwVer		= 0x3a,
    SoftRst		= 0x3d,
    HandShake		= 0x40,
    GetRandomCode	= 0x14,
    WriteNotepad	= 0x18,
    ReadIndexTable	= 0x1f,
    AuraLedConfig	= 0x35,
    ReadProdInfo	= 0x3c
}

#[derive(Copy, Clone)]
#[repr(u16)]
pub enum Packets {
    StartCode		= 0xEF01,	// High byte transferred first.
    CommandPacket	= 0x01,
    DataPacket		= 0x02,
    AckPacket		= 0x07,
    EndDataPacket	= 0x08
}

#[derive(Copy, Clone)]
#[repr(u8)]
pub enum AuroraLEDControl {
    BreathingLight	= 0x01,
    FlashingLight	= 0x02,
    AlwaysOn		= 0x03,
    AlwaysOff		= 0x04,
    GraduallyOn		= 0x05,
    GraduallyOff	= 0x06
}

#[derive(Copy, Clone)]
#[repr(u8)]
pub enum AuroraLEDColour {
    Red			= 0x01,
    Blue		= 0x02,
    Purple		= 0x03
}

// =====

pub struct R503<'l> {
    dma: PeripheralRef<'l, AnyChannel>,
    sm: StateMachine<'l, PIO0, 0>,

    buffer: [u8; 128]
}

impl<'l> R503<'l> {
    pub fn new(
	pio:		impl Peripheral<P = PIO0> + 'l,
	dma:		impl Peripheral<P = impl Channel> + 'l,
	pin_send:	impl PioPin,
	pin_receive:	impl PioPin,
	pin_wakeup:	impl PioPin
    ) -> Self {
	into_ref!(dma);

	let Pio {
	    mut common,
	    mut sm0,
	    ..
	} = Pio::new(pio, Irqs);

	let tx = common.make_pio_pin(pin_send);
	let rx = common.make_pio_pin(pin_receive);
	let wu = common.make_pio_pin(pin_wakeup);
	let mut cfg = Config::default();

	// FIFO setup.
	cfg.fifo_join = FifoJoin::TxOnly;
	cfg.shift_out = ShiftConfig {
	    auto_fill: true,
	    threshold: 24,
	    direction: ShiftDirection::Left,
	};

	// Pin setup.
	sm0.set_pin_dirs(Direction::Out, &[&tx]);
	sm0.set_pin_dirs(Direction::In,  &[&rx, &wu]);	// TODO: Is WakeUp an INPUT or OUTPUT??
	sm0.set_pins(Level::Low, &[&tx, &rx, &wu]);

	sm0.set_config(&cfg);
	sm0.set_enable(true);

	Self {
	    dma:    dma.map_into(),
	    sm:     sm0,
	    buffer: [0x20; 128]
	}
    }

    // ===== Internal functions

    // Data package format
    // Name		Length		Description
    // ==========================================================================================================
    // Start	2 bytes		Fixed value of 0xEF01; High byte transferred first.
    // Address	4 bytes		Default value is 0xFFFFFFFF, which can be modified by command.
    //				High byte transferred first and at wrong adder value, module
    //				will reject to transfer.
    // PID	1 byte		01H	Command packet;
    //				02H	Data packet; Data packet shall not appear alone in executing
    //					processs, must follow command packet or acknowledge packet.
    //				07H	Acknowledge packet;
    //				08H	End of Data packet.
    // LENGTH	2 bytes		Refers to the length of package content (command packets and data packets)
    //				plus the length of Checksum (2 bytes). Unit is byte. Max length is 256 bytes.
    //				And high byte is transferred first.
    // DATA	-		It can be commands, data, command’s parameters, acknowledge result, etc.
    //				(fingerprint character value, template are all deemed as data);
    // SUM	2 bytes		The arithmetic sum of package identifier, package length and all package
    //				contens. Overflowing bits are omitted. high byte is transferred first.

    // This is where the "magic" happens! NO IDEA HOW TO WRITE OR READ TO/FROM THAT THING!!

    fn write(&mut self, package: &[u32]) -> Status {
	debug!("Writing package");

	// The Python lib uses `self._uart.write(bytearray(packet))` to do the actual write!

	return Status::CmdExecComplete;
	//return Status::ErrorReceivePackage;
	//return Status::ErrorPassword;
	//return Status::ErrorCombineCharFiles;
    }

    fn read(&mut self) -> Status {
	debug!("Reading reply");

	// The Python lib uses `self._uart.read(expected)` to do the actual read!
	return Status::CmdExecComplete;
    }

    // -----

    fn send_command(&mut self, command: Command, data: u32) -> Status {
	debug!("Sending command {=u32:#04x}", command as u32);

	// Setup package.
	let mut package: [u32; 6] = [0; 6];
	package[0] = 0xEF01;		// Start (u16)
	package[1] = 0xFFFFFFFF;	// Address (u32)
	package[2] = command as u32;	// PID
	if(data != 0) {
	    package[4] = data as u32;
	}
	package[3] = package.len() as u32;
	package[5] = chksum8::sum(&package) as u32;
	debug!("Package: {:?}", package);

	// Send package.
	return self.write(&package);
    }

    fn compute_length(&self) -> u16 {
	debug!("Computing package length");

	return 0;
    }

    fn parse_reply(&self) -> Status {
	debug!("Parsing reply");

	return Status::CmdExecComplete;
    }

    // ===== System-related instructions

    // Description: Verify Module’s handshaking password.
    // Input Parameter: PassWord (4 bytes)
    // Return Parameter: Confirmation code (1 byte)
    //   Confirmation code = 00H: Correct password;
    //   Confirmation code = 01H: Error when receiving package;
    //   Confirmation code = 13H: Wrong password;
    // Instruction code: 13H
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x07
    //   Instruction code	 1 byte		0x13
    //   Data			 4 bytes
    //     PassWord		 4 byte
    //   Checksum		 2 bytes	Sum		(see top)
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x03
    //   Confirmation code	 1 byte		xx		(see above)
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn VfyPwd(&mut self, pass: u32) -> Status {
	debug!("Checking password: '{:?}'", pass);

	let ret = self.send_command(Command::VfyPwd, pass);
	if(ret as u8 == 0) {
	    return ret;
	} else if(ret as u8 == 1) {
	    return Status::ErrorReceivePackage;
	} else {
	    return Status::ErrorPassword;
	}
    }

    // Description: Set Module’s handshaking password.
    // Input Parameter: PassWord (4 bytes)
    // Return Parameter: Confirmation code (1 byte)
    //   Confirmation code=00H: password setting complete;
    //   Confirmation code=01H: error when receiving package;
    // Instruction code: 12H
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x07
    //   Instruction code	 1 byte		0x12
    //   Data			 4 bytes
    //     PassWord		 4 byte
    //   Checksum		 2 bytes	Sum		(see top)
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   ??? Package Identifier	 1 byte		0x07 ???	NO PID ?!??
    //   Package Length		 2 byte		0x03
    //   Confirmation code	 1 byte		xx		(see above)
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn SetPwd(&mut self, pass: u32) -> Status {
	return self.send_command(Command::SetPwd, pass);
    }

    // Description: Set Module address.
    // Input Parameter: None.
    // Return Parameter: Confirmation code (1 byte)
    //   Confirmation code=00H: address setting complete;
    //   Confirmation code=01H: error when receiving package;
    // Instruction code: 15H
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x07
    //   Instruction code	 1 byte		0x13
    //   Data			 4 bytes
    //     NewAddress		 4 byte
    //   Checksum		 2 bytes	Sum		(see top)
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   NewAddress		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x07
    //   Confirmation code	 1 byte		xx		(see above)
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn SetAdder(&mut self, addr: u32) -> Status {
	return self.send_command(Command::SetAdder, addr);
    }

    // Description: Operation parameter settings.
    // Input Parameter: Parameter number.
    // Return Parameter: Confirmation code (1 byte)
    //   Confirmation code=00H: parameter setting complete;
    //   Confirmation code=01H: error when receiving package;
    //   Confirmation code=1aH: wrong register number;
    // Instruction code: 0eH
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x07
    //   Instruction code	 1 byte		0x0e
    //   Data			 2 bytes
    //     Parameter Number	 1 byte		4/5/6
    //     Content		 1 byte		xx
    //   Checksum		 2 bytes	Sum		(see top)
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x03
    //   Confirmation code	 1 byte		xx		(see above)
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn SetSysPara(&mut self, param: u8) -> Status {
	return self.send_command(Command::SetSysPara, param as u32);
    }

    // Description:
    //   For UART protocol, it control the “on/off” of USB port;
    //   For USB protocol, it control the “on/off” of UART port;
    // Input Parameter: control code
    //   Control code ”0” means turns off the port;
    //   Control code ”1” means turns on the port;
    // Return Parameter: Confirmation code (1 byte)
    //   Confirmation code=00H: Port operation complete;
    //   Confirmation code=01H: error when receiving package;
    //   Confirmation code=1dH: fail to operate the communication port;
    // Instruction code: 17H
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x04
    //   Instruction code	 1 byte		0x17
    //   Data			 1 bytes
    //     ControlCode		 1 byte		0/1
    //   Checksum		 2 bytes	Sum		(see top)
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x03
    //   Confirmation code	 1 byte		xx		(see above)
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn Control(&mut self, ctrl: bool) -> Status {
	return self.send_command(Command::Control, ctrl as u32);
    }

    // Description: Read Module’s status register and system basic configuration parameters.
    // Input Parameter: none
    // Return Parameter: Confirmation code (1 byte) + basic parameter(16bytes)
    //   Confirmation code=00H: read complete;
    //   Confirmation code=01H: error when receiving package;
    // Instruction code: 0fH
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x03
    //   Instruction code	 1 byte		0x0f
    //   Checksum		 2 bytes	Sum		(see top)
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		3+16
    //   Confirmation code	 1 byte		xx		(see above)
    //   Data			16 bytes
    //     Basic Param List	16 byte
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn ReadSysPara(&mut self) -> Status {
	return self.send_command(Command::ReadSysPara, 0 as u32);
    }

    // Description: read the current valid template number of the Module.
    // Input Parameter: none
    // Return Parameter: Confirmation code (1 byte) +template number:N
    //   Confirmation code=0x00: read success;
    //   Confirmation code=0x01: error when receiving package;
    // Instruction code: 1dH
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x0003
    //   Instruction code	 1 byte		0x1d
    //   Checksum		 2 bytes	0x0021
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x05
    //   Confirmation code	 1 byte		xx		(see above)
    //   Data			 2 bytes
    //     Template Number	 2 byte
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn TempleteNum(&mut self) -> Status {
	return self.send_command(Command::TempleteNum, 0 as u32);
    }

    // Description: Read the fingerprint template index table of the module,
    //              read the index table of the fingerprint template up to 256 at a time (32 bytes).
    // Input Parameter: Index page
    //   Index tables are read per page, 256 templates per page
    //   Index page 0 means to read 0 ~ 255 fingerprint template index table;
    //   Index page 1 means to read 256 ~ 511 fingerprint template index table;
    //   Index page 2 means to read 512 ~ 767 fingerprint template index table;
    //   Index page 3 means to read 768 ~ 1023 fingerprint template index table
    // Return Parameter: Confirmation code + Fingerprint template index table
    //   Confirmation code=0x00: read complete;
    //   Confirmation code=0x01: error when receiving package;
    // Instruction code: 1fH
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x0x0004
    //   Instruction code	 1 byte		0x1f
    //   Data			 1 bytes
    //     Index Page		 1 byte
    //   Checksum		 2 bytes	Sum		(see top)
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x0023
    //   Confirmation code	 1 byte		xx		(see above)
    //   Data			32 bytes
    //     Index Page		32 bytes			(see documentation)
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn ReadIndexTable(&mut self, page: u8) -> Status {
	return self.send_command(Command::ReadIndexTable, page as u32);
    }

    // ===== Fingerprint-processing instructions

    // Description: Detecting finger and store the detected finger image in ImageBuffer while returning
    //              successfull confirmation code; If there is no finger, returned confirmation code
    //              would be “can’t detect finger”.
    // Input Parameter: none
    // Return Parameter: Confirmation code (1 byte)
    //   Confirmation code=00H: finger collection successs;
    //   Confirmation code=01H: error when receiving package;
    //   Confirmation code=02H: can’t detect finger;
    //   Confirmation code=03H: fail to collect finger;
    // Instruction code: 01H
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x03
    //   Instruction code	 1 byte		0x01
    //   Checksum		 2 bytes	0x05
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x03
    //   Confirmation code	 1 byte		xx		(see above)
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn GenImg(&mut self) -> Status {
	return self.send_command(Command::GenImg, 0 as u32);
    }

    // Description: to upload the image in Img_Buffer to upper computer.
    // Input Parameter: none
    // Return Parameter: Confirmation code (1 byte)
    //   Confirmation code=00H: ready to transfer the following data packet;
    //   Confirmation code=01H: error when receiving package;
    //   Confirmation code=0fH: fail to transfer the following data packet;
    // Instruction code: 0aH
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x03
    //   Instruction code	 1 byte		0x0a
    //   Checksum		 2 bytes	0x000e
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x03
    //   Confirmation code	 1 byte		xx		(see above)
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn UpImage(&mut self) -> Status {
	return self.send_command(Command::UpImage, 0 as u32);
    }

    // Description: Download image from upper computer to Img_Buffer.
    // Input Parameter: none
    // Return Parameter: Confirmation code (1 byte)
    //   Confirmation code=00H: ready to transfer the following data packet;
    //   Confirmation code=01H: error when receiving package;
    //   Confirmation code=0eH: fail to transfer the following data packet;
    // Instruction code: 0bH
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x03
    //   Instruction code	 1 byte		0x0b
    //   Checksum		 2 bytes	0x000f
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x03
    //   Confirmation code	 1 byte		xx		(see above)
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn DownImage(&mut self) -> Status {
	return self.send_command(Command::DownImage, 0 as u32);
    }

    // Description: Generate character file from the original finger image in ImageBuffer and store the
    //              file in CharBuffer1 or CharBuffer2.
    //              Note: BufferID of CharBuffer1 and CharBuffer2 are 1h and 2h respectively. Other values
    //                    (except 1h, 2h) would be processed as CharBuffer2.
    // Input Parameter: BufferID (character file buffer number)
    // Return Parameter: Confirmation code (1 byte)
    //   Confirmation code=00H: generate character file complete;
    //   Confirmation code=01H: error when receiving package;
    //   Confirmation code=06H: fail to generate character file due to the over-disorderly fingerprint image;
    //   Confirmation code=07H: fail to generate character file due to lackness of character point or
    //                          over-smallness of fingerprint image;
    //   Confirmation code=15H: fail to generate the image for the lackness of valid primary image;
    // Instruction code: 02H
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x04
    //   Instruction code	 1 byte		0x02
    //   Data			 1 bytes
    //     BufferID		 1 byte
    //   Checksum		 2 bytes	Sum		(see top)
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x03
    //   Confirmation code	 1 byte		xx		(see above)
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn Img2Tz(&mut self, buff: u8) -> Status {
	return self.send_command(Command::Img2Tz, buff as u32);
    }

    // Description: Combine information of character files from CharBuffer1 and CharBuffer2 and generate
    //              a template which is stroed back in both CharBuffer1 and CharBuffer2.
    // Input Parameter: none
    // Return Parameter: Confirmation code (1 byte)
    //   Confirmation code=00H: operation success;
    //   Confirmation code=01H: error when receiving package;
    //   Confirmation code=0aH: fail to combine the character files. That’s, the character files don’t belong
    //                          to one finger.
    // Instruction code: 05H
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x03
    //   Instruction code	 1 byte		0x05
    //   Checksum		 2 bytes	0x09
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x03
    //   Confirmation code	 1 byte		xx		(see above)
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn RegModel(&mut self) -> Status {
	return self.send_command(Command::RegModel, 0 as u32);
    }

    // Description: Upload the character file or template of CharBuffer1/CharBuffer2 to upper computer.
    //              Note: BufferID of CharBuffer1 and CharBuffer2 are 1h and 2h respectively. Other values
    //                    (except 1h, 2h) would be processed as CharBuffer2.
    // Input Parameter: BufferID (Buffer number)
    // Return Parameter: Confirmation code (1 byte)
    //   Confirmation code=00H: ready to transfer the following data packet;
    //   Confirmation code=01H: error when receiving package;
    //   Confirmation code=0dH: error when uploading template;
    // Instruction code: 08H
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x04
    //   Instruction code	 1 byte		0x08
    //   Data			 1 bytes
    //     BufferID		 1 byte		
    //   Checksum		 2 bytes	Sum		(see top)
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x03
    //   Confirmation code	 1 byte		xx		(see above)
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn UpChar(&mut self, buff: u8) -> Status {
	return self.send_command(Command::UpChar, buff as u32);
    }

    // Description: Upper computer download template to module buffer.
    // Input Parameter: CharBufferID (Buffer number)
    // Return Parameter: Confirmation code (1 byte)
    //   Confirmation code=00H: ready to transfer the following data packet;
    //   Confirmation code=01H: error when receiving package;
    //   Confirmation code=0eH: can not receive the following data packet;
    // Instruction code: 09H
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x04
    //   Instruction code	 1 byte		0x09
    //   Data			 1 bytes
    //     CharBufferID		 1 byte		
    //   Checksum		 2 bytes	Sum		(see top)
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x03
    //   Confirmation code	 1 byte		xx		(see above)
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn DownChar(&mut self, buff: u8) -> Status {
	return self.send_command(Command::DownChar, buff as u32);
    }

    // Description: Store the template of specified buffer (Buffer1/Buffer2) at the designated location
    //              of Flash library.
    //              Note: BufferID of CharBuffer1 and CharBuffer2 are 1h and 2h respectively. Other values
    //                    (except 1h, 2h) would be processed as CharBuffer2.
    // Input Parameter:
    //   BufferID(buffer number);
    //   PageID(Flash location of the template, two bytes with high byte front and low byte behind)
    // Return Parameter: Confirmation code (1 byte)
    //   Confirmation code=00H: storage success;
    //   Confirmation code=01H: error when receiving package;
    //   Confirmation code=0bH: addressing PageID is beyond the finger library;
    //   Confirmation code=18H: error when writing Flash;
    // Instruction code: 06H
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x06
    //   Instruction code	 1 byte		0x06
    //   Data			 3 bytes
    //     BufferID		 1 byte
    //     PageID		 2 bytes
    //   Checksum		 2 bytes	Sum		(see top)
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x03
    //   Confirmation code	 1 byte		xx		(see above)
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn Store(&mut self, buff: u8, page: u16) -> Status {
	// TODO: Merge `buff` and `page`.
	return self.send_command(Command::Store, buff as u32);
    }

    // Description: Load template at the specified location (PageID) of Flash library to template buffer
    //              CharBuffer1/CharBuffer2
    // Input Parameter:
    //   BufferID(buffer number);
    //   PageID (Flash location of the template, two bytes with high byte front and low byte behind)。
    // Return Parameter: Confirmation code (1 byte)
    //   Confirmation code=00H: load success;
    //   Confirmation code=01H: error when receiving package;
    //   Confirmation code=0cH: error when reading template from library or the readout template is invalid;
    //   Confirmation code=0BH: addressing PageID is beyond the finger library;
    // Instruction code: 07H
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x06
    //   Instruction code	 1 byte		0x07
    //   Data			 3 bytes
    //     BufferID		 1 byte
    //     PageID		 2 bytes
    //   Checksum		 2 bytes	Sum		(see top)
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x03
    //   Confirmation code	 1 byte		xx		(see above)
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn LoadChar(&mut self, buff: u8, page: u16) -> Status {
	// TODO: Merge `buff` and `page`.
	return self.send_command(Command::LoadChar, buff as u32);
    }

    // Description: Delete a segment (N) of templates of Flash library started from the specified location
    //              (or PageID);
    // Input Parameter:
    //   PageID (template number in Flash);
    //   N (number of templates to be deleted)
    // Return Parameter: Confirmation code (1 byte)
    //   Confirmation code=00H: delete success;
    //   Confirmation code=01H: error when receiving package;
    //   Confirmation code=10H: faile to delete templates;
    // Instruction code: 0cH
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x07
    //   Instruction code	 1 byte		0x0c
    //   Data			 4 bytes
    //     PageID		 2 byte
    //     N			 2 bytes
    //   Checksum		 2 bytes	Sum		(see top)
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x03
    //   Confirmation code	 1 byte		xx		(see above)
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn DeletChar(&mut self, page: u8, n: u8) -> Status {
	// TODO: Merge `buff` and `page`.
	return self.send_command(Command::DeletChar, page as u32);
    }

    // Description: to delete all the templates in the Flash library.
    // Input Parameter: none
    // Return Parameter: Confirmation code (1 byte)
    //   Confirmation code=00H: empty success;
    //   Confirmation code=01H: error when receiving package;
    //   Confirmation code=11H: fail to clear finger library;
    // Instruction code: 0dH
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x03
    //   Instruction code	 1 byte		0x0d
    //   Checksum		 2 bytes	0x0011
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x03
    //   Confirmation code	 1 byte		xx		(see above)
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn Empty(&mut self) -> Status {
	return self.send_command(Command::Empty, 0 as u32);
    }

    // Description: Carry out precise matching of templates from CharBuffer1 and CharBuffer2, providing
    //              matching results.
    // Input Parameter: none
    // Return Parameter: Confirmation code (1 byte)，matching score.
    //   Confirmation code=00H: templates of the two buffers are matching;
    //   Confirmation code=01H: error when receiving package;
    //   Confirmation code=08H: templates of the two buffers aren’t matching;
    // Instruction code: 03H
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x03
    //   Instruction code	 1 byte		0x03
    //   Checksum		 2 bytes	0x07
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x05
    //   Confirmation code	 1 byte		xx		(see above)
    //   Data			 2 bytes
    //     Matching Score	 2 byte
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn Match(&mut self) -> Status {
	return self.send_command(Command::Match, 0 as u32);
    }

    // Description: Search the whole finger library for the template that matches the one in CharBuffer1
    //              or CharBuffer2. When found, PageID will be returned.
    // Input Parameter:
    //   BufferID;
    //   StartPage (searching start address);
    //   PageNum (searching numbers)
    // Return Parameter: Confirmation code (1 byte) + PageID (matching templates location)
    //   Confirmation code=00H: found the matching finer;
    //   Confirmation code=01H: error when receiving package;
    //   Confirmation code=09H: No matching in the library (both the PageID and matching score are 0);
    // Instruction code: 04H
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x08
    //   Instruction code	 1 byte		0x04
    //   Data			 5 bytes
    //     BufferID		 1 byte
    //     StartPage		 2 bytes
    //     PageNum		 2 bytes
    //   Checksum		 2 bytes	Sum		(see top)
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x07
    //   Confirmation code	 1 byte		xx		(see above)
    //   Data			 4 bytes
    //     PageID		 2 bytes
    //     MatchScore		 2 bytes
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn Search(&mut self, buff: u8, start: u16, page: u16) -> Status {
	// TODO: Merge `buff`, `start` and `page`.
	return self.send_command(Command::Search, buff as u32);
    }

    // Description: Detect the finger, record the fingerprint image and store it in ImageBuffer, return
    //              it and record the successful confirmation code;
    //              If no finger is detected, return no finger confirmation code(the module responds
    //              quickly to each instruction,therefore, for continuous detection, cycle processing
    //              is required, which can be limited to the number of cycles or the total time).
    //              Differences between GetImageEx and the GetImage:
    //                GetImage: Return the confirmation code 0x00 when the image quality is too bad
    //                          (image collection succeeded).
    //                GetImageEx: Return the confirmation code 0x07 when the image quality is too bad
    //                            (poor collection quality).
    // Input Parameter: none
    // Return Parameter: Confirmation code
    //   Confirmation code=0x00: read success
    //   Confirmation code=0x01: error when receiving package;
    //   Confirmation code=0x02: no fingers on the sensor;
    //   Confirmation code=0x03: unsuccessful entry;
    //   Confirmation code=0x07: poor image quality;
    // Instruction code: 28H
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x0003
    //   Instruction code	 1 byte		0x28
    //   Checksum		 2 bytes	Sum		(see top)
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x0003
    //   Confirmation code	 1 byte		xx		(see above)
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn GetImageEx(&mut self) -> Status {
	return self.send_command(Command::GetImageEx, 0 as u32);
    }

    // Description: Cancel instruction
    // Input Parameter: none
    // Return Parameter: Confirmation code
    //   Confirmation code=0x00: cancel setting successful;
    //   Confirmation code=other: cancel setting failed;
    // Instruction code: 30H
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x0003
    //   Instruction code	 1 byte		0x30
    //   Checksum		 2 bytes	Sum		(see top)
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x0003
    //   Confirmation code	 1 byte		xx		(see above)
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn Cancel(&mut self) -> Status {
	return self.send_command(Command::Cancel, 0 as u32);
    }

    // Description: Send handshake instructions to the module. If the module works normally, the
    //              confirmation code 0x00 will be returned. The upper computer can continue to
    //              send instructions to the module.If the confirmation code is other or no reply,
    //              it means that the device is abnormal.
    // Input Parameter: none
    // Return Parameter: Confirmation code
    //   Confirmation code=0x00: the device is normal and can receive instructions;
    //   Confirmation code=other: the device is abnormal;
    // Instruction code: 40H
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x0003
    //   Instruction code	 1 byte		0x40
    //   Checksum		 2 bytes	Sum		(see top)
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x0003
    //   Confirmation code	 1 byte		xx		(see above)
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn HandShake(&mut self) -> Status {
	return self.send_command(Command::HandShake, 0 as u32);
    }

    // Description: Check whether the sensor is normal.
    // Input Parameter: none
    // Return Parameter: Confirmation code
    //   Confirmation code=0x00: the sensor is normal;
    //   Confirmation code=0x29: the sensor is abnormal;
    // Instruction code: 36H
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x0003
    //   Instruction code	 1 byte		0x36
    //   Checksum		 2 bytes	Sum		(see top)
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x0003
    //   Confirmation code	 1 byte		xx		(see above)
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn CheckSensor(&mut self) -> Status {
	return self.send_command(Command::CheckSensor, 0 as u32);
    }

    // Description: Get the algorithm library version.
    // Input Parameter: none
    // Return Parameter: Confirmation code (1 byte) + AlgVer (algorithm library version string)
    //   Confirmation code=0x00: success;
    //   Confirmation code=0x01: error when receiving package;
    // Instruction code: 39H
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x0003
    //   Instruction code	 1 byte		0x39
    //   Checksum		 2 bytes	Sum		(see top)
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x0023
    //   Confirmation code	 1 byte		xx		(see above)
    //   Data			32 bytes
    //     AlgVer		32 bytes
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn GetAlgVer(&mut self) -> Status {
	return self.send_command(Command::GetAlgVer, 0 as u32);
    }

    // Description: Get the firmware version.
    // Input Parameter: none
    // Return Parameter: Confirmation code + FwVer (Firmware version string)
    //   Confirmation code=0x00: success;
    //   Confirmation code=0x01: error when receiving package;
    // Instruction code: 3aH
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x0003
    //   Instruction code	 1 byte		0x3a
    //   Checksum		 2 bytes	Sum		(see top)
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x0023
    //   Confirmation code	 1 byte		xx		(see above)
    //   Data			32 bytes
    //     FwVer		32 bytes
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn GetFwVer(&mut self) -> Status {
	return self.send_command(Command::GetFwVer, 0 as u32);
    }

    // Description: Read product information.
    // Input Parameter: none
    // Return Parameter: Confirmation code + ProdInfo (product information)
    //   Confirmation code=0x00: success;
    //   Confirmation code=0x01: error when receiving package;
    // Instruction code: 3cH
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x0003
    //   Instruction code	 1 byte		0x3c
    //   Checksum		 2 bytes	Sum		(see top)
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x0031
    //   Confirmation code	 1 byte		xx		(see above)
    //   Data			46 bytes
    //     ProdInfo		46 bytes			(see documentation)
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn ReadProdInfo(&mut self) -> Status {
	return self.send_command(Command::ReadProdInfo, 0 as u32);
    }

    // Description: Send soft reset instruction to the module. If the module works normally, return
    //              confirmation code 0x00, and then perform reset operation.
    // Input Parameter: none
    // Return Parameter: Confirmation code (1 byte)
    //   Confirmation code=0x00: success;
    //   Confirmation code=other: device is abnormal
    // Instruction code: 3dH
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x0003
    //   Instruction code	 1 byte		0x3d
    //   Checksum		 2 bytes	Sum		(see top)
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x0003
    //   Confirmation code	 1 byte		xx		(see above)
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn SoftRst(&mut self) -> Status {
	return self.send_command(Command::SoftRst, 0 as u32);
    }

    // Description: Aura LED control
    // Input Parameter:
    //   Control code;
    //     0x01: Breathing light
    //     0x02: Flashing light
    //     0x03: Light Always on
    //     0x04: Light Always off
    //     0x05: Light gradually on
    //     0x06: Light gradually off
    //   Speed;
    //     0x00-0xff, 256 gears, Minimum 5s cycle.
    //   ColorIndex;
    //     0x01: Red
    //     0x02: Blue
    //     0x03: Purple
    //   Times
    //     Number of cycles: 0- infinite, 1-255.
    // Return Parameter: Confirmation code (1 byte)
    //   Confirmation code=0x00: success;
    //   Confirmation code=0x01: error when receiving package;
    // Instruction code: 35H
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x07
    //   Instruction code	 1 byte		0x35
    //   Data			 4 bytes
    //     Control code		 1 byte		Ctrl		(see above)
    //     Speed		 1 byte		Speed		(see above)
    //     Colour index		 1 byte		ColourIndex	(see above)
    //     Times		 1 byte		Times		(see above)
    //   Checksum		 2 bytes	Sum		(see top)
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x0003
    //   Confirmation code	 1 byte		xx		(see above)
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn AuraLedConfig(&mut self, ctrl: u8, speed: u8, colour: u8, times: i8) -> Status {
	// TODO: Merge `ctrl`, `speed`, `colour` and `times`.
	return self.send_command(Command::AuraLedConfig, ctrl as u32);
    }

    // ===== Other instructions

    // Description: Command the Module to generate a random number and return it to upper computer.
    // Input Parameter: none
    // Return Parameter: Confirmation code (1 byte)
    //   Confirmation code=00H: generation success;
    //   Confirmation code=01H: error when receiving package;
    // Instruction code: 14H
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x03
    //   Instruction code	 1 byte		0x14
    //   Checksum		 2 bytes	0x0018
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x07
    //   Confirmation code	 1 byte		xx		(see above)
    //   Data			 4 bytes
    //     Random Number	 4 bytes
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn GetRandomCode(&mut self) -> Status {
	return self.send_command(Command::GetRandomCode, 0 as u32);
    }

    // Description: read information page(512bytes)
    // Input Parameter: none
    // Return Parameter: Confirmation code (1 byte)
    //   Confirmation code=00H: ready to transfer the following data packet;
    //   Confirmation code=01H: error when receiving package;
    //   Confirmation code=0fH: can not transfer the following data packet;
    // Instruction code: 16H
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x03
    //   Instruction code	 1 byte		0x16
    //   Checksum		 2 bytes	Sum		(see top)
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x03
    //   Confirmation code	 1 byte		xx		(see above)
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn ReadInfPage(&mut self) -> Status {
	return self.send_command(Command::ReadInfPage, 0 as u32);
    }

    // Description: Upper computer to write data to the specified Flash page. Also see ReadNotepad.
    // Input Parameter: NotePageNum, user content (or data content)
    // Return Parameter: Confirmation code (1 byte)
    //   Confirmation code=00H: write success;
    //   Confirmation code=01H: error when receiving package;
    // Instruction code: 18H
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x36
    //   Instruction code	 1 byte		0x18
    //   Data			33 bytes
    //     PageNumber		 1 byte
    //     Content		32 bytes
    //   Checksum		 2 bytes	Sum		(see top)
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		0x03
    //   Confirmation code	 1 byte		xx		(see above)
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn WriteNotepad(&mut self, page: u8) -> Status {
	return self.send_command(Command::WriteNotepad, page as u32);
    }

    // Description: Read the specified page’s data content. Also see WriteNotepad.
    // Input Parameter: none
    // Return Parameter: Confirmation code (1 byte) + data content
    //   Confirmation code=00H: read success;
    //   Confirmation code=01H: error when receiving package;
    // Instruction code: 19H
    // Command Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x01
    //   Package Length		 2 byte		0x04
    //   Instruction code	 1 byte		0x19
    //   Data			 1 bytes
    //     PageNumber		 1 byte
    //   Checksum		 2 bytes	Sum		(see top)
    // Acknowledge Package format:
    //   Header			 2 bytes	0xEF01
    //   Address		 4 bytes	xxxxxx
    //   Package Identifier	 1 byte		0x07
    //   Package Length		 2 byte		3+32
    //   Confirmation code	 1 byte		xx		(see above)
    //   Data			32 bytes
    //     User Content		32 bytes
    //   Checksum		 2 bytes	Sum		(see top)
    pub async fn ReadNotepad(&mut self) -> Status {
	return self.send_command(Command::ReadNotepad, 0 as u32);
    }
}
