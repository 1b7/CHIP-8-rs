use rand::random;
use std::fs;

const DISPLAY_WIDTH: usize = 64;
const DISPLAY_HEIGHT: usize = 32;
const INSTRUCTION_SIZE: u16 = 2; // All instructions are 2 bytes long.

pub struct Machine {
    // Address of the next instruction to execute:
    program_counter: u16,

    // Note that the stack pointer is specified to be 8 bit, but this being larger
    // shouldn't matter, and avoids casting when indexing into the stack.
    stack_pointer: usize,
    stack: [u16; 16],

    // When non-zero, timers decrement at a rate of 60Hz.
    // When the sound_timer is non-zero, sound should be activated.
    delay_timer: u8,
    sound_timer: u8,

    // 16 general 8-bit registers, referred to as V0, V1, ..., VF.
    registers: [u8; 16],

    // Additional register, usually used to store memory addresses.
    register_i: u16,

    // General RAM; note that the first 512 bytes (up to address 0x200) are reserved.
    // The built-in hexadecimal font is stored in this reserved memory. 
    pub(crate) memory: [u8; 4096],

    // Display Buffer; holds the state of the display.
    pub display: [[u8; DISPLAY_WIDTH]; DISPLAY_HEIGHT],

    // The key buffer serves as a 'bitfield' of currently-pressed keys,
    // such that a key with value 0 being active is represented as 0b0000_0001.
    // E.g. a value of 0b1000_0011 would represent keys 0, 1, and F being pressed.
    key_buffer: u16,
}

impl Machine {
    /// Creates a new instance of a CHIP-8 interpreter and initialises its
    /// various registers and memory cells to a default state (typically 0).
    pub fn new() -> Self {
        let mut memory = [0; 4096];
        Self::load_font(&mut memory);

        Self { 
            program_counter: Default::default(), 
            stack_pointer: Default::default(), 
            stack: Default::default(), 
            delay_timer: Default::default(), 
            sound_timer: Default::default(), 
            registers: Default::default(), 
            register_i: Default::default(),
            memory,
            display: [[0; DISPLAY_WIDTH]; DISPLAY_HEIGHT],
            key_buffer: Default::default(),
        }
    }

    /// Prints the current state of th machine (current instruction, program counter,
    /// and registers) to the console.
    pub fn debug_print_state(&self) { 
        let instruction: u16 = (self.memory[self.program_counter as usize] as u16) << 8
            | (self.memory[(self.program_counter + 1) as usize] as u16);
        println!("[{:04X}] {:04X} :: {:?}", self.program_counter, instruction, self.registers);
    }
    
    /// Returns true if the buzzer should currently be sounding.
    pub fn buzzer_active(&self) -> bool { self.sound_timer > 0 }

    /// Decrement the delay and sound timers by 1 tick.
    pub fn decay_timers(&mut self) {
        self.delay_timer = self.delay_timer.saturating_sub(1);
        self.sound_timer = self.sound_timer.saturating_sub(1);
    }

    /// Register a key as being pressed.
    /// Keys are indicated as powers of 2, such that each of the 16 keys can be
    /// represented as 1 bit of a 16 bit integer. 
    /// I.e. key 0 = 0b0000_0001, key 1 = 0b0000_0010, and so on.
    pub fn key_down(&mut self, k: u16) { self.key_buffer |= k; }

    /// Unregister a key, marking it as no-longer being pressed.
    /// Keys are indicated as powers of 2, such that each of the 16 keys can be
    /// represented as 1 bit of a 16 bit integer.
    /// I.e. key 0 = 0b0000_0001, key 1 = 0b0000_0010, and so on.
    pub fn key_up(&mut self, k: u16) { self.key_buffer &= !k; }

    /// Calling this function causes the interpreter to read and execute the
    /// next instruction of the currently loaded program.
    pub fn cycle(&mut self) {
        let instruction: u16 = (self.memory[self.program_counter as usize] as u16) << 8
            | (self.memory[(self.program_counter + 1) as usize] as u16);

        let prefix = instruction >> 12;
        match prefix {
            0x0 => {
                match instruction {
                    // CLS: Clears the screen:
                    0x00E0 => {
                        for row in &mut self.display { 
                            for pixel in row { *pixel = 0; }
                        }
                    }

                    // RET: Return from the current function:
                    0x00EE => {
                        self.stack_pointer -= 1;
                        self.program_counter = self.stack[self.stack_pointer];
                    },

                    // There is also a 'jump' style instruction to call machine-language
                    // routines of the form 0XXX, but this is intentionally unsupported. 

                    _ => panic!("Unrecognised Instruction ({:04X})", instruction),
                };
                self.program_counter += INSTRUCTION_SIZE;
            },

            0x1 => {
                // JP: Set Program Counter to value of the instruction's
                //  least-significant 12 bits.
                self.program_counter = 0x0FFF & instruction;
            },

            0x2 => {
                // CALL: Call a subroutine at the address specified by the
                //  instruction's least-significant 12 bits.
                self.stack[self.stack_pointer] = self.program_counter;
                self.stack_pointer += 1;
                self.program_counter = 0x0FFF & instruction;
            },

            0x3 => {
                // SE: Test if Vx == value; skip the next instruction if true.
                let register = (0x000F & (instruction >> 8)) as usize;
                // The lowest 8 bits contains the value, so we can use a narrowing cast.
                let value = instruction as u8;

                self.program_counter += if value == self.registers[register] {
                    2 * INSTRUCTION_SIZE
                } else {
                    INSTRUCTION_SIZE
                };
            },

            0x4 => {
                // SNE: Test if Vx != value; skip the next instruction if true.
                let register: usize = (0x000F & (instruction >> 8)).into();
                let value = instruction as u8;
                
                self.program_counter += if value != self.registers[register] {
                    2 * INSTRUCTION_SIZE
                } else {
                    INSTRUCTION_SIZE
                };
            },

            0x5 => {
                // SE: Test if Vx == Vy; skip the next instruction if true.
                let rx: usize = (0x000F & (instruction >> 8)).into();
                let ry: usize = (0x000F & (instruction >> 4)).into();
                
                self.program_counter += if self.registers[rx] == self.registers[ry] {
                    2 * INSTRUCTION_SIZE
                } else {
                    INSTRUCTION_SIZE
                };
            },

            0x6 => {
                // LD: Load a value into a specified register.
                let dest: usize = (0x000F & (instruction >> 8)).into();
                let value = instruction as u8;
                self.registers[dest] = value;
                self.program_counter += INSTRUCTION_SIZE;
            },

            0x7 => {
                // ADD: Add a value to a specified register.
                let dest: usize = (0x000F & (instruction >> 8)).into();
                self.registers[dest] = self.registers[dest].wrapping_add(instruction as u8);
                self.program_counter += INSTRUCTION_SIZE;
            },

            0x8 => {
                // 0x8XYN instructions involve two registers, where X, Y are the
                // registers involved, and N specifies the operation to carry out.
                let least_significant_nibble = instruction & 0x000F;
                let rx: usize = (0x000F & (instruction >> 8)).into();
                let ry: usize = (0x000F & (instruction >> 4)).into();

                match least_significant_nibble {
                    // LD x, y: Insert value of register y into register x.
                    0x0 => { self.registers[rx] = self.registers[ry]; },

                    // OR x, y: Bitwise OR of x, y, store result in x.
                    0x1 => { self.registers[rx] |= self.registers[ry]; },
                    
                    // AND x, y: Bitwise AND of x, y, store result in x.
                    0x2 => { self.registers[rx] &= self.registers[ry]; },

                    // XOR x, y: Bitwise XOR of x, y, store result in x.
                    0x3 => { self.registers[rx] ^= self.registers[ry]; },
                    
                    // ADD x, y: Add x, y, store result in x, set VF = carry.
                    0x4 => { 
                        let (result, carry) = self.registers[rx].overflowing_add(self.registers[ry]);
                        self.registers[rx] = result;
                        self.registers[0xF] = carry.into();
                    },

                    // SUB x, y: Subtract y from x, store result in x.
                    // Also sets VF = 1 if x > y, else 0.
                    0x5 => {
                        let carry = (self.registers[rx] >= self.registers[ry]).into();
                        self.registers[rx] = self.registers[rx].wrapping_sub(self.registers[ry]);
                        self.registers[0xF] = carry;
                    },

                    // SHR x: Sets VF = 1 if x's LSB was 1, else 0, then shifts x right by 1 bit. 
                    // Register y is ignored.
                    0x6 => {
                        let carry = self.registers[rx] & 1;
                        self.registers[rx] >>= 1;
                        self.registers[0xF] = carry;
                    },

                    // SUBN x, y: Subtract x from y, store result in x.
                    // Also sets VF = 1 if y > x, else 0.
                    0x7 => {
                        let carry = (self.registers[ry] >= self.registers[rx]).into();
                        self.registers[rx] = self.registers[ry].wrapping_sub(self.registers[rx]);
                        self.registers[0xF] = carry;
                    },

                    // SHL x: Sets VF = 1 if x's MSB is 1, else 0, then shifts x left by 1 bit. 
                    // Register y is ignored.
                    0xE => {
                        let carry = ((self.registers[rx] & 0b1000_0000) > 0).into();
                        self.registers[rx] <<= 1;
                        self.registers[0xF] = carry;
                    },

                    _ => panic!("Unrecognised instruction code")
                };

                self.program_counter += INSTRUCTION_SIZE;
            },

            0x9 => {
                // SNE x, y: Skip the next instruction if the value of register x != register y.
                let rx: usize = (0x000F & (instruction >> 8)).into();
                let ry: usize = (0x000F & (instruction >> 4)).into();
                
                self.program_counter += if self.registers[rx] != self.registers[ry] {
                    2 * INSTRUCTION_SIZE
                } else {
                    INSTRUCTION_SIZE
                };

            },

            0xA => {
                // LD I, addr: Load an address into register I.
                self.register_i = instruction & 0x0FFF;
                self.program_counter += INSTRUCTION_SIZE;
            },

            0xB => {
                // JP V0, nnn: Jumps to the location of V0 + nnn, where nnn are
                // the last 3 nibbles of the instruction.
                self.program_counter = self.registers[0x0000] as u16 + (instruction & 0x0FFF);
            },

            0xC => {
                // RND Vx, kk: Generates a random byte, ANDs it with kk, stores
                // the value in Vx.
                let register: usize = (0x000F & (instruction >> 8)).into();
                
                self.registers[register] = random::<u8>() & (instruction as u8);
                self.program_counter += INSTRUCTION_SIZE;
            },

            0xD => {
                // DRW Vx, Vy, n: Display an n-byte sprite starting from the address
                // in register I, starting from the position held in registers Vx, Vy.
                // VF is set to 1 if this causes any pixels to be erased.
                //
                // Drawing is carried out by XORing the sprite onto the screen.
                // If any pixels would be drawn off-screen, they re-appear on the 
                // opposite side of the screen.

                let n_bytes = instruction & 0x000F;

                let rx: usize = (0x000F & (instruction >> 8)).into();
                let start_col: usize = (self.registers[rx] % DISPLAY_WIDTH as u8).into();

                let ry: usize = (0x000F & (instruction >> 4)).into();
                let start_row: usize = (self.registers[ry] % DISPLAY_HEIGHT as u8).into();
                
                // Initialise erase flag:
                self.registers[0xF] = 0;
                for byte in 0..n_bytes {
                    let sprite_row = self.memory[(self.register_i + byte) as usize];
                    for bit in 0..8 {
                        // Test if this pixel of the sprite is 'on' (i.e. bit == 1):
                        let pixel_on: u8 = ((sprite_row & (1 << (7 - bit))) > 0).into();

                        let row = start_row + byte as usize;
                        let col = start_col + bit;

                        if row < DISPLAY_HEIGHT && col < DISPLAY_WIDTH {
                            // Set erase flag if a pixel will be erased:
                            self.registers[0xF] |= ((pixel_on != 0) & (self.display[row][col] != 0)) as u8;
                            self.display[row][col] ^= pixel_on;
                        }
                    }
                }

                // self.display_debug();
                self.program_counter += INSTRUCTION_SIZE;
            },

            0xE => {
                let least_sig_byte = instruction & 0x00FF;

                match least_sig_byte {
                    0x9E => {
                        // SKP Vx: Skip the next instruction if the key with value
                        // Vx is currently pressed.
                        let register: usize = (0x000F & (instruction >> 8)).into();
                        let key_pressed = (self.key_buffer & (1 << self.registers[register])) > 0;

                        self.program_counter += if key_pressed {
                            2 * INSTRUCTION_SIZE
                        } else {
                            INSTRUCTION_SIZE
                        };
                    },

                    0xA1 => {
                        // SKP Vx: Skip the next instruction if the key with value
                        // Vx is NOT currently pressed.
                        let register: usize = (0x000F & (instruction >> 8)).into();
                        let key_pressed = (self.key_buffer & (1 << self.registers[register])) > 0;
                        
                        self.program_counter += if !key_pressed {
                            2 * INSTRUCTION_SIZE
                        } else {
                            INSTRUCTION_SIZE
                        };
                    },

                    _ => panic!("Unrecognised instruction code: {:04X}", instruction)
                };
            },

            0xF => {
                let least_sig_byte = instruction & 0x00FF;
                let rx: usize = (0x000F & (instruction >> 8)).into();

                match least_sig_byte {
                    0x07 => {
                        // LD Vx, DT - Set Vx to the value of the delay timer.
                        self.registers[rx] = self.delay_timer;
                        self.program_counter += INSTRUCTION_SIZE;
                    },

                    0x0A => {
                        // Wait for a key press, store the value of the key in Vx;
                        // Halt execution until key is pressed:
                        let key_pressed = (self.key_buffer & (1 << self.registers[rx])) > 0;
                        if key_pressed { self.program_counter += INSTRUCTION_SIZE }
                    },

                    0x15 => {
                        // LD DT, Vx: Set the Delay timer to the value of Vx.
                        self.delay_timer = self.registers[rx];
                        self.program_counter += INSTRUCTION_SIZE;
                    },

                    0x18 => {
                        // LD ST, Vx: Set the Sound timer to the value of Vx.
                        self.sound_timer = self.registers[rx];
                        self.program_counter += INSTRUCTION_SIZE;
                    },

                    0x1E => {
                        // ADD I, Vx: Set I = I + Vx
                        self.register_i += self.registers[rx] as u16;
                        self.program_counter += INSTRUCTION_SIZE;
                    },

                    0x29 => {
                        // LD F, Vx: Set I to the location of the sprite for digit Vx.
                        // Note that fonts are stored starting from index 0,
                        // each taking up 5 bytes total.
                        // Thus, '0' is found starting at bytes 0 through 4, 
                        // '1' at 5 through 9, and so on.
                        self.register_i = self.registers[rx] as u16 * 5;
                        self.program_counter += INSTRUCTION_SIZE;
                    },

                    0x33 => {
                        // LD B, Vx: Store the Binary-Coded Decimal representation
                        // of Vx. Hundreds digit -> I, tens -> I+1, ones -> I+2.
                        let mut vx = self.registers[rx];
                        self.memory[self.register_i as usize + 2] = vx % 10;

                        vx /= 10;
                        self.memory[self.register_i as usize + 1] = vx % 10;
                        
                        vx /= 10;
                        self.memory[self.register_i as usize] = vx % 10;
                        
                        self.program_counter += INSTRUCTION_SIZE;
                    },

                    0x55 => {
                        // LD [I], Vx: Stores registers V0 through Vx in memory,
                        // starting from the location in I.
                        for i in 0..=rx {
                            self.memory[self.register_i as usize + i] = self.registers[i];
                        }

                        self.program_counter += INSTRUCTION_SIZE;
                    },

                    0x65 => {
                        // LD Vx, [I]: Reads registers V0 through Vx from memory,
                        // starting from the location in I.
                        for i in 0..=rx {
                            self.registers[i] = self.memory[self.register_i as usize + i];
                        }

                        self.program_counter += INSTRUCTION_SIZE;
                    }

                    _ => panic!("Unrecognised instruction code: {:04X}", instruction)
                }
            }

            _ => panic!("Unrecognised instruction code: {:04X}", instruction)
        };
    }

    /// Insert the default font for hexadecimal characters into reserved memory,
    /// starting from address 0x0000.
    fn load_font(memory: &mut [u8; 4096]) {
        // Sprites are monochromatic, 8px wide, and up to 15px tall.
        // This is represented as a series of up-to 15 bytes.
        const HEX_FONT: [[u8; 5]; 16] = [
            [0xF0, 0x90, 0x90, 0x90, 0xF0], // 0
            [0x20, 0x60, 0x20, 0x20, 0x70], // 1
            [0xF0, 0x10, 0xF0, 0x80, 0xF0], // 2
            [0xF0, 0x10, 0xF0, 0x10, 0xF0], // 3
            [0x90, 0x90, 0xF0, 0x10, 0x10], // 4
            [0xF0, 0x80, 0xF0, 0x10, 0xF0], // 5
            [0xF0, 0x80, 0xF0, 0x90, 0xF0], // 6
            [0xF0, 0x10, 0x20, 0x40, 0x40], // 7
            [0xF0, 0x90, 0xF0, 0x90, 0xF0], // 8
            [0xF0, 0x90, 0xF0, 0x10, 0xF0], // 9
            [0xF0, 0x90, 0xF0, 0x90, 0x90], // A
            [0xE0, 0x90, 0xE0, 0x90, 0xE0], // B
            [0xF0, 0x80, 0x80, 0x80, 0xF0], // C
            [0xE0, 0x90, 0x90, 0x90, 0xE0], // D
            [0xF0, 0x80, 0xF0, 0x80, 0xF0], // E
            [0xF0, 0x80, 0xF0, 0x80, 0x80], // F
        ];

        for digit in 0..16 {
            for r in 0..5 {
                memory[digit * 5 + r] = HEX_FONT[digit][r];
            }
        }
    }
    
    /// Load a program from a file into the virtual machine's memory.
    pub fn load_program(&mut self, path: &str) {
        let mem_base = 0x200;
        if let Ok(bytes) = fs::read(path) {
            self.memory[mem_base..(bytes.len() + mem_base)].copy_from_slice(&bytes[..]);
            self.program_counter = mem_base as u16;
        } else {
            panic!("File `{path}` could not be read.")
        }
    }
    
    /// Prints a representation of the system's display buffer to the console.
    fn debug_print_display(&self) {
        for row in 0..self.display.len() {
            for col in 0..self.display[row].len() {
                print!("{}", if self.display[row][col] > 0 { '█' } else { ' ' });
            }
            println!();
        }
    }
}

// Note: Unit tests for the CHIP-8 system are included in this file rather than
// a separate testing file because the alternative would necessitate giving the
// functions tested less strict access modifiers (i.e. pub(crate) instead of private).
mod test {
    use super::*;

    #[test]
    /// Test instruction 0x00E0.
    fn test_clear_screen() {
        let mut chip8 = Machine::new();

        // Set some pixels as 'on':
        chip8.display[0][0] = 1;
        chip8.display[0][DISPLAY_WIDTH - 1] = 1;
        chip8.display[DISPLAY_HEIGHT - 1][0] = 1;
        chip8.display[DISPLAY_HEIGHT - 1][DISPLAY_WIDTH - 1] = 1;
        
        // Set the program as a single clear display instruction, and run it:
        chip8.program_counter = 0x200;
        chip8.memory[0x200] = 0x00;
        chip8.memory[0x201] = 0xE0;
        chip8.cycle();

        // Test that all pixels in the display buffer are now switched off.
        for row in chip8.display {
            for pixel in row { assert_eq!(pixel, 0); }
        }
    }

    #[test]
    /// Test instruction 0x1nnn.
    fn test_jump() {
        let mut chip8 = Machine::new();
        
        // Set the program as a single jump instruction, and run it:
        chip8.program_counter = 0x200;
        chip8.memory[0x200] = 0x13;
        chip8.memory[0x201] = 0x00;
        chip8.cycle();

        assert_eq!(chip8.program_counter, 0x0300);
        assert_eq!(chip8.stack_pointer, 0);
    }

    #[test]
    /// Test instructions 0x2nnn and 0x00EE.
    fn test_call_ret() {
        let mut chip8 = Machine::new();

        // Insert RET instruction:
        chip8.memory[0x300] = 0x00;
        chip8.memory[0x301] = 0xEE;
        
        // Insert CALL instruction, directing to procedure at 0x300:
        chip8.memory[0x200] = 0x23;
        chip8.memory[0x201] = 0x00;

        chip8.program_counter = 0x200;
        assert_eq!(chip8.stack[0], 0x000);

        // Execute CALL, check that PC/Stack have been updated:
        chip8.cycle();
        assert_eq!(chip8.program_counter, 0x300);
        assert_eq!(chip8.stack_pointer, 1);
        assert_eq!(chip8.stack[0], 0x200);

        // Execute RETurn, check that the return address has been popped and
        // execution picks up after the CALL instruction: 
        chip8.cycle();
        assert_eq!(chip8.program_counter, 0x202);
        assert_eq!(chip8.stack_pointer, 0);
    }

    #[test]
    /// Test instruction 3xkk.
    fn test_skip_equal_literal() {
        let mut chip8 = Machine::new();
        
        // Test whether register D (which should currently have a value of 0)
        // is equal to 8. This should fail, and the next instruction should not be
        // skipped.
        chip8.memory[0x200] = 0x3D;
        chip8.memory[0x201] = 0x08;
        chip8.program_counter = 0x200;
        chip8.cycle();
        assert_eq!(chip8.program_counter, 0x202);

        // Rerun the same instruction but with register E, which does contain 8.
        // In this case, the program counter should be increased such that the
        // next instruction is skipped.
        chip8.registers[0xE] = 8;
        chip8.memory[0x200] = 0x3E;
        chip8.program_counter = 0x200;
        chip8.cycle();
        assert_eq!(chip8.program_counter, 0x204);
    }
    
    #[test]
    /// Test instruction 4xkk.
    fn test_skip_not_equal_literal() {
        let mut chip8 = Machine::new();
        
        // Test whether register D (which should currently have a value of 0)
        // is equal to 8. This will not be true, and the PC should be increased
        // such that the next instruction is skipped.
        chip8.memory[0x200] = 0x4D;
        chip8.memory[0x201] = 0x08;
        chip8.program_counter = 0x200;
        chip8.cycle();
        assert_eq!(chip8.program_counter, 0x204);

        // Rerun the same instruction but with register E, which does contain 8.
        // In this case, the next instruction should not be skipped.
        chip8.registers[0xE] = 8;
        chip8.memory[0x200] = 0x4E;
        chip8.program_counter = 0x200;
        chip8.cycle();
        assert_eq!(chip8.program_counter, 0x202);
    }

    #[test]
    /// Test instruction 5xy0.
    fn test_skip_equal_register() {
        let mut chip8 = Machine::new();
        
        chip8.registers[0x0] = 1;

        // Compare registers 0 and 1 - which are unequal; the next instruction
        // should not be skipped.
        chip8.memory[0x200] = 0x50;
        chip8.memory[0x201] = 0x10;
        chip8.program_counter = 0x200;
        chip8.cycle();
        assert_eq!(chip8.program_counter, 0x202);
        
        chip8.program_counter = 0x200;
        // Compare registers 1 and 2 - which are equal; the next instruction
        // should be skipped.
        chip8.memory[0x200] = 0x51;
        chip8.memory[0x201] = 0x20;
        chip8.program_counter = 0x200;
        chip8.cycle();
        assert_eq!(chip8.program_counter, 0x204);
    }

    #[test]
    /// Test instruction 6xkk.
    fn test_load_literal() {
        // Try to load value 0xFF into register 7:
        let mut chip8 = Machine::new();
        chip8.memory[0x200] = 0x67;
        chip8.memory[0x201] = 0xFF;
        chip8.program_counter = 0x200;
        chip8.cycle();
        assert_eq!(chip8.registers, [0, 0, 0, 0, 0, 0, 0, 0xFF, 0, 0, 0, 0, 0, 0, 0, 0]);
    }

    #[test]
    /// Test instruction 7xkk.
    fn test_add_literal() {
        let mut chip8 = Machine::new();
        assert_eq!(chip8.registers[1], 0);

        // Test program: Add 15 twice, then add 255.
        chip8.memory[0x200] = 0x71;
        chip8.memory[0x201] = 0x0F;
        chip8.memory[0x202] = 0x71;
        chip8.memory[0x203] = 0x0F;
        chip8.memory[0x204] = 0x71;
        chip8.memory[0x205] = 0xFF;
        chip8.program_counter = 0x200;

        chip8.cycle();
        assert_eq!(chip8.registers[1], 0x0F);
        
        chip8.cycle();
        assert_eq!(chip8.registers[1], 0x1E);

        // Overflowing addition; 0x1E + 0xFF = 0x11D, but we only have an 8 bit
        // register so the extra nibble is lost. 
        chip8.cycle();
        assert_eq!(chip8.registers[1], 0x1D);
    }

    #[test]
    /// Test instruction 8xy0.
    fn test_load_register() {
        // Try to load value 0xFF into register 7:
        let mut chip8 = Machine::new();
        chip8.registers[1] = 0x0A;
        chip8.registers[2] = 0x0C;

        chip8.memory[0x200] = 0x81;
        chip8.memory[0x201] = 0x20;
        chip8.program_counter = 0x200;
        chip8.cycle();

        assert_eq!(chip8.registers[1], 0x0C);
    }

    #[test]
    /// Test instruction 8xy1.
    fn test_bitwise_or() {
        // Set up registers and expectation:
        let mut chip8 = Machine::new();
        chip8.registers[1] = 0b1100;
        chip8.registers[2] = 0b1010;
        let expectation    = 0b1110;

        // Execute bitwise OR:
        chip8.memory[0x200] = 0x81;
        chip8.memory[0x201] = 0x21;
        chip8.program_counter = 0x200;
        chip8.cycle();

        assert_eq!(chip8.registers[1], expectation);
    }

    #[test]
    /// Test instruction 8xy2.
    fn test_bitwise_and() {
        // Set up registers and expectation:
        let mut chip8 = Machine::new();
        chip8.registers[1] = 0b1100;
        chip8.registers[2] = 0b1010;
        let expectation    = 0b1000;

        // Execute bitwise AND:
        chip8.memory[0x200] = 0x81;
        chip8.memory[0x201] = 0x22;
        chip8.program_counter = 0x200;
        chip8.cycle();

        assert_eq!(chip8.registers[1], expectation);
    }
    
    #[test]
    /// Test instruction 8xy3.
    fn test_bitwise_xor() {
        // Set up registers and expectation:
        let mut chip8 = Machine::new();
        chip8.registers[1] = 0b1100;
        chip8.registers[2] = 0b1010;
        let expectation    = 0b0110;

        // Execute bitwise XOR:
        chip8.memory[0x200] = 0x81;
        chip8.memory[0x201] = 0x23;
        chip8.program_counter = 0x200;
        chip8.cycle();

        assert_eq!(chip8.registers[1], expectation);
    }
    
    #[test]
    /// Test instruction 8xy4.
    fn test_add_register() {
        let mut chip8 = Machine::new();
        assert_eq!(chip8.registers[1], 0);
        chip8.registers[0x2] = 0x0F;
        chip8.registers[0xE] = 0xFF;

        // Test program: Add register 2 (value of 0x0F) twice.
        chip8.memory[0x200] = 0x80;
        chip8.memory[0x201] = 0x24;
        chip8.memory[0x202] = 0x80;
        chip8.memory[0x203] = 0x24;

        // Then add register E (value of 0xFF) twice.
        chip8.memory[0x204] = 0x80;
        chip8.memory[0x205] = 0xE4;
        chip8.program_counter = 0x200;

        chip8.cycle();
        assert_eq!(chip8.registers[0x0], 0x0F);
        assert_eq!(chip8.registers[0xF], 0x00);
        
        chip8.cycle();
        assert_eq!(chip8.registers[0x0], 0x1E);
        assert_eq!(chip8.registers[0xF], 0x00);

        // Overflowing addition; 0x1E + 0xFF = 0x11D, but we only have 8 bit registers
        // so the extra nibble is lost; this sets the carry bit in register F.
        chip8.cycle();
        assert_eq!(chip8.registers[0x0], 0x1D);
        assert_eq!(chip8.registers[0xF], 0x01);
    }
    
    #[test]
    /// Test instruction 8xy5.
    fn test_sub_register() {
        let mut chip8 = Machine::new();
        assert_eq!(chip8.registers[1], 0);
        chip8.registers[0x0] = 0x0A;
        chip8.registers[0x1] = 0x06;

        // Test program: Subtract register 1 from 2 twice:
        chip8.program_counter = 0x200;
        chip8.memory[0x200] = 0x80;
        chip8.memory[0x201] = 0x15;
        chip8.memory[0x202] = 0x80;
        chip8.memory[0x203] = 0x15;

        chip8.cycle();
        assert_eq!(chip8.registers[0x0], 0x04);
        // The carry flag should be set if rx > ry.
        assert_eq!(chip8.registers[0xF], 0x01);
        
        chip8.cycle();
        assert_eq!(chip8.registers[0x0], 0xFE);
        assert_eq!(chip8.registers[0xF], 0x00);
    }

    #[test]
    /// Test instruction 8xy6.
    fn test_shift_right() {
        let mut chip8 = Machine::new();
        chip8.registers[0x0] = 0b1010;

        chip8.program_counter = 0x200;
        chip8.memory[0x200] = 0x80;
        chip8.memory[0x201] = 0x16;
        chip8.memory[0x202] = 0x80;
        chip8.memory[0x203] = 0x16;

        chip8.cycle();
        assert_eq!(chip8.registers[0x0], 0b0101);
        assert_eq!(chip8.registers[0xF], 0);

        chip8.cycle();
        assert_eq!(chip8.registers[0x0], 0b0010);
        assert_eq!(chip8.registers[0xF], 1);
    }
    
    #[test]
    /// Test instruction 8xy7.
    fn test_subn_register() {
        let mut chip8 = Machine::new();
        assert_eq!(chip8.registers[1], 0);
        chip8.registers[0x0] = 0x06;
        chip8.registers[0x1] = 0x0A;

        // Test program: Subtract register 1 from 2:
        chip8.program_counter = 0x200;
        chip8.memory[0x200] = 0x80;
        chip8.memory[0x201] = 0x17;

        chip8.cycle();
        assert_eq!(chip8.registers[0x0], 0x04);
        // The carry flag should be set only if ry > rx.
        assert_eq!(chip8.registers[0xF], 0x01);

        chip8.registers[0x1] = 0x03;
        chip8.program_counter = 0x200;
        chip8.memory[0x200] = 0x80;
        chip8.memory[0x201] = 0x17;
        chip8.cycle();
        
        assert_eq!(chip8.registers[0x0], 0xFF);
        assert_eq!(chip8.registers[0xF], 0x00);
    }

    #[test]
    /// Test instruction 8xyE.
    fn test_shift_left() {
        let mut chip8 = Machine::new();
        chip8.registers[0x0] = 0b1000_0001;

        chip8.program_counter = 0x200;
        chip8.memory[0x200] = 0x80;
        chip8.memory[0x201] = 0x1E;
        chip8.memory[0x202] = 0x80;
        chip8.memory[0x203] = 0x1E;

        chip8.cycle();
        assert_eq!(chip8.registers[0x0], 0b0000_0010);
        assert_eq!(chip8.registers[0xF], 1);

        chip8.cycle();
        assert_eq!(chip8.registers[0x0], 0b0000_0100);
        assert_eq!(chip8.registers[0xF], 0);
    }

    #[test]
    /// Test instruction 9xy0.
    fn test_skip_not_equal_register() {
        let mut chip8 = Machine::new();
        chip8.registers[0x0] = 1;
        
        // Test whether register 0 (which should currently have a value of 1)
        // is equal to register 1 (value of 0).
        // This will not be true, and the PC should be increased such that the
        // next instruction is skipped.
        chip8.program_counter = 0x200;
        chip8.memory[0x200] = 0x90;
        chip8.memory[0x201] = 0x10;
        chip8.cycle();
        assert_eq!(chip8.program_counter, 0x204);

        // Rerun the same instruction but with registers 1 and 2, which each contain 0.
        // In this case, the next instruction should not be skipped.
        chip8.memory[0x200] = 0x92;
        chip8.program_counter = 0x200;
        chip8.cycle();
        assert_eq!(chip8.program_counter, 0x202);
    }

    #[test]
    /// Test instruction Annn.
    fn test_set_register_i() {
        let mut chip8 = Machine::new();
        assert_eq!(chip8.register_i, 0);

        // Try to set Register I to 0x0123. 
        chip8.program_counter = 0x200;
        chip8.memory[0x200] = 0xA1;
        chip8.memory[0x201] = 0x23;
        chip8.cycle();
        assert_eq!(chip8.register_i, 0x123);
    }
    
    #[test]
    /// Test instruction Bnnn.
    fn test_jump_add_i() {
        let mut chip8 = Machine::new();
        assert_eq!(chip8.register_i, 0);
        chip8.registers[0] = 0x021;

        // Try to add 0x300 to register 0 (0x021), and jump to this address.
        chip8.program_counter = 0x200;
        chip8.memory[0x200] = 0xB3;
        chip8.memory[0x201] = 0x00;
        chip8.cycle();
        assert_eq!(chip8.registers[0], 0x021);
        assert_eq!(chip8.program_counter, 0x321);
    }

    #[test]
    /// test instruction Dxyn.
    fn test_draw() {
        let mut chip8 = Machine::new();
        
        // Define a byte-long sprite, which is effectively a 1px x 1px dot.
        chip8.memory[0x300] = 0b1000_0000;
        chip8.memory[0x301] = 0b0000_0000; // Padding.
        chip8.register_i = 0x300;
        
        chip8.registers[2] = 63;
        chip8.registers[3] = 31;

        // Draw sprites in the top-left and bottom-right corners:
        chip8.program_counter = 0x200;
        chip8.memory[0x200] = 0xD0;
        chip8.memory[0x201] = 0x11;
        chip8.memory[0x202] = 0xD2;
        chip8.memory[0x203] = 0x31;
        chip8.memory[0x204] = 0xD0;
        chip8.memory[0x205] = 0x11;

        chip8.cycle();

        assert_eq!(chip8.display[0][0], 1);
        assert_eq!(chip8.display[0][1], 0);
        assert_eq!(chip8.display[1][0], 0);
        assert_eq!(chip8.registers[0xF], 0);
        
        chip8.cycle();
        assert_eq!(chip8.display[31][63], 1);
        assert_eq!(chip8.display[31][62], 0);
        assert_eq!(chip8.display[30][63], 0);
        assert_eq!(chip8.display[0][0], 1);
        assert_eq!(chip8.display[0][1], 0);
        assert_eq!(chip8.display[1][0], 0);
        assert_eq!(chip8.registers[0xF], 0);
        
        // Redrawing the top-left sprite deletes it (due to XOR behaviour):
        chip8.cycle();
        assert_eq!(chip8.display[31][63], 1);
        assert_eq!(chip8.display[31][62], 0);
        assert_eq!(chip8.display[30][63], 0);
        assert_eq!(chip8.display[0][0], 0);
        assert_eq!(chip8.display[0][1], 0);
        assert_eq!(chip8.display[1][0], 0);
        assert_eq!(chip8.registers[0xF], 1);
    }

    #[test]
    /// Test instruction Ex9E.
    fn test_skip_if_key_pressed() {
        let mut chip8 = Machine::new();

        // The relevant bit for key 1 is the second-least-significant bit.
        chip8.key_buffer = 0b10;
        chip8.registers[1] = 1;

        // Check if key with value of R1 is pressed;
        // this is true, so the PC should skip an instruction.
        chip8.program_counter = 0x200;
        chip8.memory[0x200] = 0xE1;
        chip8.memory[0x201] = 0x9E;
        chip8.cycle();
        assert_eq!(chip8.program_counter, 0x204);
        
        // If we instead test against the value of R0, this key isn't pressed,
        // so the PC should not skip forward.
        chip8.program_counter = 0x200;
        chip8.memory[0x200] = 0xE0;
        chip8.cycle();
        assert_eq!(chip8.program_counter, 0x202);
    }
    
    #[test]
    /// Test instruction ExA1.
    fn test_skip_if_key_not_pressed() {
        let mut chip8 = Machine::new();

        // The relevant bit for key 1 is the second-least-significant bit.
        chip8.key_buffer = 0b10;
        chip8.registers[1] = 1;

        // Check if key with value of R1 is pressed;
        // this is true, so the PC should not skip forward.
        chip8.program_counter = 0x200;
        chip8.memory[0x200] = 0xE1;
        chip8.memory[0x201] = 0xA1;
        chip8.cycle();
        assert_eq!(chip8.program_counter, 0x202);
        
        // If we instead test against the value of R0, this key isn't pressed,
        // so the PC should now skip ahead one instruction.
        chip8.program_counter = 0x200;
        chip8.memory[0x200] = 0xE0;
        chip8.cycle();
        assert_eq!(chip8.program_counter, 0x204);
    }

    #[test]
    /// Test instruction Fx07.
    fn test_get_delay_timer() {
        let mut chip8 = Machine::new();

        // Place 60 into the delay timer and attempt to load this into R0:
        chip8.delay_timer = 60;
        chip8.program_counter = 0x200;
        chip8.memory[0x200] = 0xF0;
        chip8.memory[0x201] = 0x07;
        chip8.cycle();
        assert_eq!(chip8.registers[0], 60);
    }

    
    #[test]
    /// Test instruction Fx0A.
    fn test_halt_until_key_pressed() {
        let mut chip8 = Machine::new();

        chip8.registers[1] = 1;
        chip8.program_counter = 0x200;
        chip8.memory[0x200] = 0xF1;
        chip8.memory[0x201] = 0x0A;

        // No key is pressed, so F10A shouldn't alter the PC:
        chip8.cycle();
        assert_eq!(chip8.program_counter, 0x200);
        
        // The relevant bit for key 1 is the second-least-significant bit.
        chip8.key_buffer = 0b10;

        // Key 1 is now pressed, so F10A should allow execution to continue:
        chip8.cycle();
        assert_eq!(chip8.program_counter, 0x202);
    }


    #[test]
    /// Test instruction Fx15.
    fn test_set_delay_timer() {
        let mut chip8 = Machine::new();

        // Place 60 into R0 and attempt to load this as the delay timer:
        chip8.registers[0] = 60;
        chip8.program_counter = 0x200;
        chip8.memory[0x200] = 0xF0;
        chip8.memory[0x201] = 0x15;
        chip8.cycle();
        assert_eq!(chip8.delay_timer, 60);
    }

    #[test]
    /// Test instruction Fx18.
    fn test_set_sound_timer() {
        let mut chip8 = Machine::new();

        // Place 60 into R0 and attempt to load this as the sound timer:
        chip8.registers[0] = 60;
        chip8.program_counter = 0x200;
        chip8.memory[0x200] = 0xF0;
        chip8.memory[0x201] = 0x18;
        chip8.cycle();
        assert_eq!(chip8.sound_timer, 60);
    }
    
    #[test]
    /// Test instruction Fx1E.
    fn test_add_register_to_i() {
        let mut chip8 = Machine::new();
        chip8.register_i = 0x10;

        // Place 60 into R0 and attempt to load this as the delay timer:
        chip8.registers[0] = 0x5;
        chip8.program_counter = 0x200;
        chip8.memory[0x200] = 0xF0;
        chip8.memory[0x201] = 0x1E;
        chip8.cycle();
        assert_eq!(chip8.register_i, 0x15);
    }
    
    #[test]
    /// Test instruction Fx29.
    fn test_load_hex_font_address() {
        let mut chip8 = Machine::new();

        // Attempt to load the sprite for 'F':
        chip8.registers[2] = 0xF;
        chip8.program_counter = 0x200;
        chip8.memory[0x200] = 0xF2;
        chip8.memory[0x201] = 0x29;
        chip8.cycle();
        assert_eq!(chip8.register_i, 75);
    }

    #[test]
    /// Test instruction Fx33.
    fn test_store_bcd() {
        let mut chip8 = Machine::new();

        // Attempt to load the sprite for 'F':
        chip8.registers[0] = 123;
        chip8.register_i = 0x300;

        chip8.program_counter = 0x200;
        chip8.memory[0x200] = 0xF0;
        chip8.memory[0x201] = 0x33;
        chip8.cycle();

        assert_eq!(chip8.memory[0x300], 1);
        assert_eq!(chip8.memory[0x300 + 1], 2);
        assert_eq!(chip8.memory[0x300 + 2], 3);
    }
    
    #[test]
    /// Test instruction Fx55.
    fn test_save_registers() {
        let mut chip8 = Machine::new();
        chip8.register_i = 0x300;

        // Initialise registers with values:
        (0..16).for_each(|i| chip8.registers[i] = 2 * i as u8);
        
        // Execute save instruction:
        chip8.program_counter = 0x200;
        chip8.memory[0x200] = 0xF9;
        chip8.memory[0x201] = 0x55;
        chip8.cycle();
        
        (0..10).for_each(|i| assert_eq!(chip8.memory[chip8.register_i as usize + i], 2 * i as u8));
        (10..16).for_each(|i| assert_eq!(chip8.memory[chip8.register_i as usize + i], 0));
    }
    
    #[test]
    /// Test instruction Fx65.
    fn test_load_registers() {
        let mut chip8 = Machine::new();
        chip8.register_i = 0x300;

        // Initialise memory with values:
        (0..16).for_each(|i| chip8.memory[chip8.register_i as usize + i] = 2 * i as u8);
        
        // Execute save instruction:
        chip8.program_counter = 0x200;
        chip8.memory[0x200] = 0xF9;
        chip8.memory[0x201] = 0x65;
        chip8.cycle();
        
        (0..10).for_each(|i| assert_eq!(chip8.registers[i], 2 * i as u8));
        (10..16).for_each(|i| assert_eq!(chip8.registers[i], 0));
    }
}