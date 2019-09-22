use rand::prelude::*;

use crate::cpu::Instruction::*;

const SPRITE_SIZE: usize = 5;
const FONTS: [u8; SPRITE_SIZE * 16] = [
    0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
    0x20, 0x60, 0x20, 0x20, 0x70, // 1
    0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
    0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
    0x90, 0x90, 0xF0, 0x10, 0x10, // 4
    0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
    0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
    0xF0, 0x10, 0x20, 0x40, 0x40, // 7
    0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
    0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
    0xF0, 0x90, 0xF0, 0x90, 0x90, // A
    0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
    0xF0, 0x80, 0x80, 0x80, 0xF0, // C
    0xE0, 0x90, 0x90, 0x90, 0xE0, // D
    0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
    0xF0, 0x80, 0xF0, 0x80, 0x80, // F
];

const MEM_FONTS_START: usize = 0x50;
const MEM_PROGRAM_START: usize = 0x200;
const MEM_END: usize = 0xFFF;

pub const VIDEO_WIDTH: u32 = 64;
pub const VIDEO_HEIGHT: u32 = 32;

type Register = usize;
type Memory = usize;

// @formatter:off
#[derive(Debug)]
enum Instruction {
    // instruction in hexadecimal
    CLS {}, // 00 E0
    RTS {}, // 00 EE
    SYS {}, // 0n-nn
    JMP {
        x: Memory,
    }, // 1x-xx
    JSR {
        x: Memory,
    }, // 2x-xx
    SKEQ {
        x: Register,
        k: u8,
    }, // 3x kk
    SKNE {
        x: Register,
        k: u8,
    }, // 4x kk
    SKEQREG {
        x: Register,
        y: Register,
    }, // 5x y0
    MOV {
        x: Register,
        k: u8,
    }, // 6x kk
    ADD {
        x: Register,
        k: u8,
    }, // 7x kk
    MOVREG {
        x: Register,
        y: Register,
    }, // 8x y0
    OR {
        x: Register,
        y: Register,
    }, // 8x y1
    AND {
        x: Register,
        y: Register,
    }, // 8x y2
    XOR {
        x: Register,
        y: Register,
    }, // 8x y3
    ADDCARRY {
        x: Register,
        y: Register,
    }, // 8x y4
    SUB {
        x: Register,
        y: Register,
    }, // 8x y5
    SHR {
        x: Register,
        y: Register,
    }, // 8x y6
    RSB {
        x: Register,
        y: Register,
    }, // 8x y7
    SHL {
        x: Register,
        y: Register,
    }, // 8x ye
    SKNEREG {
        x: Register,
        y: Register,
    }, // 9x y0
    MVI {
        x: Memory,
    }, // ax-xx
    JMI {
        x: Memory,
    }, // bx-xx
    RAND {
        x: Register,
        k: u8,
    }, // cx kk
    SPRITE {
        x: Register,
        y: Register,
        height: usize,
    }, // dx ys
    SKPR {
        x: Register,
    }, // ex 9e
    SKUP {
        x: Register,
    }, // ex a1
    GDELAY {
        x: Register,
    }, // fx 07
    KEY {
        x: Register,
    }, // fx 0a
    SDELAY {
        x: Register,
    }, // fx 15
    SSOUND {
        x: Register,
    }, // fx 18
    ADDINDEX {
        x: Register,
    }, // fx 1e
    FONT {
        x: Register,
    }, // fx 29
    BCD {
        x: Register,
    }, // fx 33
    STR {
        x: Register,
    }, // fx 55
    LDF {
        x: Register,
    }, // fx 65
}
// @formatter:on

pub struct Cpu {
    reg: [u8; 16],
    memory: Vec<u8>,
    pc: usize,
    index: usize,
    stack: [usize; 16],
    sp: usize,
    delay_timer: u8,
    sound_timer: u8,
    keypad: Vec<bool>,
    pub video: Vec<u8>,
    cur_instruction_num: u64,
}

impl Cpu {
    pub fn new(rom_bytes: &[u8]) -> Self {
        let mut m = vec![0; 4096];
        m.splice(
            MEM_FONTS_START..MEM_FONTS_START + FONTS.len(),
            FONTS.iter().cloned(),
        );
        m.splice(
            MEM_PROGRAM_START..MEM_PROGRAM_START + rom_bytes.len(),
            rom_bytes.iter().cloned(),
        );
        Cpu {
            reg: [0; 16],
            memory: m,
            pc: MEM_PROGRAM_START,
            index: 0,
            stack: [0; 16],
            sp: 0,
            delay_timer: 0,
            sound_timer: 0,
            keypad: vec![false; 16],
            video: vec![0; (VIDEO_HEIGHT * VIDEO_WIDTH) as usize],
            cur_instruction_num: 0,
        }
    }

    pub fn cycle(&mut self, s: f64) {
        const CLOCK_SPEED: f64 = 600.0;
        const TIMER_SPEED: f64 = 60.0;

        let num_instructions = (s * CLOCK_SPEED).round() as u64;
        for _ in 0..num_instructions {
            if self.cur_instruction_num % (CLOCK_SPEED / TIMER_SPEED) as u64 == 0 {
                self.delay_timer -= if self.delay_timer > 0 { 1 } else { 0 };
                self.sound_timer -= if self.sound_timer > 0 { 1 } else { 0 };
            }

            let instruction = self.fetch_instruction().unwrap_or_else(|| {
                panic!(
                    "Illegal instruction: 0x{:02x?} 0x{:02x?}. PC: {:#x?}",
                    self.memory[self.pc],
                    self.memory[self.pc + 1],
                    self.pc
                )
            });

            #[cfg(debug_assertions)]
            println!("PC: 0x{:?} : {:?}", self.pc, instruction);

            self.pc += 2;

            self.execute_instruction(instruction);

            self.cur_instruction_num += 1;
        }
    }

    fn fetch_instruction(&self) -> Option<Instruction> {
        let mask_xo = |byte: u8| (byte & 0b1111_0000) as usize;
        let mask_ox = |byte: u8| (byte & 0b0000_1111) as usize;
        let shift_right_xo = |byte: u8| (byte >> 4) as usize;
        let mask_oxxx = |h: u8, l: u8| ((mask_ox(h) << 8) | l as usize) as usize;

        let [h, l] = [self.memory[self.pc], self.memory[self.pc + 1]];

        match mask_xo(h) {
            0x00 => match l {
                0xE0 => Some(CLS {}),
                0xEE => Some(RTS {}),
                _ => Some(SYS {}),
            },
            0x10 => Some(JMP { x: mask_oxxx(h, l) }),
            0x20 => Some(JSR { x: mask_oxxx(h, l) }),
            0x30 => Some(SKEQ {
                x: mask_ox(h),
                k: l,
            }),
            0x40 => Some(SKNE {
                x: mask_ox(h),
                k: l,
            }),
            0x50 => Some(SKEQREG {
                x: mask_ox(h),
                y: shift_right_xo(l),
            }),
            0x60 => Some(MOV {
                x: mask_ox(h),
                k: l,
            }),
            0x70 => Some(ADD {
                x: mask_ox(h),
                k: l,
            }),
            0x80 => match mask_ox(l) {
                0x00 => Some(MOVREG {
                    x: mask_ox(h),
                    y: shift_right_xo(l),
                }),
                0x01 => Some(OR {
                    x: mask_ox(h),
                    y: shift_right_xo(l),
                }),
                0x02 => Some(AND {
                    x: mask_ox(h),
                    y: shift_right_xo(l),
                }),
                0x03 => Some(XOR {
                    x: mask_ox(h),
                    y: shift_right_xo(l),
                }),
                0x04 => Some(ADDCARRY {
                    x: mask_ox(h),
                    y: shift_right_xo(l),
                }),
                0x05 => Some(SUB {
                    x: mask_ox(h),
                    y: shift_right_xo(l),
                }),
                0x06 => Some(SHR {
                    x: mask_ox(h),
                    y: shift_right_xo(l),
                }),
                0x07 => Some(RSB {
                    x: mask_ox(h),
                    y: shift_right_xo(l),
                }),
                0x0E => Some(SHL {
                    x: mask_ox(h),
                    y: shift_right_xo(l),
                }),
                _ => None,
            },
            0x90 => Some(SKNEREG {
                x: mask_ox(h),
                y: shift_right_xo(l),
            }),
            0xA0 => Some(MVI { x: mask_oxxx(h, l) }),
            0xB0 => Some(JMI { x: mask_oxxx(h, l) }),
            0xC0 => Some(RAND {
                x: mask_ox(h),
                k: l,
            }),
            0xD0 => Some(SPRITE {
                x: mask_ox(h),
                y: shift_right_xo(l),
                height: mask_ox(l),
            }),
            0xE0 => match l {
                0x9E => Some(SKPR { x: mask_ox(h) }),
                0xA1 => Some(SKUP { x: mask_ox(h) }),
                _ => None,
            },
            0xF0 => match l {
                0x07 => Some(GDELAY { x: mask_ox(h) }),
                0x0a => Some(KEY { x: mask_ox(h) }),
                0x15 => Some(SDELAY { x: mask_ox(h) }),
                0x18 => Some(SSOUND { x: mask_ox(h) }),
                0x1E => Some(ADDINDEX { x: mask_ox(h) }),
                0x29 => Some(FONT { x: mask_ox(h) }),
                0x33 => Some(BCD { x: mask_ox(h) }),
                0x55 => Some(STR { x: mask_ox(h) }),
                0x65 => Some(LDF { x: mask_ox(h) }),
                _ => None,
            },
            _ => None,
        }
    }
    fn execute_instruction(&mut self, instruction: Instruction) {
        match instruction {
            SYS {} => {
                // This instruction is only used on the old computers on which Chip-8 was originally implemented. It is ignored by modern interpreters.
            }
            CLS {} => {
                // Clear the display.
                self.video = vec![0; (VIDEO_HEIGHT * VIDEO_WIDTH) as usize];
            }
            RTS {} => {
                // The interpreter sets the program counter to the address at the top of the stack, then subtracts 1 from the stack pointer.
                assert!(self.sp > 0);
                self.sp -= 1;
                self.pc = self.stack[self.sp];
            }
            JMP { x } => {
                // The interpreter sets the program counter to nnn.
                assert!(x < MEM_END);
                self.pc = x
            }
            JSR { x } => {
                // The interpreter increments the stack pointer, then puts the current PC on the top of the stack. The PC is then set to xxx.
                assert!(self.sp < 15);
                self.stack[self.sp] = self.pc;
                self.sp += 1;

                self.pc = x;
            }
            SKEQ { x, k } => {
                // The interpreter compares register Vx to kk, and if they are equal, increments the program counter by 2.
                self.pc += if self.reg[x] == k { 2 } else { 0 };
            }
            SKNE { x, k } => {
                // The interpreter compares register Vx to kk, and if they are not equal, increments the program counter by 2.
                self.pc += if self.reg[x] != k { 2 } else { 0 };
            }
            SKEQREG { x, y } => {
                // The interpreter compares register Vx to register Vy, and if they are equal, increments the program counter by 2.
                self.pc += if self.reg[x] == self.reg[y] { 2 } else { 0 };
            }
            MOV { x, k } => {
                // The interpreter puts the value kk into register Vx.
                self.reg[x] = k;
            }
            ADD { x, k } => {
                // Adds the value kk to the value of register Vx, then stores the result in Vx.
                self.reg[x] = (u16::from(self.reg[x]) + u16::from(k)) as u8;
            }
            MOVREG { x, y } => {
                // Stores the value of register Vy in register Vx.
                self.reg[x] = self.reg[y];
            }
            OR { x, y } => {
                // Performs a bitwise OR on the values of Vx and Vy, then stores the result in Vx.
                // A bitwise OR compares the corresponding bits from two values, and if either bit is 1,
                // then the same bit in the result is also 1. Otherwise, it is 0.
                self.reg[x] |= self.reg[y]
            }
            AND { x, y } => {
                // Performs a bitwise AND on the values of Vx and Vy, then stores the result in Vx.
                // A bitwise AND compares the corresponding bits from two values, and if both bits are 1,
                // then the same bit in the result is also 1. Otherwise, it is 0.
                self.reg[x] &= self.reg[y]
            }
            XOR { x, y } => {
                // Performs a bitwise exclusive OR on the values of Vx and Vy, then stores the result in Vx.
                // An exclusive OR compares the corresponding bits from two values, and if the bits are not both the same,
                // then the corresponding bit in the result is set to 1. Otherwise, it is 0.
                self.reg[x] ^= self.reg[y]
            }
            ADDCARRY { x, y } => {
                // The values of Vx and Vy are added together. If the result is greater than 8 bits (i.e., > 255,) VF is set to 1, otherwise 0.
                // Only the lowest 8 bits of the result are kept, and stored in Vx.
                assert_eq!(1, (288 > 255) as u8);

                let res = u16::from(self.reg[x]) + u16::from(self.reg[y]);
                self.reg[0x0F] = (res > 255) as u8;
                self.reg[x] = res as u8;
            }
            SUB { x, y } => {
                // If Vx > Vy, then VF is set to 1, otherwise 0. Then Vy is subtracted from Vx, and the results stored in Vx.
                self.reg[0x0F] = (self.reg[x] > self.reg[y]) as u8;
                self.reg[x] = (self.reg[x] as i8 - self.reg[y] as i8) as u8;
            }
            SHR { x, .. } => {
                // If the least-significant bit of Vx is 1, then VF is set to 1, otherwise 0. Then Vx is divided by 2.
                self.reg[0x0F] = self.reg[x] & 0b0000_0001;
                self.reg[x] /= 2;
            }
            RSB { x, y } => {
                // If Vy > Vx, then VF is set to 1, otherwise 0. Then Vx is subtracted from Vy, and the results stored in Vx.
                self.reg[0x0F] = (self.reg[y] > self.reg[x]) as u8;
                self.reg[x] = (self.reg[y] as i8 - self.reg[x] as i8) as u8;
            }
            SHL { x, .. } => {
                // If the most-significant bit of Vx is 1, then VF is set to 1, otherwise to 0. Then Vx is multiplied by 2.
                self.reg[0x0F] = (0b1000_0000 & self.reg[x]) >> 7;
                self.reg[x] = (u16::from(self.reg[x]) * 2) as u8;
            }
            SKNEREG { x, y } => {
                // The values of Vx and Vy are compared, and if they are not equal, the program counter is increased by 2.
                self.pc += if self.reg[x] != self.reg[y] { 2 } else { 0 };
            }
            MVI { x } => {
                // The value of register I is set to nnn.
                self.index = x;
            }
            JMI { x } => {
                // The program counter is set to nnn plus the value of V0.
                self.pc = self.reg[0] as usize + x;
            }
            RAND { x, k } => {
                // The interpreter generates a random number from 0 to 255, which is then ANDed with the value kk. The results are stored in Vx. See instruction 8xy2 for more information on AND.
                self.reg[x] = random::<u8>() & k;
            }
            SPRITE { x, y, height } => {
                // The interpreter reads n bytes from memory, starting at the address stored in I.
                // These bytes are then displayed as sprites on screen at coordinates (Vx, Vy).
                // Sprites are XORed onto the existing screen. If this causes any pixels to be erased, VF is set to 1, otherwise it is set to 0.
                // If the sprite is positioned so part of it is outside the coordinates of the display, it wraps around to the opposite side of the screen.
                // See instruction 8xy3 for more information on XOR, and section 2.4, Display, for more information on the Chip-8 screen and sprites.

                let [x_pos, y_pos] = [self.reg[x] as usize, self.reg[y] as usize];
                let sprite = &self.memory[self.index..self.index + height];
                self.reg[0xF] = 0;
                for (i, sprite_byte) in sprite.iter().enumerate() {
                    for j in 0..8 {
                        let sprite_pixel = sprite_byte & (0b1000_0000 >> j);
                        let screen_pixel_idx = (y_pos + i) % VIDEO_HEIGHT as usize
                            * VIDEO_WIDTH as usize
                            + (x_pos + j as usize) % VIDEO_WIDTH as usize;
                        if sprite_pixel > 0 {
                            self.reg[0xF] |= (self.video[screen_pixel_idx] == 0xFF) as u8;
                            self.video[screen_pixel_idx] ^= 0xFF;
                        }
                    }
                }
            }
            SKPR { x } => {
                // Checks the keyboard, and if the key corresponding to the value of Vx is currently in the down position, PC is increased by 2.
                self.pc += if self.keypad[self.reg[x] as usize] {
                    2
                } else {
                    0
                };
            }
            SKUP { x } => {
                // Checks the keyboard, and if the key corresponding to the value of Vx is currently in the up position, PC is increased by 2.
                self.pc += if !self.keypad[self.reg[x] as usize] {
                    2
                } else {
                    0
                };
            }
            GDELAY { x } => {
                // The value of DT is placed into Vx.
                self.reg[x] = self.delay_timer;
            }
            KEY { x } => {
                // All execution stops until a key is pressed, then the value of that key is stored in Vx.
                match self.keypad.iter().position(|&p| p) {
                    Some(i) => self.reg[x] = i as u8,
                    None => self.pc -= 2, // Loop back
                }
            }
            SDELAY { x } => {
                // DT is set equal to the value of Vx.
                self.delay_timer = self.reg[x];
            }
            SSOUND { x } => {
                // ST is set equal to the value of Vx.
                self.sound_timer = self.reg[x];
            }
            ADDINDEX { x } => {
                // The values of I and Vx are added, and the results are stored in I.
                self.index += self.reg[x] as usize;
            }
            FONT { x } => {
                // The value of I is set to the location for the hexadecimal sprite corresponding to the value of Vx.
                // See section 2.4, Display, for more information on the Chip-8 hexadecimal font.
                self.index = MEM_FONTS_START + SPRITE_SIZE * self.reg[x] as usize;
            }
            BCD { x } => {
                // The interpreter takes the decimal value of Vx, and places the hundreds digit in memory at location in I,
                // the tens digit at location I+1, and the ones digit at location I+2.
                let hundreds_tens_ones = [
                    self.reg[x] / 100,
                    (self.reg[x] % 100) / 10,
                    self.reg[x] % 10,
                ];
                self.memory[self.index..self.index + 3].copy_from_slice(&hundreds_tens_ones);
            }
            STR { x } => {
                // The interpreter copies the values of registers V0 through Vx into memory, starting at the address in I.
                self.memory[self.index..=self.index + x].copy_from_slice(&self.reg[..=x]);
            }
            LDF { x } => {
                // The interpreter reads values from memory starting at location I into registers V0 through Vx.
                self.reg[..=x].copy_from_slice(&self.memory[self.index..=self.index + x]);
            }
        }
    }

    pub fn handle_key(&mut self, code: i32, state: bool) {
        let key_value = |code| match code {
            48..=57 => Some((code - 48) as usize),
            97..=102 => Some((code - 97 + 10) as usize),
            _ => None,
        };

        if let Some(value) = key_value(code) {
            self.keypad[value] = state;
        }
    }
}
