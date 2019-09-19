use std::fs::File;
use std::io::prelude::*;

use rand::prelude::*;
use piston_window::*;

use crate::Instr::*;

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
    0xF0, 0x80, 0xF0, 0x80, 0x80  // F
];

const MEM_FONTS_START: usize = 0x50;
const MEM_PROGRAM_START: usize = 0x200;
const MEM_END: usize = 0xFFF;

const VIDEO_WIDTH: usize = 64;
const VIDEO_HEIGHT: usize = 32;

type RegIdx = usize;
type MemAddr = usize;

// @formatter:off
#[derive(Debug)]
enum Instr {                                        // instruction in hexadecimal
    CLS {},                                         // 00 E0
    RTS {},                                         // 00 EE
    SYS {},                                         // 0n-nn
    JMP { x: MemAddr },                             // 1x-xx
    JSR { x: MemAddr },                             // 2x-xx
    SKEQ { x: RegIdx, k: u8 },                      // 3x kk
    SKNE { x: RegIdx, k: u8 },                      // 4x kk
    SKEQREG { x: RegIdx, y: RegIdx },               // 5x y0
    MOV { x: RegIdx, k: u8 },                       // 6x kk
    ADD { x: RegIdx, k: u8 },                       // 7x kk
    MOVREG { x: RegIdx, y: RegIdx },                // 8x y0
    OR { x: RegIdx, y: RegIdx },                    // 8x y1
    AND { x: RegIdx, y: RegIdx },                   // 8x y2
    XOR { x: RegIdx, y: RegIdx },                   // 8x y3
    ADDCARRY { x: RegIdx, y: RegIdx },              // 8x y4
    SUB { x: RegIdx, y: RegIdx },                   // 8x y5
    SHR { x: RegIdx, y: RegIdx },                   // 8x y6
    RSB { x: RegIdx, y: RegIdx },                   // 8x y7
    SHL { x: RegIdx, y: RegIdx },                   // 8x ye
    SKNEREG { x: RegIdx, y: RegIdx },               // 9x y0
    MVI { x: MemAddr },                             // ax-xx
    JMI { x: MemAddr },                             // bx-xx
    RAND { x: RegIdx, k: u8 },                      // cx kk
    SPRITE { x: RegIdx, y: RegIdx, height: usize }, // dx ys
    SKPR { x: RegIdx },                             // ex 9e
    SKUP { x: RegIdx },                             // ex a1
    GDELAY { x: RegIdx },                           // fx 07
    KEY { x: RegIdx },                              // fx 0a
    SDELAY { x: RegIdx },                           // fx 15
    SSOUND { x: RegIdx },                           // fx 18
    ADDINDEX { x: RegIdx },                         // fx 1e
    FONT { x: RegIdx },                             // fx 29
    BCD { x: RegIdx },                              // fx 33
    STR { x: RegIdx },                              // fx 55
    LDF { x: RegIdx },                              // fx 65
}
// @formatter:on

struct Cpu
{
    reg: [u8; 16],
    memory: Vec<u8>,
    pc: usize,
    index: usize,
    stack: [usize; 16],
    sp: usize,
    delay_timer: u8,
    sound_timer: u8,
    keypad: Vec<bool>,
    video: Vec<u8>,
    cur_instruction_num: u64,
}

impl Cpu {
    fn new(rom_bytes: &[u8]) -> Self {
        let mut m = vec![0; 4096];
        m.splice(MEM_FONTS_START..MEM_FONTS_START + FONTS.len(), FONTS.iter().cloned());
        m.splice(MEM_PROGRAM_START..MEM_PROGRAM_START + rom_bytes.len(), rom_bytes.iter().cloned());
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
            video: vec![0; VIDEO_HEIGHT * VIDEO_WIDTH],
            cur_instruction_num: 0,
        }
    }

//    fn format_video(&self) -> String {
//        let format_line = |w: &[u8]| w.iter()
//            .map(|&x| if x > 0 { String::from("*") } else { String::from(" ") })
//            .collect::<Vec<String>>()
//            .join("");
//
//        self.video
//            .chunks(VIDEO_WIDTH)
//            .map(|l| format_line(l))
//            .collect::<Vec<String>>()
//            .join("\n")
//    }

    fn cycle(&mut self, s: f64) {
        const CLOCK_SPEED: f64 = 600.0;
        const TIMER_SPEED: f64 = 60.0;

        let num_instructions = (s * CLOCK_SPEED).round() as u64;
        for _ in 0..num_instructions {
            if self.cur_instruction_num % (CLOCK_SPEED / TIMER_SPEED) as u64 == 0 {
                self.delay_timer -= if self.delay_timer > 0 { 1 } else { 0 };
                self.sound_timer -= if self.sound_timer > 0 { 1 } else { 0 };
            }

            let instruction = self.fetch_instruction();

            #[cfg(debug_assertions)]
            println!("PC: 0x{:?} : {:?}, {:?}", self.pc, instruction, self.keypad);

            self.pc += 2;

            self.execute_instruction(instruction);

            self.cur_instruction_num += 1;
        }
    }

    fn fetch_instruction(&self) -> Instr {
        let mask_high = |byte: u8| (byte & 0b11110000) as usize;
        let mask_low = |byte: u8| (byte & 0b00001111) as usize;
        let shift_right = |byte: u8| (byte >> 4) as usize;
        let as_u12 = |h: u8, l: u8| ((mask_low(h) << 8) | l as usize) as usize;

        let [h, l] = [self.memory[self.pc], self.memory[self.pc + 1]];
        let panic = || panic!("Illegal instruction: 0x{:02x?} 0x{:02x?}. PC: {:#x?}", h, l, self.pc);

        match mask_high(h) {
            0x00 => match l {
                0xE0 => CLS {},
                0xEE => RTS {},
                _ => SYS {}
            },
            0x10 => JMP { x: as_u12(h, l) },
            0x20 => JSR { x: as_u12(h, l) },
            0x30 => SKEQ { x: mask_low(h), k: l },
            0x40 => SKNE { x: mask_low(h), k: l },
            0x50 => SKEQREG { x: mask_low(h), y: shift_right(l) },
            0x60 => MOV { x: mask_low(h), k: l },
            0x70 => ADD { x: mask_low(h), k: l },
            0x80 => match mask_low(l) {
                0x00 => MOVREG { x: mask_low(h), y: shift_right(l) },
                0x01 => OR { x: mask_low(h), y: shift_right(l) },
                0x02 => AND { x: mask_low(h), y: shift_right(l) },
                0x03 => XOR { x: mask_low(h), y: shift_right(l) },
                0x04 => ADDCARRY { x: mask_low(h), y: shift_right(l) },
                0x05 => SUB { x: mask_low(h), y: shift_right(l) },
                0x06 => SHR { x: mask_low(h), y: shift_right(l) },
                0x07 => RSB { x: mask_low(h), y: shift_right(l) },
                0x0E => SHL { x: mask_low(h), y: shift_right(l) },
                _ => panic()
            },
            0x90 => SKNEREG { x: mask_low(h), y: shift_right(l) },
            0xA0 => MVI { x: as_u12(h, l) },
            0xB0 => JMI { x: as_u12(h, l) },
            0xC0 => RAND { x: mask_low(h), k: l },
            0xD0 => SPRITE { x: mask_low(h), y: shift_right(l), height: mask_low(l) },
            0xE0 => match l {
                0x9E => SKPR { x: mask_low(h) },
                0xA1 => SKUP { x: mask_low(h) },
                _ => panic()
            },
            0xF0 => match l {
                0x07 => GDELAY { x: mask_low(h) },
                0x0a => KEY { x: mask_low(h) },
                0x15 => SDELAY { x: mask_low(h) },
                0x18 => SSOUND { x: mask_low(h) },
                0x1E => ADDINDEX { x: mask_low(h) },
                0x29 => FONT { x: mask_low(h) },
                0x33 => BCD { x: mask_low(h) },
                0x55 => STR { x: mask_low(h) },
                0x65 => LDF { x: mask_low(h) },
                _ => panic()
            },
            _ => panic()
        }
    }
    fn execute_instruction(&mut self, instruction: Instr) {
        match instruction {
            SYS {} => {
                // This instruction is only used on the old computers on which Chip-8 was originally implemented. It is ignored by modern interpreters.
            },
            CLS {} => {
                // Clear the display.
                self.video = vec![0; VIDEO_HEIGHT * VIDEO_WIDTH];
            },
            RTS {} => {
                // The interpreter sets the program counter to the address at the top of the stack, then subtracts 1 from the stack pointer.
                assert!(self.sp > 0);
                self.sp -= 1;
                self.pc = self.stack[self.sp];
            },
            JMP { x } => {
                // The interpreter sets the program counter to nnn.
                assert!(x < MEM_END);
                self.pc = x
            },
            JSR { x } => {
                // The interpreter increments the stack pointer, then puts the current PC on the top of the stack. The PC is then set to xxx.
                assert!(self.sp < 15);
                self.stack[self.sp] = self.pc;
                self.sp += 1;

                self.pc = x;
            },
            SKEQ { x, k } => {
                // The interpreter compares register Vx to kk, and if they are equal, increments the program counter by 2.
                self.pc += if self.reg[x] == k { 2 } else { 0 };
            },
            SKNE { x, k } => {
                // The interpreter compares register Vx to kk, and if they are not equal, increments the program counter by 2.
                self.pc += if self.reg[x] != k { 2 } else { 0 };
            },
            SKEQREG { x, y } => {
                // The interpreter compares register Vx to register Vy, and if they are equal, increments the program counter by 2.
                self.pc += if self.reg[x] == self.reg[y] { 2 } else { 0 };
            },
            MOV { x, k } => {
                // The interpreter puts the value kk into register Vx.
                self.reg[x] = k;
            },
            ADD { x, k } => {
                // Adds the value kk to the value of register Vx, then stores the result in Vx.
                self.reg[x] = (self.reg[x] as u16 + k as u16) as u8;
            },
            MOVREG { x, y } => {
                // Stores the value of register Vy in register Vx.
                self.reg[x] = self.reg[y];
            },
            OR { x, y } => {
                // Performs a bitwise OR on the values of Vx and Vy, then stores the result in Vx.
                // A bitwise OR compares the corresponding bits from two values, and if either bit is 1, then the same bit in the result is also 1. Otherwise, it is 0.
                self.reg[x] |= self.reg[y]
            },
            AND { x, y } => {
                // Performs a bitwise AND on the values of Vx and Vy, then stores the result in Vx. A bitwise AND compares the corresponding bits from two values, and if both bits are 1, then the same bit in the result is also 1. Otherwise, it is 0.
                self.reg[x] &= self.reg[y]
            },
            XOR { x, y } => {
                // Performs a bitwise exclusive OR on the values of Vx and Vy, then stores the result in Vx. An exclusive OR compares the corresponding bits from two values, and if the bits are not both the same, then the corresponding bit in the result is set to 1. Otherwise, it is 0.
                self.reg[x] ^= self.reg[y]
            },
            ADDCARRY { x, y } => {
                // The values of Vx and Vy are added together. If the result is greater than 8 bits (i.e., > 255,) VF is set to 1, otherwise 0.
                // Only the lowest 8 bits of the result are kept, and stored in Vx.
                assert_eq!(1, (288 > 255) as u8);

                let res = self.reg[x] as u16 + self.reg[y] as u16;
                self.reg[0x0F] = (res > 255) as u8;
                self.reg[x] = res as u8;
            },
            SUB { x, y } => {
                // If Vx > Vy, then VF is set to 1, otherwise 0. Then Vy is subtracted from Vx, and the results stored in Vx.
                self.reg[0x0F] = (self.reg[x] > self.reg[y]) as u8;
                self.reg[x] = (self.reg[x] as i8 - self.reg[y] as i8) as u8;
            },
            SHR { x, y: _ } => {
                // If the least-significant bit of Vx is 1, then VF is set to 1, otherwise 0. Then Vx is divided by 2.
                self.reg[0x0F] = self.reg[x] & 0b00000001;
                self.reg[x] /= 2;
            },
            RSB { x, y } => {
                // If Vy > Vx, then VF is set to 1, otherwise 0. Then Vx is subtracted from Vy, and the results stored in Vx.
                self.reg[0x0F] = (self.reg[y] > self.reg[x]) as u8;
                self.reg[x] = (self.reg[y] as i8 - self.reg[x] as i8) as u8;
            },
            SHL { x, y: _ } => {
                // If the most-significant bit of Vx is 1, then VF is set to 1, otherwise to 0. Then Vx is multiplied by 2.
                self.reg[0x0F] = (self.reg[x] & 0b10000000) >> 7;
                self.reg[x] = (self.reg[x] as u16 * 2) as u8;
            },
            SKNEREG { x, y } => {
                // The values of Vx and Vy are compared, and if they are not equal, the program counter is increased by 2.
                self.pc += if self.reg[x] != self.reg[y] { 2 } else { 0 };
            },
            MVI { x } => {
                // The value of register I is set to nnn.
                self.index = x;
            },
            JMI { x } => {
                // The program counter is set to nnn plus the value of V0.
                self.pc = self.reg[0] as usize + x;
            },
            RAND { x, k } => {
                // The interpreter generates a random number from 0 to 255, which is then ANDed with the value kk. The results are stored in Vx. See instruction 8xy2 for more information on AND.
                self.reg[x] = random::<u8>() & k;
            },
            SPRITE { x, y, height } => {
                // The interpreter reads n bytes from memory, starting at the address stored in I.
                // These bytes are then displayed as sprites on screen at coordinates (Vx, Vy).
                // Sprites are XORed onto the existing screen. If this causes any pixels to be erased, VF is set to 1, otherwise it is set to 0.
                // If the sprite is positioned so part of it is outside the coordinates of the display, it wraps around to the opposite side of the screen.
                // See instruction 8xy3 for more information on XOR, and section 2.4, Display, for more information on the Chip-8 screen and sprites.

                let [x_pos, y_pos] = [self.reg[x] as usize, self.reg[y] as usize];
//                let encoded_sprite = self.memory[self.index..self.index + height]
//                    .iter()
//                    .map(|byte| {
//                        (0..8)
//                            .map(|j| byte & (0b10000000 >> j) as u8)
//                            .collect::<Vec<u8>>()
//                    }).collect::<Vec<Vec<u8>>>();
//
//                println!("{:?}, {:?}", height, encoded_sprite);
//
//                let mut did_collide = 0;
//                for (v, s) in self.video
//                    .chunks(VIDEO_WIDTH)
//                    .skip(y_pos)
//                    .take(height)
//                    .zip(encoded_sprite)
//                    {
//                        for (a, b) in *v.iter_mut().skip(x_pos).zip(s)
//                            {
//                                if b > 0 {
//                                    did_collide |= (*a > 0) as u8;
//                                    *a ^= 0xFF;
//                                }
//                            }
//                    }
//
//                self.reg[0xF] = did_collide;

//                let mut did_collide = 0;
//                self.video = self.video
//                    .chunks(VIDEO_WIDTH)
//                    .skip(y_pos)
//                    .take(height)
//                    .zip(encoded_sprite)
//                    .map(|(v, s)|
//                        v.iter().skip(x_pos).zip(s).map(|(&a, b)|
//                            {
//                                if b > 0 {
//                                    did_collide |= (a > 0) as u8;
//                                    a ^ 0xFF
//                                } else {
//                                    a
//                                }
//                            }
//                        ).collect::<Vec<u8>>()
//                    ).flatten()
//                    .collect();
//
//                self.reg[0xF] = did_collide;

                let sprite = &self.memory[self.index..self.index + height];
                self.reg[0xF] = 0;
                for i in 0..height {
                    let sprite_byte = sprite[i];
                    for j in 0..8 {
                        let sprite_pixel = sprite_byte & (0b10000000 >> j);
                        let screen_pixel_idx = (y_pos + i) % VIDEO_HEIGHT * VIDEO_WIDTH + (x_pos + j as usize) % VIDEO_WIDTH;
                        if sprite_pixel > 0 {
                            self.reg[0xF] |= (self.video[screen_pixel_idx] == 0xFF) as u8;
                            self.video[screen_pixel_idx] ^= 0xFF;
                        }
                    }
                }
            },
            SKPR { x } => {
                // Checks the keyboard, and if the key corresponding to the value of Vx is currently in the down position, PC is increased by 2.
                self.pc += if self.keypad[self.reg[x] as usize] { 2 } else { 0 };
            },
            SKUP { x } => {
                // Checks the keyboard, and if the key corresponding to the value of Vx is currently in the up position, PC is increased by 2.
                self.pc += if !self.keypad[self.reg[x] as usize] { 2 } else { 0 };
            },
            GDELAY { x } => {
                // The value of DT is placed into Vx.
                self.reg[x] = self.delay_timer;
            },
            KEY { x } => {
                // All execution stops until a key is pressed, then the value of that key is stored in Vx.
                match self.keypad.iter().position(|&p| p == true) {
                    Some(i) => self.reg[x] = i as u8,
                    None => self.pc -= 2 // Loop back
                }
            },
            SDELAY { x } => {
                // DT is set equal to the value of Vx.
                self.delay_timer = self.reg[x];
            },
            SSOUND { x } => {
                // ST is set equal to the value of Vx.
                self.sound_timer = self.reg[x];
            },
            ADDINDEX { x } => {
                // The values of I and Vx are added, and the results are stored in I.
                self.index += self.reg[x] as usize;
            },
            FONT { x } => {
                // The value of I is set to the location for the hexadecimal sprite corresponding to the value of Vx.
                // See section 2.4, Display, for more information on the Chip-8 hexadecimal font.
                self.index = MEM_FONTS_START + SPRITE_SIZE * self.reg[x] as usize;
            },
            BCD { x } => {
                // The interpreter takes the decimal value of Vx, and places the hundreds digit in memory at location in I, the tens digit at location I+1, and the ones digit at location I+2.
                let hundreds_tens_ones = [self.reg[x] / 100, (self.reg[x] % 100) / 10, self.reg[x] % 10];
                self.memory[self.index..self.index + 3].copy_from_slice(&hundreds_tens_ones);
            },
            STR { x } => {
                // The interpreter copies the values of registers V0 through Vx into memory, starting at the address in I.
                self.memory[self.index..=self.index + x].copy_from_slice(&self.reg[..=x]);
            },
            LDF { x } => {
                // The interpreter reads values from memory starting at location I into registers V0 through Vx.
                self.reg[..=x].copy_from_slice(&self.memory[self.index..=self.index + x]);
            },
        }
    }

    pub fn handle_key(&mut self, code: i32, state: bool) {
        let key_value = |code| {
            match code {
                48..=57 => Some((code - 48) as usize),
                97..=102 => Some((code - 97 + 10) as usize),
                _ => None
            }
        };

        if let Some(value) = key_value(code) {
            self.keypad[value] = state;
        }
    }
}

const ENLARGEMENT: usize = 20;
const WINDOW_DIMENSIONS: [u32; 2] = [(VIDEO_WIDTH * ENLARGEMENT) as u32, (VIDEO_HEIGHT * ENLARGEMENT) as u32];

fn main() {
//    let mut f = File::open("/home/horigome/dev/rust/chip8/src/BC_test.ch8").unwrap();
    let mut f = File::open("/home/horigome/dev/rust/chip8/src/games/PONG2").unwrap();
    let mut buffer = Vec::new();
    let _bytes_read = f.read_to_end(&mut buffer);

    let mut cpu = Cpu::new(buffer.as_slice());

    let mut window: PistonWindow = WindowSettings::new("Chip-8 Interpreter", WINDOW_DIMENSIONS)
        .exit_on_esc(true)
        .build()
        .unwrap();

    while let Some(e) = window.next() {
        if let Some(_) = e.render_args() {
            window.draw_2d(&e, |context, graphics, _| {
                piston_window::clear(color::BLACK, graphics);

                for (i, row) in cpu.video.chunks(VIDEO_WIDTH).enumerate() {
                    for (j, &val) in row.iter().enumerate() {
                        if val > 0 {
                            let dimensions = [(j * ENLARGEMENT) as f64, (i * ENLARGEMENT) as f64, ENLARGEMENT as f64, ENLARGEMENT as f64];
                            Rectangle::new(color::WHITE)
                                .draw(dimensions, &context.draw_state, context.transform, graphics);
                        }
                    }
                }
            });
        }

        if let Some(u) = e.update_args() {
            cpu.cycle(u.dt);
        }

        if let Some(Button::Keyboard(key)) = e.release_args() {
            cpu.handle_key(key.code(), false);
        }

        if let Some(Button::Keyboard(key)) = e.press_args() {
            cpu.handle_key(key.code(), true);
        }
    }
}