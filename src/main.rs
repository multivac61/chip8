use std::env;
use std::io::Read;
use std::fs::File;

use piston_window::*;

mod cpu;


fn main() {
    let file_name = env::args().nth(1).expect("Must give game name as first file");
    let mut file = File::open(file_name).expect("There was an issue opening the file");
    let mut buffer = Vec::new();
    let _bytes_read = file.read_to_end(&mut buffer);

    let mut cpu = cpu::Cpu::new(buffer.as_slice());

    const GUI_SCALE: f64 = 10.0;
    let window_dimensions = [cpu::VIDEO_WIDTH * GUI_SCALE as u32, cpu::VIDEO_HEIGHT * GUI_SCALE as u32];
    let mut window: PistonWindow = WindowSettings::new("Chip-8 Interpreter", window_dimensions)
        .exit_on_esc(true)
        .build()
        .expect("Failed to create a piston window");

    while let Some(e) = window.next() {
        if let Some(_) = e.render_args() {
            window.draw_2d(&e, |context, graphics, _| {
                piston_window::clear(color::BLACK, graphics);

                for (i, row) in cpu.video.chunks(cpu::VIDEO_WIDTH as usize).enumerate() {
                    for (j, &val) in row.iter().enumerate() {
                        if val > 0 {
                            let d = [j as f64 * GUI_SCALE, i as f64 * GUI_SCALE, GUI_SCALE, GUI_SCALE];
                            Rectangle::new(color::WHITE)
                                .draw(d, &context.draw_state, context.transform, graphics);
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