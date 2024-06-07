use std::time::Duration;
use std::env;

mod cpu;
use cpu::Machine;

mod beeper;
use beeper::Beeper;

mod controls;
use controls::*;

use sdl2::{
    event::Event,
    keyboard::Keycode,
    pixels::Color,
    rect::Point,
    render::Canvas,
    video::Window,
    EventPump,
};

const DISPLAY_HEIGHT: u32 = 32;
const ASPECT_RATIO: u32 = 2;
const SCALE_FACTOR: u32 = 25;

/// Nanoseconds per instruction.
const CPU_TICK_RATE: f32 = 1_000_000_000.0 / 500.0;

/// Initialises the SDL context for events and drawing, 
/// returns a canvas and event pump.
pub fn sdl_init() -> (Canvas<Window>, EventPump) {
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let window = video_subsystem
        .window(
            "CHIP-8-EMU",
            DISPLAY_HEIGHT * ASPECT_RATIO * SCALE_FACTOR,
            DISPLAY_HEIGHT * SCALE_FACTOR,
        )
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().build().unwrap();

    canvas.set_scale(SCALE_FACTOR as f32, SCALE_FACTOR as f32)
        .expect("Error setting canvas scale factor");
    canvas.scale();

    (canvas, sdl_context.event_pump().unwrap())
}

pub fn main() {
    let mut chip8 = Machine::new();

    let argv: Vec<String> = env::args().collect();

    if argv.len() < 2 { panic!("Error: Must provide a path to an executable file.") }
    chip8.load_program(&argv[1]);

    // Calculate the number of ticks between each screen update, targeting 60Hz.
    let ticks_per_refresh = ((1_000_000_000.0 / 60.0) / CPU_TICK_RATE).floor() as usize;
    let mut tick_counter = 0;

    // Initialise SDL2 components to handle graphics, audio, and event polling:
    let (mut canvas,mut event_pump) = sdl_init();
    let beeper = Beeper::new();
    let mut buzzer_on = false;

    // Main execution loop:
    'running: loop {
        if buzzer_on != chip8.buzzer_active() {
            buzzer_on = chip8.buzzer_active();
            if buzzer_on { beeper.start(); } else { beeper.stop(); }
        } 

        // Update the screen and timers at a rate of roughly 60Hz.
        if tick_counter == 0 {
            canvas.set_draw_color(Color::RGB(0, 0, 0));
            canvas.clear();
            canvas.set_draw_color(Color::RGB(255, 255, 255));
    
            for row in 0..32 {
                for col in 0..64 {
                    if chip8.display[row][col] > 0 { 
                        canvas.draw_point(Point::new(col as i32, row as i32))
                            .expect("Error occurred when drawing pixels"); 
                    }
                }
            }

            chip8.decay_timers();
        }
        
        // Check for keyboard inputs and update the system's input buffer accordingly:
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. } 
                | Event::KeyDown { keycode: Some(Keycode::Escape), .. } => break 'running,
                Event::KeyDown { keycode: Some(k), .. } => {
                    if controls::ACCEPTED_KEYS.contains(&k) { chip8.key_down(to_key(k)); }
                },
                Event::KeyUp { keycode: Some(k), .. } => {
                    if ACCEPTED_KEYS.contains(&k) { chip8.key_up(to_key(k)); }
                },
                _ => {}
            }
        }

        tick_counter = (tick_counter + 1) % ticks_per_refresh;
        chip8.cycle();
        canvas.present();

        std::thread::sleep(Duration::new(0, CPU_TICK_RATE as u32));
    }
}
