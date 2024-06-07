use sdl2::audio::{AudioCallback, AudioSpecDesired, AudioDevice};

// Square Wave implementation from https://docs.rs/sdl2/latest/sdl2/audio/index.html
struct SquareWave {
    phase_inc: f32,
    phase: f32,
    volume: f32
}

impl AudioCallback for SquareWave {
    type Channel = f32;

    fn callback(&mut self, out: &mut [f32]) {
        // Generate a square wave
        for x in out.iter_mut() {
            *x = if self.phase <= 0.5 {
                self.volume
            } else {
                -self.volume
            };
            self.phase = (self.phase + self.phase_inc) % 1.0;
        }
    }
}

pub struct Beeper(AudioDevice<SquareWave>);

impl Beeper {
    pub fn new() -> Self {
        let sdl_context = sdl2::init().unwrap();
        let audio_subsystem = sdl_context.audio().unwrap();
        
        let desired_spec = AudioSpecDesired {
            freq: Some(100000),
            channels: Some(1),  // mono
            samples: None       // default sample size
        };
        
        Self(
            audio_subsystem.open_playback(None, &desired_spec, |spec| {
                // initialize the audio callback
                SquareWave {
                    phase_inc: 800.0 / spec.freq as f32,
                    phase: 0.0,
                    volume: 0.01
                }
            }).unwrap()
        )
    }

    /// Begin playing sound indefinitely.
    pub fn start(&self) { self.0.resume(); }
    /// Stop playing sound.
    pub fn stop(&self) { self.0.pause(); }
}