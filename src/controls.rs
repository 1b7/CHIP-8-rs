use sdl2::keyboard::Keycode;

/// The keycodes that form the basis of our 'controller'.
pub(crate) const ACCEPTED_KEYS: [Keycode; 16] = [
    Keycode::Num1, Keycode::Num2, Keycode::Num3, Keycode::Num4,
    Keycode::Q,    Keycode::W,    Keycode::E,    Keycode::R, 
    Keycode::A,    Keycode::S,    Keycode::D,    Keycode::F, 
    Keycode::Z,    Keycode::X,    Keycode::C,    Keycode::V,
];

/// Translate SDL2 Keycode values to CHIP-8 equivalents.
/// NOTE: Will panic upon encountering invalid keys. 
pub(crate) fn to_key(k: Keycode) -> u16 {
    match k {
        //                 FEDC_BA98_7654_3210
        Keycode::Num1 => 0b0000_0000_0000_0010, // 1
        Keycode::Num2 => 0b0000_0000_0000_0100, // 2
        Keycode::Num3 => 0b0000_0000_0000_1000, // 3
        Keycode::Num4 => 0b0001_0000_0000_0000, // C

        Keycode::Q    => 0b0000_0000_0001_0000, // 4
        Keycode::W    => 0b0000_0000_0010_0000, // 5
        Keycode::E    => 0b0000_0000_0100_0000, // 6
        Keycode::R    => 0b0010_0000_0000_0000, // D

        Keycode::A    => 0b0000_0000_1000_0000, // 7
        Keycode::S    => 0b0000_0001_0000_0000, // 8
        Keycode::D    => 0b0000_0010_0000_0000, // 9
        Keycode::F    => 0b0100_0000_0000_0000, // E

        Keycode::Z    => 0b0000_0100_0000_0000, // A
        Keycode::X    => 0b0000_0000_0000_0001, // 0
        Keycode::C    => 0b0000_1000_0000_0000, // B
        Keycode::V    => 0b1000_0000_0000_0000, // F

        _ => panic!("Invalid key provided")
    }
}