use crate::bitboard::{get_file, get_rank};

pub fn parse_string(square: &str) -> Result<u8, &str> {
    if square.len() != 2 {
        return Err("Failed to parse square. Must be 2 characters");
    }

    let chars = square.as_bytes();

    let file = match chars[0] {
        0x61..=0x68 => 7 - (chars[0] - 0x61), // a - h
        0x41..=0x48 => 7 - (chars[0] - 0x41), // A - H
        _ => return Err("Failed to parse square. Invalid file character"),
    };

    let rank = match chars[1] {
        0x31..=0x38 => chars[1] - 0x31, // 1 - 8
        _ => return Err("Failed to parse square. Invalid rank character"),
    };

    return Ok(rank * 8 + file);
}

pub fn to_string(square: u8) -> String {
    let file = (0x61 + (7 - get_file(square))) as char;
    let rank = (0x31 + get_rank(square)) as char;
    return format!("{file}{rank}");
}
