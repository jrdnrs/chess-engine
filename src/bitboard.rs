use std::num;

use crate::bits::*;

pub const H1: u8 = 0;
pub const G1: u8 = 1;
pub const F1: u8 = 2;
pub const E1: u8 = 3;
pub const D1: u8 = 4;
pub const C1: u8 = 5;
pub const B1: u8 = 6;
pub const A1: u8 = 7;
pub const H2: u8 = 8;
pub const G2: u8 = 9;
pub const F2: u8 = 10;
pub const E2: u8 = 11;
pub const D2: u8 = 12;
pub const C2: u8 = 13;
pub const B2: u8 = 14;
pub const A2: u8 = 15;
pub const H3: u8 = 16;
pub const G3: u8 = 17;
pub const F3: u8 = 18;
pub const E3: u8 = 19;
pub const D3: u8 = 20;
pub const C3: u8 = 21;
pub const B3: u8 = 22;
pub const A3: u8 = 23;
pub const H4: u8 = 24;
pub const G4: u8 = 25;
pub const F4: u8 = 26;
pub const E4: u8 = 27;
pub const D4: u8 = 28;
pub const C4: u8 = 29;
pub const B4: u8 = 30;
pub const A4: u8 = 31;
pub const H5: u8 = 32;
pub const G5: u8 = 33;
pub const F5: u8 = 34;
pub const E5: u8 = 35;
pub const D5: u8 = 36;
pub const C5: u8 = 37;
pub const B5: u8 = 38;
pub const A5: u8 = 39;
pub const H6: u8 = 40;
pub const G6: u8 = 41;
pub const F6: u8 = 42;
pub const E6: u8 = 43;
pub const D6: u8 = 44;
pub const C6: u8 = 45;
pub const B6: u8 = 46;
pub const A6: u8 = 47;
pub const H7: u8 = 48;
pub const G7: u8 = 49;
pub const F7: u8 = 50;
pub const E7: u8 = 51;
pub const D7: u8 = 52;
pub const C7: u8 = 53;
pub const B7: u8 = 54;
pub const A7: u8 = 55;
pub const H8: u8 = 56;
pub const G8: u8 = 57;
pub const F8: u8 = 58;
pub const E8: u8 = 59;
pub const D8: u8 = 60;
pub const C8: u8 = 61;
pub const B8: u8 = 62;
pub const A8: u8 = 63;

pub const FILE_A: u64 = 0x8080808080808080;
pub const FILE_B: u64 = 0x4040404040404040;
pub const FILE_C: u64 = 0x2020202020202020;
pub const FILE_D: u64 = 0x1010101010101010;
pub const FILE_E: u64 = 0x0808080808080808;
pub const FILE_F: u64 = 0x0404040404040404;
pub const FILE_G: u64 = 0x0202020202020202;
pub const FILE_H: u64 = 0x0101010101010101;

pub const RANK_1: u64 = 0x00000000000000FF;
pub const RANK_2: u64 = 0x000000000000FF00;
pub const RANK_3: u64 = 0x0000000000FF0000;
pub const RANK_4: u64 = 0x00000000FF000000;
pub const RANK_5: u64 = 0x000000FF00000000;
pub const RANK_6: u64 = 0x0000FF0000000000;
pub const RANK_7: u64 = 0x00FF000000000000;
pub const RANK_8: u64 = 0xFF00000000000000;

pub const A8_H1: u64 = 0x08040201008040201;
pub const A1_H8: u64 = 0x00102040810204080;

#[rustfmt::skip]
pub const ROOK_MAGIC_SHIFT: [u64; 64] = [
 52, 53, 53, 53, 53, 53, 53, 52,
 53, 54, 54, 54, 54, 54, 54, 53,
 53, 54, 54, 54, 54, 54, 54, 53,
 53, 54, 54, 54, 54, 54, 54, 53,
 53, 54, 54, 54, 54, 54, 54, 53,
 53, 54, 54, 54, 54, 54, 54, 53,
 53, 54, 54, 54, 54, 54, 54, 53,
 53, 54, 54, 53, 53, 53, 53, 53
];

#[rustfmt::skip]
pub const ROOK_MAGIC: [u64; 64] = [
 0x0080001020400080, 0x0040001000200040, 0x0080081000200080, 0x0080040800100080,
 0x0080020400080080, 0x0080010200040080, 0x0080008001000200, 0x0080002040800100,
 0x0000800020400080, 0x0000400020005000, 0x0000801000200080, 0x0000800800100080,
 0x0000800400080080, 0x0000800200040080, 0x0000800100020080, 0x0000800040800100,
 0x0000208000400080, 0x0000404000201000, 0x0000808010002000, 0x0000808008001000,
 0x0000808004000800, 0x0000808002000400, 0x0000010100020004, 0x0000020000408104,
 0x0000208080004000, 0x0000200040005000, 0x0000100080200080, 0x0000080080100080,
 0x0000040080080080, 0x0000020080040080, 0x0000010080800200, 0x0000800080004100,
 0x0000204000800080, 0x0000200040401000, 0x0000100080802000, 0x0000080080801000,
 0x0000040080800800, 0x0000020080800400, 0x0000020001010004, 0x0000800040800100,
 0x0000204000808000, 0x0000200040008080, 0x0000100020008080, 0x0000080010008080,
 0x0000040008008080, 0x0000020004008080, 0x0000010002008080, 0x0000004081020004,
 0x0000204000800080, 0x0000200040008080, 0x0000100020008080, 0x0000080010008080,
 0x0000040008008080, 0x0000020004008080, 0x0000800100020080, 0x0000800041000080,
 0x00FFFCDDFCED714A, 0x007FFCDDFCED714A, 0x003FFFCDFFD88096, 0x0000040810002101,
 0x0001000204080011, 0x0001000204000801, 0x0001000082000401, 0x0001FFFAABFAD1A2
];

#[rustfmt::skip]
pub const BISHOP_MAGIC_SHIFT: [u64; 64] = [
 58, 59, 59, 59, 59, 59, 59, 58,
 59, 59, 59, 59, 59, 59, 59, 59,
 59, 59, 57, 57, 57, 57, 59, 59,
 59, 59, 57, 55, 55, 57, 59, 59,
 59, 59, 57, 55, 55, 57, 59, 59,
 59, 59, 57, 57, 57, 57, 59, 59,
 59, 59, 59, 59, 59, 59, 59, 59,
 58, 59, 59, 59, 59, 59, 59, 58
];

#[rustfmt::skip]
pub const BISHOP_MAGIC: [u64; 64] = [
 0x0002020202020200, 0x0002020202020000, 0x0004010202000000, 0x0004040080000000,
 0x0001104000000000, 0x0000821040000000, 0x0000410410400000, 0x0000104104104000,
 0x0000040404040400, 0x0000020202020200, 0x0000040102020000, 0x0000040400800000,
 0x0000011040000000, 0x0000008210400000, 0x0000004104104000, 0x0000002082082000,
 0x0004000808080800, 0x0002000404040400, 0x0001000202020200, 0x0000800802004000,
 0x0000800400A00000, 0x0000200100884000, 0x0000400082082000, 0x0000200041041000,
 0x0002080010101000, 0x0001040008080800, 0x0000208004010400, 0x0000404004010200,
 0x0000840000802000, 0x0000404002011000, 0x0000808001041000, 0x0000404000820800,
 0x0001041000202000, 0x0000820800101000, 0x0000104400080800, 0x0000020080080080,
 0x0000404040040100, 0x0000808100020100, 0x0001010100020800, 0x0000808080010400,
 0x0000820820004000, 0x0000410410002000, 0x0000082088001000, 0x0000002011000800,
 0x0000080100400400, 0x0001010101000200, 0x0002020202000400, 0x0001010101000200,
 0x0000410410400000, 0x0000208208200000, 0x0000002084100000, 0x0000000020880000,
 0x0000001002020000, 0x0000040408020000, 0x0004040404040000, 0x0002020202020000,
 0x0000104104104000, 0x0000002082082000, 0x0000000020841000, 0x0000000000208800,
 0x0000000010020200, 0x0000000404080200, 0x0000040404040400, 0x0002020202020200
];

#[rustfmt::skip]
const WHITE_ROOK_SCORE: [i32; 64] = [
 0,  0,  0,  0,  0,  0,  0,  0,
 5, 10, 10, 10, 10, 10, 10,  5,
   -5,  0,  0,  0,  0,  0,  0, -5,
   -5,  0,  0,  0,  0,  0,  0, -5,
   -5,  0,  0,  0,  0,  0,  0, -5,
   -5,  0,  0,  0,  0,  0,  0, -5,
   -5,  0,  0,  0,  0,  0,  0, -5,
 0,  0,  0,  5,  5,  0,  0,  0
];
#[rustfmt::skip]
const WHITE_BISHOP_SCORE: [i32; 64] = [
 -20,-10,-10,-10,-10,-10,-10,-20,
 -10,  0,  0,  0,  0,  0,  0,-10,
 -10,  0,  5, 10, 10,  5,  0,-10,
 -10,  5,  5, 10, 10,  5,  5,-10,
 -10,  0, 10, 10, 10, 10,  0,-10,
 -10, 10, 10, 10, 10, 10, 10,-10,
 -10,  5,  0,  0,  0,  0,  5,-10,
 -20,-10,-10,-10,-10,-10,-10,-20,
];

#[rustfmt::skip]
const WHITE_KNIGHT_SCORE: [i32; 64] = [
 -50,-40,-30,-30,-30,-30,-40,-50,
 -40,-20,  0,  0,  0,  0,-20,-40,
 -30,  0, 10, 15, 15, 10,  0,-30,
 -30,  5, 15, 20, 20, 15,  5,-30,
 -30,  0, 15, 20, 20, 15,  0,-30,
 -30,  5, 10, 15, 15, 10,  5,-30,
 -40,-20,  0,  5,  5,  0,-20,-40,
 -50,-40,-30,-30,-30,-30,-40,-50,
];

#[rustfmt::skip]
const WHITE_QUEEN_SCORE: [i32; 64] = [
 -20,-10,-10, -5, -5,-10,-10,-20,
 -10,  0,  0,  0,  0,  0,  0,-10,
 -10,  0,  5,  5,  5,  5,  0,-10,
  -5,  0,  5,  5,  5,  5,  0, -5,
   0,  0,  5,  5,  5,  5,  0, -5,
 -10,  5,  5,  5,  5,  5,  0,-10,
 -10,  0,  5,  0,  0,  0,  0,-10,
 -20,-10,-10, -5, -5,-10,-10,-20
];

#[rustfmt::skip]
const WHITE_PAWN_SCORE: [i32; 64] = [
  0,  0,  0,  0,  0,  0,  0,  0,
 50, 50, 50, 50, 50, 50, 50, 50,
 10, 10, 20, 30, 30, 20, 10, 10,
  5,  5, 10, 25, 25, 10,  5,  5,
  0,  0,  0, 20, 20,  0,  0,  0,
  5, -5,-10,  0,  0,-10, -5,  5,
  5, 10, 10,-20,-20, 10, 10,  5,
  0,  0,  0,  0,  0,  0,  0,  0
];

#[rustfmt::skip]
const WHITE_KING_MIDDLE_SCORE: [i32; 64] = [
 -30,-40,-40,-50,-50,-40,-40,-30,
 -30,-40,-40,-50,-50,-40,-40,-30,
 -30,-40,-40,-50,-50,-40,-40,-30,
 -30,-40,-40,-50,-50,-40,-40,-30,
 -20,-30,-30,-40,-40,-30,-30,-20,
 -10,-20,-20,-20,-20,-20,-20,-10,
  20, 20,  0,  0,  0,  0, 20, 20,
  20, 30, 10,  0,  0, 10, 30, 20
];

#[rustfmt::skip]
const WHITE_KING_END_SCORE: [i32; 64] = [
 -50,-40,-30,-20,-20,-30,-40,-50,
 -30,-20,-10,  0,  0,-10,-20,-30,
 -30,-10, 20, 30, 30, 20,-10,-30,
 -30,-10, 30, 40, 40, 30,-10,-30,
 -30,-10, 30, 40, 40, 30,-10,-30,
 -30,-10, 20, 30, 30, 20,-10,-30,
 -30,-30,  0,  0,  0,  0,-30,-30,
 -50,-30,-30,-30,-30,-30,-30,-50
];

#[rustfmt::skip]
const BLACK_ROOK_SCORE: [i32; 64] = [
 0, 0, 0, 0, 0, 0, 0, 0,
 5, 10, 10, 10, 10, 10, 10, 5,
-5, 0, 0, 0, 0, 0, 0,-5,
-5, 0, 0, 0, 0, 0, 0,-5,
-5, 0, 0, 0, 0, 0, 0,-5,
-5, 0, 0, 0, 0, 0, 0,-5,
-5, 0, 0, 0, 0, 0, 0,-5,
 0, 0, 0, 5, 5, 0, 0, 0,
];

#[rustfmt::skip]
const BLACK_BISHOP_SCORE: [i32; 64] = [
-20,-10,-10,-10,-10,-10,-10,-20,
-10, 0, 0, 0, 0, 0, 0,-10,
-10, 0, 5, 10, 10, 5, 0,-10,
-10, 5, 5, 10, 10, 5, 5,-10,
-10, 0, 10, 10, 10, 10, 0,-10,
-10, 10, 10, 10, 10, 10, 10,-10,
-10, 5, 0, 0, 0, 0, 5,-10,
-20,-10,-10,-10,-10,-10,-10,-20,
];

#[rustfmt::skip]
const BLACK_KNIGHT_SCORE: [i32; 64] = [
-50,-40,-30,-30,-30,-30,-40,-50,
-40,-20, 0, 0, 0, 0,-20,-40,
-30, 0, 10, 15, 15, 10, 0,-30,
-30, 5, 15, 20, 20, 15, 5,-30,
-30, 0, 15, 20, 20, 15, 0,-30,
-30, 5, 10, 15, 15, 10, 5,-30,
-40,-20, 0, 5, 5, 0,-20,-40,
-50,-40,-30,-30,-30,-30,-40,-50,
];

#[rustfmt::skip]
const BLACK_QUEEN_SCORE: [i32; 64] = [
-20,-10,-10,-5,-5,-10,-10,-20,
-10, 0, 0, 0, 0, 0, 0,-10,
-10, 0, 5, 5, 5, 5, 0,-10,
-5, 0, 5, 5, 5, 5, 0,-5,
-5, 0, 5, 5, 5, 5, 0, 0,
-10, 0, 5, 5, 5, 5, 5,-10,
-10, 0, 0, 0, 0, 5, 0,-10,
-20,-10,-10,-5,-5,-10,-10,-20,
];

#[rustfmt::skip]
const BLACK_PAWN_SCORE: [i32; 64] = [
 0, 0, 0, 0, 0, 0, 0, 0,
 50, 50, 50, 50, 50, 50, 50, 50,
 10, 10, 20, 30, 30, 20, 10, 10,
 5, 5, 10, 25, 25, 10, 5, 5,
 0, 0, 0, 20, 20, 0, 0, 0,
 5,-5,-10, 0, 0,-10,-5, 5,
 5, 10, 10,-20,-20, 10, 10, 5,
 0, 0, 0, 0, 0, 0, 0, 0,
];

#[rustfmt::skip]
const BLACK_KING_MIDDLE_SCORE: [i32; 64] = [
-30,-40,-40,-50,-50,-40,-40,-30,
-30,-40,-40,-50,-50,-40,-40,-30,
-30,-40,-40,-50,-50,-40,-40,-30,
-30,-40,-40,-50,-50,-40,-40,-30,
-20,-30,-30,-40,-40,-30,-30,-20,
-10,-20,-20,-20,-20,-20,-20,-10,
 20, 20, 0, 0, 0, 0, 20, 20,
 20, 30, 10, 0, 0, 10, 30, 20,
];

#[rustfmt::skip]
const BLACK_KING_END_SCORE: [i32; 64] = [
-50,-40,-30,-20,-20,-30,-40,-50,
-30,-20,-10, 0, 0,-10,-20,-30,
-30,-10, 20, 30, 30, 20,-10,-30,
-30,-10, 30, 40, 40, 30,-10,-30,
-30,-10, 30, 40, 40, 30,-10,-30,
-30,-10, 20, 30, 30, 20,-10,-30,
-30,-30, 0, 0, 0, 0,-30,-30,
-50,-30,-30,-30,-30,-30,-30,-50,
];

pub const SQUARE_SCORES: [[[i32; 64]; 6]; 2] = [
    [
        WHITE_ROOK_SCORE,
        WHITE_BISHOP_SCORE,
        WHITE_KNIGHT_SCORE,
        WHITE_QUEEN_SCORE,
        WHITE_PAWN_SCORE,
        WHITE_KING_MIDDLE_SCORE,
    ],
    [
        BLACK_ROOK_SCORE,
        BLACK_BISHOP_SCORE,
        BLACK_KNIGHT_SCORE,
        BLACK_QUEEN_SCORE,
        BLACK_PAWN_SCORE,
        BLACK_KING_MIDDLE_SCORE,
    ],
];

pub fn print_bitboard(bb: u64) {
    for rank in (0..8).rev() {
        print!("{} | ", rank + 1);
        for file in (0..8).rev() {
            if test_bit(bb, rank * 8 + file) {
                print!("x ");
            } else {
                print!("- ");
            }
        }
        println!()
    }
    println!("   -----------------");
    println!("    A B C D E F G H ");
}

pub fn get_rank(square: u8) -> u8 {
    square >> 3
}

pub fn get_file(square: u8) -> u8 {
    square & 7
}

pub fn if_color(color: usize, white: u8, black: u8) -> u8 {
    white + (black - white) * color as u8
}

/// Shifts the bitboard north if `color` is *WHITE* (0) \
/// or south if `color` is *BLACK* (1). \
///
/// (!) This is done in a branchless manner using bit rotations, meaning that
/// bits will wrap. Often, this is not desired but is sometimes optimal.
pub fn shift_vertical(bb: u64, color: usize) -> u64 {
    bb.rotate_left(8 + (color as u32) * 48)
}

/// Adds 8 to the `square` index if `color` is *WHITE* (0) \
/// or subtracts 8 if `color` is *BLACK* (1)
pub fn shift_vertical_index(square: u8, color: usize) -> u8 {
    square + 8 - 16 * color as u8
}

pub fn shift_north(bb: u64) -> u64 {
    bb << 8
}

pub fn shift_north_east(bb: u64) -> u64 {
    (bb & !FILE_H) << 7
}

pub fn shift_east(bb: u64) -> u64 {
    (bb & !FILE_H) >> 1
}

pub fn shift_east_n(bb: u64, n: usize) -> u64 {
    let mut new_bb = bb;
    for _ in 0..n {
        new_bb = (new_bb & !FILE_H) >> 1;
    }
    new_bb
}

pub fn shift_south_east(bb: u64) -> u64 {
    (bb & !FILE_H) >> 9
}

pub fn shift_south(bb: u64) -> u64 {
    bb >> 8
}

pub fn shift_south_west(bb: u64) -> u64 {
    (bb & !FILE_A) >> 7
}

pub fn shift_west(bb: u64) -> u64 {
    (bb & !FILE_A) << 1
}

pub fn shift_west_n(bb: u64, n: usize) -> u64 {
    let mut new_bb = bb;
    for _ in 0..n {
        new_bb = (new_bb & !FILE_A) << 1;
    }
    new_bb
}

pub fn shift_north_west(bb: u64) -> u64 {
    (bb & !FILE_A) << 9
}
