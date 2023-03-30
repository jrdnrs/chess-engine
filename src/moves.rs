use crate::{
    bits::{clear_lsb, set_bit},
    board::{BLACK, WHITE}, sort::selection_sort_once,
};

pub const CAPTURE: u8 = 1;
pub const PROMOTION: u8 = 1 << 1;
pub const KING_SIDE: u8 = 1 << 2;
pub const QUEEN_SIDE: u8 = 2 << 2;
pub const EN_PASSANT_MOVE: u8 = 1 << 4;
pub const EN_PASSANT_CAP: u8 = 2 << 4;

#[derive(Clone)]
#[repr(C)]
pub struct Move {
    pub piece: u8,
    pub from: u8,
    pub to: u8,
    pub flags: u8,
}

impl Move {
    const CAPTURE_MASK: u8 = CAPTURE;
    const PROMOTION_MASK: u8 = PROMOTION;
    const CASTLING_MASK: u8 = KING_SIDE | QUEEN_SIDE;
    const EN_PASSANT_MASK: u8 = EN_PASSANT_MOVE | EN_PASSANT_CAP;

    pub fn new(piece: usize, from: u8, to: u8, flags: u8) -> Self {
        Self {
            piece: piece as u8,
            from,
            to,
            flags,
        }
    }

    pub fn is_capture(&self) -> bool {
        self.flags & Self::CAPTURE_MASK > 0
    }

    pub fn is_promotion(&self) -> bool {
        self.flags & Self::PROMOTION_MASK > 0
    }

    pub fn is_en_passant(&self) -> bool {
        self.flags & Self::EN_PASSANT_MASK > 0
    }

    pub fn is_castle(&self) -> bool {
        self.flags & Self::CASTLING_MASK > 0
    }

    pub fn get_castling_bits(&self) -> u8 {
        self.flags & Self::CASTLING_MASK
    }

    pub fn get_en_pass_bits(&self) -> u8 {
        self.flags & Self::EN_PASSANT_MASK
    }
}

pub struct IndexOfNextBest {
    evals: Vec<(i32, usize)>,
    current_index: usize,
}

impl IndexOfNextBest {
    pub fn new(evals: Vec<(i32, usize)>) -> Self {
        Self {
            evals,
            current_index: 0,
        }
    }
}

impl Iterator for IndexOfNextBest {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_index >= self.evals.len() {
            return None;
        }
        selection_sort_once(&mut self.evals[self.current_index..]);
        let ret = Some(self.evals[self.current_index].1);
        self.current_index += 1;
        return ret;
    }
}

pub fn moves_from_bitboard(
    start_square: u8,
    piece: usize,
    bb: u64,
    flags: u8,
    list: &mut Vec<Move>,
) {
    let mut copy_bb = bb;

    while copy_bb > 0 {
        let square = copy_bb.trailing_zeros() as u8;
        copy_bb = clear_lsb(copy_bb);
        list.push(Move::new(piece, start_square, square, flags));
    }
}

pub struct PreMoveState {
    bits: u16,
}

impl PreMoveState {
    const EN_PASSANT_MASK: u16 = 0b111111 << 4;

    pub fn new(castling_rights: [[bool; 2]; 2], en_passant: u64, captured_piece: usize) -> Self {
        let castling_bits = (castling_rights[WHITE][0] as u16) << 3
            | (castling_rights[WHITE][1] as u16) << 2
            | (castling_rights[BLACK][0] as u16) << 1
            | (castling_rights[BLACK][1] as u16);
        let en_passant_bits = (en_passant.trailing_zeros() as u16) << 4;
        let captured_piece_bits = (captured_piece as u16) << 11;

        Self {
            bits: castling_bits | en_passant_bits | captured_piece_bits,
        }
    }

    pub fn get_castling_rights(&self) -> [[bool; 2]; 2] {
        [
            [
                ((self.bits & 0b1000) >> 3) != 0,
                ((self.bits & 0b0100) >> 2) != 0,
            ],
            [((self.bits & 0b0010) >> 1) != 0, (self.bits & 0b0001) != 0],
        ]
    }

    pub fn get_en_passant(&self) -> u64 {
        set_bit(0, ((self.bits & Self::EN_PASSANT_MASK) >> 4) as u8)
    }

    pub fn get_captured_piece(&self) -> usize {
        (self.bits >> 11) as usize
    }
}
