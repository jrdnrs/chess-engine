use crate::{
    bitboard::*,
    bits::{clear_bit, clear_lsb, set_bit},
    moves::{self, Move, PreMoveState},
    square,
};

pub const WHITE: usize = 0;
pub const BLACK: usize = 1;
pub const ALL: usize = 2;

pub const ROOK: usize = 0;
pub const BISHOP: usize = 1;
pub const KNIGHT: usize = 2;
pub const QUEEN: usize = 3;
pub const PAWN: usize = 4;
pub const KING: usize = 5;
pub const NONE: usize = 6;

pub const PIECE_VALUE: [i32; 5] = [600, 400, 400, 1200, 100];

pub struct Board {
    pub occupancy: [u64; 3],
    pub pieces: [[u64; 6]; 2],
    pub en_passant: u64,
    pub active_color: usize,
    pub enemy_color: usize,
    pub castling_rights: [[bool; 2]; 2],
}

impl Board {
    pub fn new() -> Self {
        Self {
            occupancy: [0; 3],
            pieces: [[0; 6]; 2],
            en_passant: 0,
            active_color: WHITE,
            enemy_color: BLACK,
            castling_rights: [[false; 2]; 2],
        }
    }

    pub fn reset(&mut self) {
        self.occupancy = [0; 3];
        self.pieces = [[0; 6]; 2];
        self.en_passant = 0;
        self.active_color = WHITE;
        self.enemy_color = BLACK;
        self.castling_rights = [[false; 2]; 2];
    }

    pub fn load_fen(&mut self, fen: &str) -> Result<(), String> {
        let mut parts = fen.split_ascii_whitespace();
        let piece_placement = parts.next();
        let active_color = parts.next();
        let castling_availability = parts.next();
        let en_passant_square = parts.next();
        // let halfmove_count = parts.next();
        // let fullmove_count = parts.next();

        // not using half/full move counts for now
        if piece_placement.is_none()
            || active_color.is_none()
            || castling_availability.is_none()
            || en_passant_square.is_none()
        {
            return Err("FEN is incomplete".to_string());
        }

        // TODO: check if correct number of pieces on each row
        if let Some(pieces) = piece_placement {
            let mut square = 63;

            for char in pieces.chars() {
                if square < 0 {
                    return Err("Malformed FEN. Too many pieces".to_string());
                }

                let mut advance_by = 1;

                match char {
                    'r' => self.pieces[BLACK][ROOK] |= set_bit(0, square as u8),
                    'b' => self.pieces[BLACK][BISHOP] |= set_bit(0, square as u8),
                    'n' => self.pieces[BLACK][KNIGHT] |= set_bit(0, square as u8),
                    'q' => self.pieces[BLACK][QUEEN] |= set_bit(0, square as u8),
                    'p' => self.pieces[BLACK][PAWN] |= set_bit(0, square as u8),
                    'k' => self.pieces[BLACK][KING] |= set_bit(0, square as u8),

                    'R' => self.pieces[WHITE][ROOK] |= set_bit(0, square as u8),
                    'B' => self.pieces[WHITE][BISHOP] |= set_bit(0, square as u8),
                    'N' => self.pieces[WHITE][KNIGHT] |= set_bit(0, square as u8),
                    'Q' => self.pieces[WHITE][QUEEN] |= set_bit(0, square as u8),
                    'P' => self.pieces[WHITE][PAWN] |= set_bit(0, square as u8),
                    'K' => self.pieces[WHITE][KING] |= set_bit(0, square as u8),

                    '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' => {
                        advance_by = char.to_digit(10).unwrap() as i8
                    }

                    '/' => advance_by = 0,

                    _ => {
                        return Err(format!(
                            "Malformed FEN. '{char}' is invalid notation for piece placement"
                        ));
                    }
                }

                square -= advance_by;
            }
        }

        if let Some(color) = active_color {
            match color {
                "w" => {
                    self.active_color = WHITE;
                    self.enemy_color = BLACK;
                }
                "b" => {
                    self.active_color = BLACK;
                    self.enemy_color = WHITE;
                }
                _ => {
                    return Err(format!(
                        "Malformed FEN. '{color}' is invalid notation for active color"
                    ));
                }
            }
        }

        if let Some(castle_rights) = castling_availability {
            for char in castle_rights.chars() {
                match char {
                    'K' => self.castling_rights[WHITE][0] = true,
                    'Q' => self.castling_rights[WHITE][1] = true,
                    'k' => self.castling_rights[BLACK][0] = true,
                    'q' => self.castling_rights[BLACK][1] = true,
                    '-' => {}
                    _ => {
                        return Err(format!(
                            "Malformed FEN. '{char}' is invalid notation for castling availability"
                        ));
                    }
                }
            }
        }

        if let Some(en_passant) = en_passant_square {
            match square::parse_string(en_passant) {
                Ok(index) => self.en_passant = set_bit(0, index),
                Err(_) => match en_passant {
                    "-" => self.en_passant = 0,
                    _ => {
                        return Err(format!(
                        "Malformed FEN. '{en_passant}' is invalid notation for en passant square"
                    ))
                    }
                },
            }
        }

        self.update_occupancy();

        return Ok(());
    }

    pub fn player_score(&self, color: usize) -> i32 {
        let mut score = 0;

        for piece in 0..5 {
            let mut piece_bb = self.pieces[color][piece];

            while piece_bb > 0 {
                let square = piece_bb.trailing_zeros() as usize;
                score += SQUARE_SCORES[color][piece][63 - square];
                score += PIECE_VALUE[piece];
                piece_bb = clear_lsb(piece_bb);
            }
        }

        return score;
    }

    pub fn swap_color(&mut self) {
        (self.active_color, self.enemy_color) = (self.enemy_color, self.active_color);
    }

    pub fn make_move(&mut self, mov: &Move) {
        if mov.is_promotion() {
            self.remove_piece(PAWN, self.active_color, mov.from);
            self.add_piece(mov.piece as usize, self.active_color, mov.to);
        } else {
            self.move_piece(mov.piece as usize, self.active_color, mov.from, mov.to);

            // check for castle rights (king / rook move)
            match mov.piece as usize {
                KING => {
                    self.castling_rights[self.active_color] = [false; 2];
                }

                ROOK => match mov.from {
                    H1 => self.castling_rights[WHITE][0] = false,
                    A1 => self.castling_rights[WHITE][1] = false,
                    H8 => self.castling_rights[BLACK][0] = false,
                    A8 => self.castling_rights[BLACK][1] = false,
                    _ => {}
                },

                _ => {}
            }
        }

        if mov.is_capture() {
            let piece = self.get_piece_on_square(self.enemy_color, mov.to);
            self.remove_piece(piece, self.enemy_color, mov.to);

            // capture of rook on its start square also voids castle rights
            if piece == ROOK {
                match mov.to {
                    H1 => self.castling_rights[WHITE][0] = false,
                    A1 => self.castling_rights[WHITE][1] = false,
                    H8 => self.castling_rights[BLACK][0] = false,
                    A8 => self.castling_rights[BLACK][1] = false,
                    _ => {}
                }
            }
        }

        match mov.get_castling_bits() {
            moves::KING_SIDE => {
                let from = if_color(self.active_color, H1, H8);
                self.move_piece(ROOK, self.active_color, from, from + 2);
                self.castling_rights[self.active_color] = [false; 2];
            }

            moves::QUEEN_SIDE => {
                let from = if_color(self.active_color, A1, A8);
                self.move_piece(ROOK, self.active_color, from, from - 3);
                self.castling_rights[self.active_color] = [false; 2];
            }

            _ => {}
        }

        match mov.get_en_pass_bits() {
            moves::EN_PASSANT_MOVE => {
                let square = shift_vertical_index(mov.from, self.active_color);
                self.en_passant = set_bit(0, square);
            }

            moves::EN_PASSANT_CAP => {
                let en_pass_square = self.en_passant.trailing_zeros() as u8;
                let pawn_square = shift_vertical_index(en_pass_square, self.enemy_color);
                self.remove_piece(PAWN, self.enemy_color, pawn_square);
                self.en_passant = 0;
            }

            _ => {
                self.en_passant = 0;
            }
        }

        self.swap_color();
        self.update_occupancy();
    }

    pub fn unmake_move(&mut self, mov: &Move, state: PreMoveState) {
        self.swap_color();
        self.castling_rights = state.get_castling_rights();
        self.en_passant = state.get_en_passant();

        if mov.is_promotion() {
            self.add_piece(PAWN, self.active_color, mov.from);
            self.remove_piece(mov.piece as usize, self.active_color, mov.to);
        } else {
            self.move_piece(mov.piece as usize, self.active_color, mov.to, mov.from);
        }

        if mov.is_capture() {
            let piece = state.get_captured_piece();
            self.add_piece(piece, self.enemy_color, mov.to);
        }

        match mov.get_castling_bits() {
            moves::KING_SIDE => {
                let from = if_color(self.active_color, H1 + 2, H8 + 2);
                self.move_piece(ROOK, self.active_color, from, from - 2);
            }

            moves::QUEEN_SIDE => {
                let from = if_color(self.active_color, A1 - 3, A8 - 3);
                self.move_piece(ROOK, self.active_color, from, from + 3);
            }

            _ => {}
        }

        match mov.get_en_pass_bits() {
            moves::EN_PASSANT_MOVE => {}

            moves::EN_PASSANT_CAP => {
                let en_pass_square = self.en_passant.trailing_zeros() as u8;
                let pawn_square = shift_vertical_index(en_pass_square, self.enemy_color);
                self.add_piece(PAWN, self.enemy_color, pawn_square);
            }

            _ => {}
        }

        self.update_occupancy();
    }

    pub fn get_piece_on_square(&self, color: usize, square: u8) -> usize {
        let b = set_bit(0, square);
        for piece in 0..6 {
            if (b & self.pieces[color][piece]) != 0 {
                return piece;
            }
        }
        return NONE;
    }

    pub fn add_piece(&mut self, piece: usize, color: usize, square: u8) {
        self.pieces[color][piece] = set_bit(self.pieces[color][piece], square);
    }

    pub fn remove_piece(&mut self, piece: usize, color: usize, square: u8) {
        self.pieces[color][piece] = clear_bit(self.pieces[color][piece], square);
    }

    pub fn move_piece(&mut self, piece: usize, color: usize, from: u8, to: u8) {
        self.pieces[color][piece] = clear_bit(self.pieces[color][piece], from);
        self.pieces[color][piece] = set_bit(self.pieces[color][piece], to);
    }

    pub fn update_occupancy(&mut self) {
        self.occupancy[WHITE] = self.pieces[WHITE][ROOK]
            | self.pieces[WHITE][BISHOP]
            | self.pieces[WHITE][KNIGHT]
            | self.pieces[WHITE][QUEEN]
            | self.pieces[WHITE][PAWN]
            | self.pieces[WHITE][KING];

        self.occupancy[BLACK] = self.pieces[BLACK][ROOK]
            | self.pieces[BLACK][BISHOP]
            | self.pieces[BLACK][KNIGHT]
            | self.pieces[BLACK][QUEEN]
            | self.pieces[BLACK][PAWN]
            | self.pieces[BLACK][KING];

        self.occupancy[ALL] = self.occupancy[WHITE] | self.occupancy[BLACK];
    }
}
