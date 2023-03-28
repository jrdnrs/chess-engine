use crate::{bitboard::*, bits::*, board::*, moves::*, pregen};

/// order: white, black
const BACK_RANK: [u8; 2] = [0, 7];
const PAWN_RANK: [u8; 2] = [1, 6];
const PENULT_RANK: [u8; 2] = [6, 1];

const CASTLE_SAFE_SQUARES: [[u64; 2]; 2] = [[0xE, 0x38], [0xE00000000000000, 0x3800000000000000]];
const CASTLE_OPEN_SQUARES: [[u64; 2]; 2] = [[0x6, 0x70], [0x600000000000000, 0x7000000000000000]];

const NEG_INF: i32 = i32::MIN + 1;
const POS_INF: i32 = i32::MAX;

const TABLE_SIZE: usize = 2 << 5;
const TABLE_MODULUS: usize = TABLE_SIZE - 1;

pub struct Engine {
    pub board: Board,

    /// A mask whereby set bits represent squares that can be moved to.\
    ///
    /// This includes the squares of pieces that are giving check, as well as any squares between that piece
    /// and the king, if it is a sliding piece.\
    ///
    /// Thus, a check mask is equal to `u64::MAX` when there are no checks given
    pub check_mask: u64,

    /// A mask whereby set bits represent squares that are threatened by the enemy.\
    ///
    /// It is important to note that this disregards the ally king as a blocker during creation. It is
    /// favourable to do so as, for sliding attacks aimed at the king, squares behind would otherwise be
    /// considered safe.\
    ///
    /// Thus, a threat mask is equal to nil when no squares are threatened.
    pub threat_mask: u64,

    /// A mask whereby set bits represent squares that can be moved to (sometimes).\
    ///
    /// Each represents a different directional line running through the ally king's square, in the order:\
    /// *horizontal*, *vertical*, *diagonal (foward slash)*, *diagonal (back slash)*\
    /// This includes the squares that pieces can move on to maintain a pin. Each pin mask will be set when exactly
    /// one ally piece, and no enemy pieces, block an enemy sliding attack aimed at the ally king.\
    ///
    /// (!) It is very important to note that, unlike with the check mask, it will be equal to nil when
    /// all squares are valid. This is because the four pin masks are combined in various ways when generating
    /// moves for each piece type. It is after combining that it then becomes `u64::MAX` upon there being
    /// no pinned squares.
    pub pin_mask: [u64; 4],

    /// A mask whereby set bits represent squares that can be moved to.\
    ///
    /// This is maintained specifically for the en passant pawn, to handle an edge case that arises due to
    /// an en passant capture being the only capture that does not maintain occupancy on the square, thus
    /// possibly revealing a check
    pub en_pass_pin_mask: u64,

    /// A mask whereby set bits represent squares that can be moved to.\
    ///
    /// This is maintained specifically for the en passant pawn, as it identifies a capturable square for pawns
    /// which otherwise would have been missed due to the way that move generation works. This is because, in
    /// this case, the square being captured has no piece on it.
    pub en_pass_check_mask: u64,

    /// The number of checks made on the ally king
    pub checks: usize,

    pub history: Vec<PreMoveState>,

    pub rays: pregen::Rays,
    pub blocker_masks: pregen::BlockerMasks,
    pub attack_sets: pregen::AttackSets,
    pub transposition_table: pregen::TranspositionTable<TABLE_SIZE>,
}

impl Engine {
    pub fn new() -> Self {
        let rays = pregen::Rays::new();
        let blocker_masks = pregen::BlockerMasks::new(&rays);
        let attack_sets = pregen::AttackSets::new(&rays, &blocker_masks);
        let transposition_table = pregen::TranspositionTable::new();

        Self {
            board: Board::new(),
            check_mask: 0,
            threat_mask: 0,
            pin_mask: [0; 4],
            en_pass_pin_mask: 0,
            en_pass_check_mask: 0,
            checks: 0,
            history: Vec::new(),
            rays,
            blocker_masks,
            attack_sets,
            transposition_table,
        }
    }

    pub fn reset(&mut self) {
        self.board.reset();
        self.check_mask = 0;
        self.threat_mask = 0;
        self.pin_mask = [0; 4];
        self.en_pass_check_mask = 0;
        self.en_pass_pin_mask = 0;
        self.checks = 0;
        self.history.clear();
    }

    pub fn load_fen(&mut self, fen: &str) -> Result<(), String> {
        self.reset();
        self.board.load_fen(fen)?;
        self.update_threats();
        self.update_pins_and_checks();
        Ok(())
    }

    fn order_moves(&self, moves: &Vec<Move>) -> Vec<usize> {
        let mut scores = Vec::with_capacity(moves.len());
        let mut order = Vec::with_capacity(moves.len());

        for (i, mov) in moves.iter().enumerate() {
            let mut score = 0;
            let piece;

            if mov.is_promotion() {
                piece = PAWN as i32;
                score += mov.piece as i32;
            } else {
                piece = mov.piece as i32;
            }

            if mov.is_capture() {
                let captured_piece = self
                    .board
                    .get_piece_on_square(self.board.enemy_color, mov.to);

                score += 10 * PIECE_VALUE[captured_piece] as i32 - piece;
            }

            scores.push(score);
            order.push(i);
        }

        order.sort_unstable_by_key(|i| scores[*i]);

        return order;
    }

    pub fn best_move(&mut self, depth: u16) -> Option<Move> {
        let moves = self.generate_moves();
        if moves.is_empty() {
            return None;
        }

        let mut best_score = NEG_INF;
        let mut best_move = &moves[0];

        for i in self.order_moves(&moves) {
            let mov = &moves[i];

            self.make_move(mov);
            let score = -self.negamax(depth - 1, -POS_INF, -best_score);
            self.unmake_move(mov);

            if score > best_score {
                best_score = score;
                best_move = mov;
            }
        }

        return Some(best_move.clone());
    }

    fn negamax(&mut self, depth: u16, mut alpha: i32, beta: i32) -> i32 {
        if depth == 0 {
            return self.quiesce(alpha, beta);
        }

        let moves = self.generate_moves();
        if moves.is_empty() {
            if self.checks > 0 {
                return NEG_INF;
            }
            return 0;
        }

        for i in self.order_moves(&moves) {
            let mov = &moves[i];

            self.make_move(mov);
            let eval = -self.negamax(depth - 1, -beta, -alpha);
            self.unmake_move(mov);

            if eval >= beta {
                return beta;
            }

            alpha = alpha.max(eval);
        }

        return alpha;
    }

    fn quiesce(&mut self, mut alpha: i32, beta: i32) -> i32 {
        let eval = self.evaluate();
        if eval >= beta {
            return beta;
        }
        alpha = alpha.max(eval);

        let moves = self.generate_moves();

        for mov in moves.iter() {
            if !mov.is_capture() {
                continue;
            }

            self.make_move(mov);
            let eval = -self.quiesce(-beta, -alpha);
            self.unmake_move(mov);

            if eval >= beta {
                return beta;
            }

            alpha = alpha.max(eval);
        }

        return alpha;
    }

    fn evaluate(&self) -> i32 {
        self.board.material_score(self.board.active_color)
            - self.board.material_score(self.board.enemy_color)
    }

    pub fn make_move(&mut self, mov: &Move) {
        // TODO: we are doing `get_piece_on_square` twice potentially (also inside `self.board.make_move`)
        let captured_piece = self
            .board
            .get_piece_on_square(self.board.enemy_color, mov.to);
        let state = PreMoveState::new(
            self.board.castling_rights,
            self.board.en_passant,
            captured_piece,
        );
        self.history.push(state);
        self.board.make_move(mov);
    }

    pub fn unmake_move(&mut self, mov: &Move) {
        self.board.unmake_move(mov, self.history.pop().unwrap());
    }

    /// Calculates threats from opponent\
    /// (!) Requires up-to-date occupancy
    fn update_threats(&mut self) {
        self.threat_mask = 0;
        let all_but_king =
            self.board.occupancy[ALL] & !self.board.pieces[self.board.active_color][KING];

        let mut rooks = self.board.pieces[self.board.enemy_color][ROOK];
        while rooks > 0 {
            let square = rooks.trailing_zeros() as usize;

            self.threat_mask |= self
                .attack_sets
                .get_rook_attacks(square, self.blocker_masks.0[ROOK][square] & all_but_king);

            rooks = clear_lsb(rooks);
        }

        let mut bishops = self.board.pieces[self.board.enemy_color][BISHOP];
        while bishops > 0 {
            let square = bishops.trailing_zeros() as usize;

            self.threat_mask |= self
                .attack_sets
                .get_bishop_attacks(square, self.blocker_masks.0[BISHOP][square] & all_but_king);

            bishops = clear_lsb(bishops);
        }

        let mut knights = self.board.pieces[self.board.enemy_color][KNIGHT];
        while knights > 0 {
            let square = knights.trailing_zeros() as usize;

            self.threat_mask |= self.attack_sets.knight[square];

            knights = clear_lsb(knights);
        }

        let mut queens = self.board.pieces[self.board.enemy_color][QUEEN];
        while queens > 0 {
            let square = queens.trailing_zeros() as usize;

            self.threat_mask |= self
                .attack_sets
                .get_rook_attacks(square, self.blocker_masks.0[ROOK][square] & all_but_king)
                | self.attack_sets.get_bishop_attacks(
                    square,
                    self.blocker_masks.0[BISHOP][square] & all_but_king,
                );

            queens = clear_lsb(queens);
        }

        let mut pawns = self.board.pieces[self.board.enemy_color][PAWN];
        while pawns > 0 {
            let square = pawns.trailing_zeros() as usize;

            self.threat_mask |= self.attack_sets.pawn[self.board.enemy_color][square];

            pawns = clear_lsb(pawns);
        }

        let king = self.board.pieces[self.board.enemy_color][KING];
        let square = king.trailing_zeros() as usize;
        self.threat_mask |= self.attack_sets.king[square];
    }

    fn update_en_passant_pin(&mut self, king_to_enemy_mask: u64, enemy_blockers: u64) {
        let en_passant_pawn = shift_vertical(self.board.en_passant, self.board.enemy_color);

        let pawn_is_blocker = (en_passant_pawn & enemy_blockers) == enemy_blockers;

        if !pawn_is_blocker {
            return;
        };

        let side_mask = en_passant_pawn << 1 | en_passant_pawn >> 1;
        let side_ally_pawns_count =
            (side_mask & self.board.pieces[self.board.active_color][PAWN]).count_ones();
        let other_ally_blockers =
            king_to_enemy_mask & self.board.occupancy[self.board.active_color] & !side_mask;

        if other_ally_blockers == 0
            && self.board.en_passant & king_to_enemy_mask == 0
            && (side_mask & king_to_enemy_mask == 0 || side_ally_pawns_count < 2)
        {
            self.en_pass_pin_mask = 0;
        }
    }

    /// Calculates pinned allys and checks from opponent\
    /// (!) Requires up-to-date occupancy
    fn update_pins_and_checks(&mut self) {
        self.checks = 0;
        self.check_mask = 0;
        self.en_pass_check_mask = 0;
        self.en_pass_pin_mask = u64::MAX;

        let king_square =
            self.board.pieces[self.board.active_color][KING].trailing_zeros() as usize;
        let enemy_rq = self.board.pieces[self.board.enemy_color][ROOK]
            | self.board.pieces[self.board.enemy_color][QUEEN];
        let enemy_bq = self.board.pieces[self.board.enemy_color][BISHOP]
            | self.board.pieces[self.board.enemy_color][QUEEN];

        let mut pin_masks = [0; 8];

        for direction in 0..8 {
            let king_ray = self.rays.0[direction][king_square];

            // first four directions are cardinal, last four are diagonal
            let enemies = if direction < 4 {
                king_ray & enemy_rq
            } else {
                king_ray & enemy_bq
            };

            if enemies == 0 {
                continue;
            }

            // TODO: make this less bad,
            //       we need `trailing, trailing, leading, leading, ...' hence '(direction >> 1) & 1 == 1'
            //       because which we use depends on the direction to get closest bit to king square
            let enemy_square = if ((direction >> 1) & 1) == 1 {
                63 - enemies.leading_zeros() as usize
            } else {
                enemies.trailing_zeros() as usize
            };

            let enemy_ray = self.rays.0[direction][enemy_square];
            let king_to_enemy_mask = king_ray & !enemy_ray;

            let enemy_blockers =
                king_to_enemy_mask & self.board.occupancy[self.board.enemy_color] & !enemies;

            match enemy_blockers.count_ones() {
                0 => {
                    let ally_blockers_count = (king_to_enemy_mask
                        & self.board.occupancy[self.board.active_color])
                        .count_ones();

                    match ally_blockers_count {
                        // 0 means checker enemy sliding checker
                        0 => {
                            self.check_mask |= king_to_enemy_mask;
                            self.checks += 1;
                        }

                        // only a pin if there is exactly one ally blocker
                        1 => {
                            pin_masks[direction] = king_to_enemy_mask;
                        }

                        _ => {}
                    }
                }

                // potential en passant pawn blocker (annoying edge case)
                1 => {
                    self.update_en_passant_pin(king_to_enemy_mask, enemy_blockers);
                }

                _ => {}
            }
        }

        let knight_checkers = self.attack_sets.knight[king_square]
            & self.board.pieces[self.board.enemy_color][KNIGHT];

        let pawn_checkers = self.attack_sets.pawn[self.board.active_color][king_square]
            & self.board.pieces[self.board.enemy_color][PAWN];

        self.en_pass_check_mask =
            shift_vertical(pawn_checkers, self.board.active_color) & self.board.en_passant;

        self.checks += (knight_checkers.count_ones() + pawn_checkers.count_ones()) as usize;
        self.check_mask |= knight_checkers | pawn_checkers;

        if self.check_mask == 0 {
            debug_assert!(self.checks == 0);
            self.check_mask = u64::MAX;
        }

        if self.en_pass_check_mask == 0 {
            self.en_pass_check_mask = u64::MAX;
        }

        self.pin_mask[0] = pin_masks[1] | pin_masks[3];
        self.pin_mask[1] = pin_masks[0] | pin_masks[2];
        self.pin_mask[2] = pin_masks[4] | pin_masks[6];
        self.pin_mask[3] = pin_masks[5] | pin_masks[7];
    }

    pub fn generate_moves(&mut self) -> Vec<Move> {
        let mut moves = Vec::new();

        self.update_threats();
        self.update_pins_and_checks();

        // if double check only king can move
        if self.checks < 2 {
            self.rook_moves(&mut moves);
            self.bishop_moves(&mut moves);
            self.knight_moves(&mut moves);
            self.queen_moves(&mut moves);
            self.pawn_moves(&mut moves);
        }
        self.king_moves(&mut moves);

        return moves;
    }

    fn rook_moves(&mut self, list: &mut Vec<Move>) {
        let mut rooks = self.board.pieces[self.board.active_color][ROOK];
        let pin_squares_hv = self.pin_mask[0] | self.pin_mask[1];
        let pin_squares_diag1 = self.pin_mask[2];
        let pin_squares_diag2 = self.pin_mask[3];

        while rooks > 0 {
            let square = rooks.trailing_zeros() as u8;
            let rook = isolate_bit(rooks, square);

            let pinmask = if rook & pin_squares_hv > 0 {
                pin_squares_hv
            } else if rook & pin_squares_diag1 > 0 {
                pin_squares_diag1
            } else if rook & pin_squares_diag2 > 0 {
                pin_squares_diag2
            } else {
                u64::MAX
            };

            let blocker_mask = self.blocker_masks.0[ROOK][square as usize];
            let blockers = blocker_mask & self.board.occupancy[ALL];
            let attacks = self.attack_sets.get_rook_attacks(square as usize, blockers)
                & !self.board.occupancy[self.board.active_color]
                & self.check_mask
                & pinmask;
            let captures = attacks & self.board.occupancy[self.board.enemy_color];
            let moves = attacks & !self.board.occupancy[self.board.enemy_color];

            moves_from_bitboard(square, ROOK, moves, 0, list);
            moves_from_bitboard(square, ROOK, captures, CAPTURE, list);

            rooks = clear_lsb(rooks);
        }
    }

    fn bishop_moves(&mut self, list: &mut Vec<Move>) {
        let mut bishops = self.board.pieces[self.board.active_color][BISHOP];
        let pin_squares_diag = self.pin_mask[2] | self.pin_mask[3];
        let pin_squares_h = self.pin_mask[0];
        let pin_squares_v = self.pin_mask[1];

        while bishops > 0 {
            let square = bishops.trailing_zeros() as u8;
            let bishop = isolate_bit(bishops, square);

            let pinmask = if bishop & pin_squares_diag > 0 {
                pin_squares_diag
            } else if bishop & pin_squares_h > 0 {
                pin_squares_h
            } else if bishop & pin_squares_v > 0 {
                pin_squares_v
            } else {
                u64::MAX
            };

            let blocker_mask = self.blocker_masks.0[BISHOP][square as usize];
            let blockers = blocker_mask & self.board.occupancy[ALL];
            let attacks = self
                .attack_sets
                .get_bishop_attacks(square as usize, blockers)
                & !self.board.occupancy[self.board.active_color]
                & self.check_mask
                & pinmask;
            let captures = attacks & self.board.occupancy[self.board.enemy_color];
            let moves = attacks & !self.board.occupancy[self.board.enemy_color];

            moves_from_bitboard(square, BISHOP, moves, 0, list);
            moves_from_bitboard(square, BISHOP, captures, CAPTURE, list);

            bishops = clear_lsb(bishops);
        }
    }

    fn queen_moves(&mut self, list: &mut Vec<Move>) {
        let mut queens = self.board.pieces[self.board.active_color][QUEEN];

        while queens > 0 {
            let square = queens.trailing_zeros() as u8;
            let queen = isolate_bit(queens, square);

            let pinmask = if queen & self.pin_mask[0] > 0 {
                self.pin_mask[0]
            } else if queen & self.pin_mask[1] > 0 {
                self.pin_mask[1]
            } else if queen & self.pin_mask[2] > 0 {
                self.pin_mask[2]
            } else if queen & self.pin_mask[3] > 0 {
                self.pin_mask[3]
            } else {
                u64::MAX
            };

            let r_blocker_mask = self.blocker_masks.0[ROOK][square as usize];
            let r_blockers = r_blocker_mask & self.board.occupancy[ALL];
            let r_attacks = self
                .attack_sets
                .get_rook_attacks(square as usize, r_blockers);

            let b_blocker_mask = self.blocker_masks.0[BISHOP][square as usize];
            let b_blockers = b_blocker_mask & self.board.occupancy[ALL];
            let b_attacks = self
                .attack_sets
                .get_bishop_attacks(square as usize, b_blockers);

            let attacks = (b_attacks | r_attacks)
                & !self.board.occupancy[self.board.active_color]
                & self.check_mask
                & pinmask;
            let captures = attacks & self.board.occupancy[self.board.enemy_color];
            let moves = attacks & !self.board.occupancy[self.board.enemy_color];

            moves_from_bitboard(square, QUEEN, moves, 0, list);
            moves_from_bitboard(square, QUEEN, captures, CAPTURE, list);

            queens = clear_lsb(queens);
        }
    }

    fn knight_moves(&mut self, list: &mut Vec<Move>) {
        let mut knights = self.board.pieces[self.board.active_color][KNIGHT];
        let pin_squares = self.pin_mask[0] | self.pin_mask[1] | self.pin_mask[2] | self.pin_mask[3];

        while knights > 0 {
            let square = knights.trailing_zeros() as u8;
            let knight = isolate_bit(knights, square);

            // no moves if pinned
            if (pin_squares & knight) > 0 {
                knights = clear_lsb(knights);
                continue;
            }

            let attacks = self.attack_sets.knight[square as usize]
                & !self.board.occupancy[self.board.active_color]
                & self.check_mask;
            let captures = attacks & self.board.occupancy[self.board.enemy_color];
            let moves = attacks & !self.board.occupancy[self.board.enemy_color];

            moves_from_bitboard(square, KNIGHT, moves, 0, list);
            moves_from_bitboard(square, KNIGHT, captures, CAPTURE, list);

            knights = clear_lsb(knights);
        }
    }

    fn pawn_moves(&self, list: &mut Vec<Move>) {
        let mut pawns = self.board.pieces[self.board.active_color][PAWN];

        while pawns > 0 {
            let square = pawns.trailing_zeros() as u8;
            let pawn = isolate_bit(pawns, square);
            let rank = get_rank(square);
            let promotion = rank == PENULT_RANK[self.board.active_color];

            let pinmask = if pawn & self.pin_mask[0] > 0 {
                self.pin_mask[0]
            } else if pawn & self.pin_mask[1] > 0 {
                self.pin_mask[1]
            } else if pawn & self.pin_mask[2] > 0 {
                self.pin_mask[2]
            } else if pawn & self.pin_mask[3] > 0 {
                self.pin_mask[3]
            } else {
                u64::MAX
            };

            let attacks = self.attack_sets.pawn[self.board.active_color][square as usize];

            // - `move once` is later used to generate the double move (if relevant) which is why we &! with
            //   all pieces now, as it will then take care of the case where we can't move forward twice because we
            //   are blocked
            // - we can use rotate here with no worry of wrapping bits, as there can never be a pawn on the last rank
            let move_once =
                shift_vertical(pawn, self.board.active_color) & !self.board.occupancy[ALL];
            let moves = move_once & pinmask & self.check_mask;

            // capture
            let captures =
                attacks & self.board.occupancy[self.board.enemy_color] & pinmask & self.check_mask;

            // if pawn is one away from last rank, 'move once' and 'capture' are promoting moves
            if promotion {
                // when PROMOTION flag is set, piece represents piece promoted to
                // TODO: do i actually need to gen 4 different moves??
                for piece in 0..4 {
                    moves_from_bitboard(square, piece, moves, PROMOTION, list);
                    moves_from_bitboard(square, piece, captures, CAPTURE | PROMOTION, list);
                }
                pawns = clear_lsb(pawns);
                continue;
            } else {
                moves_from_bitboard(square, PAWN, moves, 0, list);
                moves_from_bitboard(square, PAWN, captures, CAPTURE, list);
            }

            // move twice
            if rank == PAWN_RANK[self.board.active_color] {
                let move_twice = shift_vertical(move_once, self.board.active_color);
                let moves = move_twice & !self.board.occupancy[ALL] & pinmask & self.check_mask;

                moves_from_bitboard(square, PAWN, moves, EN_PASSANT_MOVE, list);
                pawns = clear_lsb(pawns);
                continue;
            }

            let en_passant_cap = attacks
                & self.board.en_passant
                & pinmask
                & self.en_pass_check_mask
                & self.en_pass_pin_mask;
            moves_from_bitboard(square, PAWN, en_passant_cap, EN_PASSANT_CAP, list);

            pawns = clear_lsb(pawns);
        }
    }

    fn king_moves(&mut self, list: &mut Vec<Move>) {
        let king = self.board.pieces[self.board.active_color][KING];
        let square = king.trailing_zeros() as u8;

        let attacks = self.attack_sets.king[square as usize]
            & !self.board.occupancy[self.board.active_color]
            & !self.threat_mask;
        let captures = attacks & self.board.occupancy[self.board.enemy_color];
        let moves = attacks & !self.board.occupancy[self.board.enemy_color];

        moves_from_bitboard(square, KING, moves, 0, list);
        moves_from_bitboard(square, KING, captures, CAPTURE, list);

        // castling
        // king-side
        if self.board.castling_rights[self.board.active_color][0] {
            let safe = CASTLE_SAFE_SQUARES[self.board.active_color][0] & self.threat_mask == 0;
            let open =
                CASTLE_OPEN_SQUARES[self.board.active_color][0] & self.board.occupancy[ALL] == 0;
            if safe && open {
                list.push(Move::new(KING, square, square - 2, KING_SIDE));
            }
        }

        // queen-side
        if self.board.castling_rights[self.board.active_color][1] {
            let safe = CASTLE_SAFE_SQUARES[self.board.active_color][1] & self.threat_mask == 0;
            let open =
                CASTLE_OPEN_SQUARES[self.board.active_color][1] & self.board.occupancy[ALL] == 0;
            if safe && open {
                list.push(Move::new(KING, square, square + 2, QUEEN_SIDE));
            }
        }
    }
}

mod tests {
    use crate::square;

    use super::*;

    const STARTING_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

    fn print_state(game: &mut Engine) {
        const COLORS: [&str; 2] = ["White", "Black"];
        const PIECES: [&str; 6] = ["Rook", "Bishop", "Knight", "Queen", "Pawn", "King"];

        println!();
        println!("Active Color: {}", COLORS[game.board.active_color]);
        println!("Other Color: {}", COLORS[game.board.enemy_color]);
        println!("Checks: {}", game.checks);

        for color in 0..2 {
            println!(
                "Castling available [{}] = kingside: {}, queenside: {}",
                COLORS[color],
                game.board.castling_rights[color][0],
                game.board.castling_rights[color][1]
            );
        }

        println!("\r\nEn Passant:");
        print_bitboard(game.board.en_passant);

        println!("\r\nThreat Mask:");
        print_bitboard(game.threat_mask);

        println!("\r\nCheck Mask:");
        print_bitboard(game.check_mask);

        println!("\r\nPin Mask (-):");
        print_bitboard(game.pin_mask[0]);

        println!("\r\nPin Mask (|):");
        print_bitboard(game.pin_mask[1]);

        println!("\r\nPin Mask (/):");
        print_bitboard(game.pin_mask[2]);

        println!("\r\nPin Mask (\\):");
        print_bitboard(game.pin_mask[3]);

        println!("\r\nOccupancy [WHITE]:");
        print_bitboard(game.board.occupancy[WHITE]);

        println!("\r\nOccupancy [BLACK]:");
        print_bitboard(game.board.occupancy[BLACK]);

        println!("\r\nOccupancy [ALL]:");
        print_bitboard(game.board.occupancy[ALL]);

        for color in 0..2 {
            for piece in 0..6 {
                println!("\r\nPiece [{}][{}]:", COLORS[color], PIECES[piece]);
                print_bitboard(game.board.pieces[color][piece]);
            }
        }

        println!("\r\nLegal Moves: ");
        let moves = game.generate_moves();
        for mov in moves {
            println!(
                "Piece: {:12} From: {:8} To: {:8}",
                PIECES[mov.piece as usize],
                square::to_string(mov.from),
                square::to_string(mov.to)
            );
        }
    }

    fn print_board(board: &Board) {
        const PIECES: [[char; 7]; 2] = [
            ['♜', '♝', '♞', '♛', '♟', '♚', ' '],
            ['♖', '♗', '♘', '♕', '♙', '♔', ' '],
            // ['R', 'B', 'N', 'Q', 'P', 'K', ' '],
            // ['r', 'b', 'n', 'q', 'p', 'k', ' '],
        ];

        for rank in (0..8).rev() {
            print!("{} | ", rank + 1);
            for file in (0..8).rev() {
                let square = rank * 8 + file;
                let mut piece = NONE;
                let mut color = 0;
                for c in 0..2 {
                    for p in 0..6 {
                        if test_bit(board.pieces[c][p], square) {
                            piece = p;
                            color = c;
                        }
                    }
                }
                print!("{} ", PIECES[color][piece]);
            }
            println!()
        }
        println!("   -----------------");
        println!("    A B C D E F G H ");
    }

    #[test]
    fn fen_test() {
        let mut game = Engine::new();

        if let Err(err) = game.load_fen("1k6/8/8/1qPpP1K1/8/8/8/8 w - d6 0 1") {
            eprintln!("{err}");
            return;
        }

        print_state(&mut game);

        let mov = Move::new(
            KING,
            square::parse_string("g5").unwrap(),
            square::parse_string("h4").unwrap(),
            0,
        );

        game.make_move(&mov);

        print_state(&mut game);

        game.unmake_move(&mov);

        print_state(&mut game);
    }

    fn perft(engine: &mut Engine, depth: u8) -> usize {
        if depth == 0 {
            return 1;
        }

        let moves = engine.generate_moves();
        let mut positions = 0;

        for mov in moves.iter() {
            engine.make_move(mov);
            positions += perft(engine, depth - 1);
            engine.unmake_move(mov);
        }

        return positions;
    }

    fn go_perft(engine: &mut Engine, depth: u8) {
        let moves = engine.generate_moves();

        let mut total_positions = 0;

        for mov in moves.iter() {
            engine.make_move(mov);
            let positions = perft(engine, depth - 1);
            total_positions += positions;

            println!(
                "{}{}: {}",
                square::to_string(mov.from),
                square::to_string(mov.to),
                positions
            );

            engine.unmake_move(&mov);
        }

        println!("\r\nNodes searched: {total_positions}");
    }

    #[test]
    fn en_passant_test() {
        let mut engine = Engine::new();

        const FENS: [(&str, bool); 11] = [
            ("k7/3q4/8/3pP3/8/8/3K4/8 w - d6 0 1", true), // one pawn vertical (en passant capture creates block)
            ("k7/3q4/8/2PpP3/8/8/3K4/8 w - d6 0 1", true), // two pawns vertical (en passant capture creates block)
            ("k7/8/8/1qPpP1K1/8/8/8/8 w - d6 0 1", true),  // two pawns horiz
            ("k7/8/8/1q1pPNK1/8/8/8/8 w - d6 0 1", true),  // one pawn and knight horiz
            ("k7/1q6/8/3p4/8/8/2K5/8 w - d6 0 1", true),   // no pawns (no threat)
            ("k7/1q6/8/3pP3/8/8/2K5/8 w - d6 0 1", true),  // one pawn (no threat)
            ("k7/8/2q5/2PpP3/8/8/6K1/8 w - d6 0 1", false), // two pawns diag
            ("k7/8/2q5/3pP3/8/8/6K1/8 w - d6 0 1", false), // one pawn diag
            ("k7/8/2q5/3p4/8/8/6K1/8 w - d6 0 1", false),  // no pawns diag
            ("k7/8/8/1q1pP1K1/8/8/8/8 w - d6 0 1", false), // one pawn horiz
            ("k7/8/8/1q1p2K1/8/8/8/8 w - d6 0 1", false),  // no pawns horiz
        ];

        for fen in FENS {
            if let Err(err) = engine.load_fen(fen.0) {
                eprintln!("{err}");
                return;
            }
            print_board(&engine.board);

            let start = std::time::Instant::now();
            let nodes = perft(&mut engine, 1);
            let elapsed = start.elapsed().as_millis();
            println!("\r\ndepth: 1  positions: {nodes}  time: {elapsed}ms");
            assert_eq!(engine.en_pass_pin_mask == u64::MAX, fen.1);
        }
    }

    #[test]
    fn generation_test() {
        let mut engine = Engine::new();

        if let Err(err) = engine.load_fen(STARTING_FEN) {
            eprintln!("{err}");
            return;
        }

        print_board(&engine.board);

        // with STARTING_FEN
        const NODES: [usize; 10] = [
            1,
            20,
            400,
            8902,
            197281,
            4865609,
            119060324,
            3195901860,
            84998978956,
            2439530234167,
        ];

        for depth in 0..10 {
            let start = std::time::Instant::now();
            let nodes = perft(&mut engine, depth);
            let elapsed = start.elapsed().as_millis();
            println!("depth: {depth}  positions: {nodes}  time: {elapsed}ms");
            assert_eq!(nodes, NODES[depth as usize]);
        }
    }

    #[test]
    fn best_move_test() {
        let mut engine = Engine::new();

        if let Err(err) =
            engine.load_fen("2rk2r1/p2p1p1p/1p2p3/6p1/6P1/6K1/4q3/8 b - - 0 1")
        {
            eprintln!("{err}");
            return;
        }

        print_board(&engine.board);

        let mov = engine.best_move(2).unwrap();

        println!(
            "{}{}",
            square::to_string(mov.from),
            square::to_string(mov.to),
        );
    }
}
