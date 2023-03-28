use crate::{
    bitboard::*,
    bits::{clear_bit, clear_lsb, set_bit},
    board::{Board, BISHOP, BLACK, ROOK, WHITE},
    moves::Move,
    rand,
};

pub struct Rays(pub [[u64; 64]; 8]);

impl Rays {
    pub fn new() -> Self {
        let mut north = [0; 64];
        let mut south = [0; 64];

        let mut east = [0; 64];
        let mut west = [0; 64];

        let mut north_east = [0; 64];
        let mut south_west = [0; 64];

        let mut north_west = [0; 64];
        let mut south_east = [0; 64];

        let n = clear_bit(FILE_H, 0);
        let s = clear_bit(FILE_A, 63);

        let e = clear_bit(RANK_8, 63);
        let w = clear_bit(RANK_1, 0);

        let ne = clear_bit(A1_H8, 7);
        let sw = clear_bit(A1_H8, 56);

        let nw = clear_bit(A8_H1, 0);
        let se = clear_bit(A8_H1, 63);

        for i in 0..64 {
            let rank_mul_8 = (i / 8) * 8;
            let file = i % 8;

            // row-wise overflow works out fine here
            north[i] = n << i;
            south[i] = s >> (63 - i);

            // row-wise masking
            west[i] = (w << i) & (RANK_1 << rank_mul_8);
            east[i] = (e >> (63 - i)) & (RANK_1 << rank_mul_8);

            // zig-zag
            north_east[i] = shift_east_n(ne, 7 - file) << rank_mul_8;
            south_west[i] = shift_west_n(sw, file) >> (56 - rank_mul_8);

            north_west[i] = shift_west_n(nw, file) << rank_mul_8;
            south_east[i] = shift_east_n(se, 7 - file) >> (56 - rank_mul_8);
        }

        // these are in this order because:
        // 1. first four are cardinal, last four are diagonal
        // 2. in each set of four, first two use trailing_zeros, and last two use leading_zeros,
        //    to find piece closest to given square
        Self([
            north, west, south, east, north_east, north_west, south_west, south_east,
        ])
    }
}

pub struct BlockerMasks(pub [[u64; 64]; 2]);

impl BlockerMasks {
    pub fn new(rays: &Rays) -> Self {
        let mut rook = [0; 64];
        let mut bishop = [0; 64];
        let masks = [
            !RANK_8,
            !FILE_A,
            !RANK_1,
            !FILE_H,
            !(RANK_8 | FILE_H),
            !(RANK_8 | FILE_A),
            !(RANK_1 | FILE_A),
            !(RANK_1 | FILE_H),
        ];

        for i in 0..64 {
            rook[i] = (rays.0[0][i] & masks[0])
                | (rays.0[1][i] & masks[1])
                | (rays.0[2][i] & masks[2])
                | (rays.0[3][i] & masks[3]);
            bishop[i] = (rays.0[4][i] & masks[4])
                | (rays.0[5][i] & masks[5])
                | (rays.0[6][i] & masks[6])
                | (rays.0[7][i] & masks[7]);
        }

        Self([rook, bishop])
    }
}

pub struct AttackSets {
    pub rook: Vec<u64>,
    pub rook_offset: [usize; 64],
    pub bishop: Vec<u64>,
    pub bishop_offset: [usize; 64],
    pub knight: [u64; 64],
    pub king: [u64; 64],
    pub pawn: [[u64; 64]; 2],
}

impl AttackSets {
    pub fn new(rays: &Rays, blocker_masks: &BlockerMasks) -> Self {
        let (rook, rook_offset) = Self::rook(rays, blocker_masks);
        let (bishop, bishop_offset) = Self::bishop(rays, blocker_masks);
        let knight = Self::knight();
        let king = Self::king();
        let pawn = Self::pawn();

        Self {
            rook,
            rook_offset,
            bishop,
            bishop_offset,
            knight,
            king,
            pawn,
        }
    }

    pub fn get_rook_attacks(&self, square: usize, blockers: u64) -> u64 {
        let key = self.rook_offset[square]
            + (blockers.wrapping_mul(ROOK_MAGIC[square]) >> ROOK_MAGIC_SHIFT[square]) as usize;
        self.rook[key]
    }

    pub fn get_bishop_attacks(&self, square: usize, blockers: u64) -> u64 {
        let key = self.bishop_offset[square]
            + (blockers.wrapping_mul(BISHOP_MAGIC[square]) >> BISHOP_MAGIC_SHIFT[square]) as usize;
        self.bishop[key]
    }

    fn rook(rays: &Rays, blocker_masks: &BlockerMasks) -> (Vec<u64>, [usize; 64]) {
        let mut table = vec![0; 96_256];
        let mut offsets = [0; 64];
        let mut offset = 0;

        for square in 0..64 {
            offsets[square] = offset;
            let blocker_mask = blocker_masks.0[ROOK][square];

            let mut blockers: u64 = 0;
            loop {
                let key = (blockers.wrapping_mul(ROOK_MAGIC[square]) >> ROOK_MAGIC_SHIFT[square])
                    as usize;
                let attacks = rook_attack(square as u64, blockers, rays);
                table[offsets[square] + key] = attacks;

                blockers = blockers.wrapping_sub(blocker_mask) & blocker_mask;
                if blockers == 0 {
                    break;
                }
            }

            offset += 1 << (64 - ROOK_MAGIC_SHIFT[square]);
        }

        return (table, offsets);
    }

    fn bishop(rays: &Rays, blocker_masks: &BlockerMasks) -> (Vec<u64>, [usize; 64]) {
        let mut table = vec![0; 5_248];
        let mut offsets = [0; 64];
        let mut offset = 0;

        for square in 0..64 {
            offsets[square] = offset;
            let blocker_mask = blocker_masks.0[BISHOP][square];

            let mut blockers: u64 = 0;
            loop {
                let key = (blockers.wrapping_mul(BISHOP_MAGIC[square])
                    >> BISHOP_MAGIC_SHIFT[square]) as usize;
                let attacks = bishop_attack(square as u64, blockers, rays);
                table[offsets[square] + key] = attacks;

                blockers = blockers.wrapping_sub(blocker_mask) & blocker_mask;
                if blockers == 0 {
                    break;
                }
            }

            offset += 1 << (64 - BISHOP_MAGIC_SHIFT[square]);
        }

        return (table, offsets);
    }

    fn knight() -> [u64; 64] {
        let mut table = [0; 64];
        for square in 0..64 {
            table[square] = knight_attack(square)
        }

        return table;
    }

    fn king() -> [u64; 64] {
        let mut table = [0; 64];
        for square in 0..64 {
            table[square] = king_attack(square)
        }

        return table;
    }

    fn pawn() -> [[u64; 64]; 2] {
        let mut table = [[0; 64]; 2];
        for color in 0..2 {
            for square in 0..64 {
                table[color][square] = pawn_attack(square, color);
            }
        }

        return table;
    }
}

pub fn knight_attack(square: usize) -> u64 {
    let b = set_bit(0, square as u8);

    return (((b >> 6) | (b << 10)) & !(FILE_G | FILE_H))
        | (((b >> 10) | (b << 6)) & !(FILE_A | FILE_B))
        | (((b >> 15) | (b << 17)) & !FILE_H)
        | (((b >> 17) | (b << 15)) & !FILE_A);
}

pub fn king_attack(square: usize) -> u64 {
    let b = set_bit(0, square as u8);

    return ((b << 9 | b << 1 | b >> 7) & !FILE_H)
        | ((b << 7 | b >> 1 | b >> 9) & !FILE_A)
        | b << 8
        | b >> 8;
}

pub fn pawn_attack(square: usize, color: usize) -> u64 {
    let b = set_bit(0, square as u8);

    if color == WHITE {
        shift_north_west(b) | shift_north_east(b)
    } else {
        shift_south_west(b) | shift_south_east(b)
    }
}

pub fn rook_attack(square: u64, blockers: u64, rays: &Rays) -> u64 {
    let mut moves = 0;

    // this works by finding all blockers on the path of a single directional ray at a time
    // then finding the closest blocker to the rook - the valid moves are then:
    // `directional_ray AND NOT(another_same_direction_ray_from_single_blocker)`

    // north and west rays (hence trailing zeros check)
    for i in 0..2 {
        let mut ray_moves = rays.0[i][square as usize];
        let ray_blockers = ray_moves & blockers;

        if ray_blockers > 0 {
            let blocker_square = ray_blockers.trailing_zeros();
            let blocker_ray = rays.0[i][blocker_square as usize];
            ray_moves &= !blocker_ray;
        }

        moves |= ray_moves;
    }

    // south and east rays (hence leading zeros check)
    for i in 2..4 {
        let mut ray_moves = rays.0[i][square as usize];
        let ray_blockers = ray_moves & blockers;

        if ray_blockers > 0 {
            let blocker_square = 63 - ray_blockers.leading_zeros();
            let blocker_ray = rays.0[i][blocker_square as usize];
            ray_moves &= !blocker_ray;
        }

        moves |= ray_moves;
    }

    return moves;
}

pub fn bishop_attack(square: u64, blockers: u64, rays: &Rays) -> u64 {
    let mut moves = 0;

    // this works by finding all blockers on the path of a single directional ray at a time
    // then finding the closest blocker to the rook - the valid moves are then:
    // `directional_ray AND NOT(another_same_direction_ray_from_single_blocker)`

    // north-east and north-west rays (hence trailing zeros check)
    for i in 4..6 {
        let mut ray_moves = rays.0[i][square as usize];
        let ray_blockers = ray_moves & blockers;

        if ray_blockers > 0 {
            let blocker_square = ray_blockers.trailing_zeros();
            let blocker_ray = rays.0[i][blocker_square as usize];
            ray_moves &= !blocker_ray;
        }

        moves |= ray_moves;
    }

    // south-west and south-east (hence leading zeros check)
    for i in 6..8 {
        let mut ray_moves = rays.0[i][square as usize];
        let ray_blockers = ray_moves & blockers;

        if ray_blockers > 0 {
            let blocker_square = 63 - ray_blockers.leading_zeros();
            let blocker_ray = rays.0[i][blocker_square as usize];
            ray_moves &= !blocker_ray;
        }

        moves |= ray_moves;
    }

    return moves;
}

#[derive(Default)]
pub struct TableEntry {
    zobrist_key: u64,
    depth: u16,
    eval: i32,
    node_type: u8,
    best_move: Option<Move>,
}

pub struct TranspositionTable<const N: usize> {
    transpositions: [TableEntry; N],

    pieces: [[[u64; 64]; 6]; 2],
    en_passants: [u64; 64],
    castling_rights: [u64; 16],
    colors: [u64; 2],
}

impl<const N: usize> TranspositionTable<N> {
    pub fn new() -> Self {
        let mut generator = rand::XORShift64::new(0x007FFCDDFCED714A);
        let pieces = core::array::from_fn::<_, 2, _>(|_| {
            core::array::from_fn::<_, 6, _>(|_| {
                core::array::from_fn::<_, 64, _>(|_| generator.rand())
            })
        });
        let en_passants = core::array::from_fn(|_| generator.rand());
        let castling_rights = core::array::from_fn(|_| generator.rand());
        let colors = core::array::from_fn(|_| generator.rand());

        Self {
            transpositions: core::array::from_fn(|_| TableEntry::default()),
            pieces,
            en_passants,
            castling_rights,
            colors,
        }
    }

    pub fn zobrist_hash(&self, board: &Board) -> u64 {
        let mut h = 0;

        h ^= self.colors[board.active_color];

        if board.en_passant > 0 {
            let square = board.en_passant.trailing_zeros() as usize;
            h ^= self.en_passants[square];
        }

        for color in 0..2 {
            for piece in 0..6 {
                let mut piece_bb = board.pieces[color][piece];
                while piece_bb > 0 {
                    let square = piece_bb.trailing_zeros() as usize;
                    h ^= self.pieces[color][piece][square];
                    piece_bb = clear_lsb(piece_bb);
                }
            }
        }

        let castling_bits = (board.castling_rights[WHITE][0] as usize) << 3
            | (board.castling_rights[WHITE][1] as usize) << 2
            | (board.castling_rights[BLACK][0] as usize) << 1
            | (board.castling_rights[BLACK][1] as usize);

        h ^= self.castling_rights[castling_bits];

        return h;
    }

    pub fn update_zobrist_hash(&self, mut hash: u64, mov: &Move) -> u64 {




        return hash;
    }
}

mod tests {

    use super::*;

    #[test]
    fn magic_rook_test() {
        let rays = Rays::new();
        let bm = BlockerMasks::new(&rays);
        let attack_sets = AttackSets::new(&rays, &bm);

        let rook_bb: u64 = 0x8000000000000000;
        let blockers_bb: u64 = 0xf7ff00000000fff7;
        let square = rook_bb.trailing_zeros() as usize;
        let blocker_mask = bm.0[ROOK][square];
        let masked_blockers = blocker_mask & blockers_bb;
        let magic_key =
            (masked_blockers.wrapping_mul(ROOK_MAGIC[square]) >> ROOK_MAGIC_SHIFT[square]) as usize;
        let attacks = attack_sets.rook[attack_sets.rook_offset[square] + magic_key];

        println!("ROOK: ");
        print_bitboard(rook_bb);

        println!("BLOCKERS: ");
        print_bitboard(blockers_bb);

        println!("BLOCKER MASK: ");
        print_bitboard(blocker_mask);

        println!("MASKED BLOCKERS: ");
        print_bitboard(masked_blockers);

        println!("KEY: ");
        println!("{}", magic_key);

        println!("ATTACKS: ");
        print_bitboard(attacks);
    }

    #[test]
    fn rays() {
        let rays = Rays::new();

        for r in rays.0[7] {
            print_bitboard(r);
        }
    }

    #[test]
    fn blockers_mask() {
        let rays = Rays::new();

        let bm = BlockerMasks::new(&rays);

        for mask in bm.0[ROOK] {
            print_bitboard(mask);
        }
    }

    #[test]
    fn rook_moves() {
        let rays = Rays::new();
        let bm = BlockerMasks::new(&rays);

        let rook_bb: u64 = 0x40000;
        let blockers_bb: u64 = 0x40000000400008;
        let square = rook_bb.trailing_zeros() as usize;
        let blocker_mask = bm.0[ROOK][square];
        let masked_blockers = blocker_mask & blockers_bb;

        print_bitboard(rook_attack(square as u64, masked_blockers, &rays));
    }

    #[test]
    fn bishop_moves() {
        let rays = Rays::new();
        let bm = BlockerMasks::new(&rays);

        let bishop_bb: u64 = 0x10000000;
        let blockers_bb: u64 = 0x200000008f000;
        let square = bishop_bb.trailing_zeros() as usize;
        let blocker_mask = bm.0[BISHOP][square];
        let masked_blockers = blocker_mask & blockers_bb;
        print_bitboard(masked_blockers);
        print_bitboard(bishop_attack(square as u64, masked_blockers, &rays));
    }

    #[test]
    fn bit_subset() {
        // https://www.chessprogramming.org/Traversing_Subsets_of_a_Set#All_Subsets_of_any_Set
        let rays = Rays::new();
        let bm = BlockerMasks::new(&rays);

        let set = 0x40400600000;

        let mut subset: u64 = 0;
        loop {
            print_bitboard(subset);
            subset = subset.wrapping_sub(set) & set;
            if subset == 0 {
                break;
            }
        }
    }
}
