pub fn set_bit(n: u64, index: u8) -> u64 {
    n | (1 << index)
}

pub fn clear_bit(n: u64, index: u8) -> u64 {
    n & !(1 << index)
}

pub fn test_bit(n: u64, index: u8) -> bool {
    n & (1 << index) != 0
}

/// Returns only the specified bit, at index 0
pub fn get_bit(n: u64, index: u8) -> u64 {
    (n & (1 << index)) >> index
}

/// Returns only the specified bit, at its original index
pub fn isolate_bit(n: u64, index: u8) -> u64 {
    n & (1 << index)
}

/// Clears the lowest significant bit
pub fn clear_lsb(n: u64) -> u64 {
    n & n.wrapping_sub(1)
}

mod tests {
    use crate::bitboard::print_bitboard;

    use super::*;

    #[test]
    fn clear_lsb_test() {
        assert_eq!(clear_lsb(0), 0);
        assert_eq!(clear_lsb(0b110), 0b100);
        assert_eq!(clear_lsb(0b1001010111010110), 0b1001010111010100);
    }

    #[test]
    fn rotate_test() {
        let board = set_bit(0, 19);
        let color = 1;


        print_bitboard(board);

        print_bitboard(board >> 8);
        print_bitboard(board.rotate_left(8 + color * 48));
    }
}
