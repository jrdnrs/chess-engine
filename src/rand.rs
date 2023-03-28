// https://en.wikipedia.org/wiki/Xorshift
pub struct XORShift64 {
    state: u64,
}

impl XORShift64 {
    pub fn new(seed: u64) -> Self {
        Self { state: seed }
    }

    pub fn rand(&mut self) -> u64 {
        let mut x = self.state;
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        self.state = x;
        return x;
    }
}
