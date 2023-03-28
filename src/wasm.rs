use std::mem::transmute;

use wasm_bindgen::prelude::*;

use crate::engine::Engine;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    pub fn log(s: &str);

    type Date;
    #[wasm_bindgen(static_method_of = Date)]
    pub fn now() -> f64;

}

#[wasm_bindgen]
pub struct ChessEngine {
    engine: Engine,
}

#[wasm_bindgen]
impl ChessEngine {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self {
            engine: Engine::new(),
        }
    }

    #[wasm_bindgen]
    pub fn reset(&mut self) {
        self.engine.reset();
    }

    #[wasm_bindgen]
    pub fn load_fen(&mut self, fen: String) -> Result<(), String> {
        self.reset();
        self.engine.load_fen(&fen)
    }

    #[wasm_bindgen]
    pub fn get_active_color(&self) -> usize {
        return self.engine.board.active_color;
    }

    #[wasm_bindgen]
    pub fn is_in_check(&self) -> bool {
        return self.engine.checks > 0;
    }

    #[wasm_bindgen]
    pub fn get_legal_moves(&mut self) -> Box<[u32]> {
        let engine_moves = self.engine.generate_moves();
        let engine_moves: Vec<u32> = unsafe { transmute(engine_moves) };
        return engine_moves.into_boxed_slice();
    }

    #[wasm_bindgen]
    pub fn get_best_move(&mut self, depth: usize) -> Option<u32> {
        return self
            .engine
            .best_move(depth as u16)
            .map(|m| unsafe { transmute(m) });
    }

    #[wasm_bindgen]
    pub fn make_move(&mut self, legal_move: u32) {
        let legal_move = unsafe { transmute(legal_move) };
        self.engine.make_move(&legal_move);
    }
}

const STARTING_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

fn perft(engine: &mut Engine, depth: u8) -> usize {
    if depth == 0 {
        return 1;
    }

    let moves = engine.generate_moves();
    let mut positions = 0;

    for mov in moves {
        engine.make_move(&mov);
        positions += perft(engine, depth - 1);
        engine.unmake_move(&mov);
    }

    return positions;
}

#[wasm_bindgen]
pub fn generation_test() {
    let mut engine = Engine::new();

    if let Err(err) = engine.load_fen(STARTING_FEN) {
        log(&err);
        return;
    }

    // with STARTING_FEN
    const NODES: [usize; 8] = [1, 20, 400, 8902, 197281, 4865609, 119060324, 3195901860];

    for depth in 0..7 {
        let start = Date::now();
        let nodes = perft(&mut engine, depth);
        let elapsed = Date::now() - start;
        log(&format!(
            "depth: {depth}  positions: {nodes}  time: {elapsed}ms \r\n"
        ));
    }
}
