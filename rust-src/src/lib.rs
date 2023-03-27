#![feature(iter_array_chunks)]
mod utils;

use js_sys::{Array, Uint8Array};
use std::convert::TryInto;
use sudoku_solver::{Board, SolutionIter};
use wasm_bindgen::prelude::*;

#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

fn board_to_vec(board: Board) -> Vec<u8> {
    (0..81)
        .into_iter()
        .map(|i| board.get_cell(i / 9, i % 9))
        .collect()
}

#[wasm_bindgen]
pub fn solve_sudoku(puzzle: Array) -> Array {
    let puzzle: Vec<_> = puzzle
        .to_vec()
        .into_iter()
        .map(|i| i.as_f64().unwrap() as u8)
        .array_chunks::<9>()
        .collect();
    let board = Board::from(&puzzle.try_into().unwrap());

    let mut f = |_, i, _| {
        let solutions = SolutionIter::new(&board);
        let arr = Array::new_with_length(81);
        for (j, cell) in solutions.take(10).map(board_to_vec).collect::<Vec<_>>()[i as usize]
            .clone()
            .into_iter()
            .enumerate()
        {
            arr.set(j as u32, JsValue::from_f64(cell as f64))
        }
        arr.into()
    };

    Array::new_with_length(10)
        .fill(&JsValue::from_f64(0.0), 0, 10)
        .map(&mut f)
}
