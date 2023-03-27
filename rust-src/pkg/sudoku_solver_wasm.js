import * as wasm from "./sudoku_solver_wasm_bg.wasm";
import { __wbg_set_wasm } from "./sudoku_solver_wasm_bg.js";
__wbg_set_wasm(wasm);
export * from "./sudoku_solver_wasm_bg.js";
