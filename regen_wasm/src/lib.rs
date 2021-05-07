// The wasm-pack uses wasm-bindgen to build and generate JavaScript binding file.
// Import the wasm-bindgen crate.
use wasm_bindgen::prelude::*;

use regen_core::hotload;

// Our Add function
// wasm-pack requires "exported" functions
// to include #[wasm_bindgen]
#[wasm_bindgen]
pub fn interpret(code : String) -> String {
  let env = regen_core::new_env();
  hotload::hotload_module(env, "wasm_test_module", &code);
  format!("Regen launched")
}
