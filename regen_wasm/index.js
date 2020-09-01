// Import our outputted wasm ES6 module
// Which, export default's, an initialization function
import * as regen from "./pkg/regen_wasm.js";

const runWasm = async () => {
  // Instantiate our wasm module
  await regen.default("./pkg/regen_wasm_bg.wasm");

  // Call the Add function export from wasm, save the result
  const interpretResult = regen.interpret(`
    (def factorial (fun (n) (
      (if (< n 2)
        (1)
        ((* n 
          (factorial (- n 1))))
      )
    )))
    
    (def factorial_loop (fun (n) (
      (let fact 1)
      (let i 2)
      (let limit (+ n 1))
      (block (
        (if (>= i limit)
          (break))
        (set fact (* fact i))
        (set i (+ i 1))
        repeat
      ))
      fact
    )))
    
    (def main (fun () (
      (debug (factorial 3))
      (debug (factorial_loop 3))
    )))
  `);

  console.log(interpretResult);

};
runWasm();