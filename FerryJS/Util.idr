module FerryJS.Util

import FerryJS

||| Call JavaScript `console.log()` with the given value
||| as argument. Useful for debugging. JavaScript (Ptr) values
||| will be printed correctly. In the case of native Idris types
||| the runtime representation will be printed, which might
||| be useless.
export
log : a -> JS_IO ()
log = jscall "console.log(%0)" (Ptr -> JS_IO ()) . believe_me 

||| Call `process.exit()` with the given exit code.
export
exit : Nat -> JS_IO ()
exit code = jscall "process.exit(%0)" (Int -> JS_IO ()) (cast code)
