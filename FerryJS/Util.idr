module FerryJS.Util

import FerryJS

export
log : a -> JS_IO ()
log = jscall "console.log(%0)" (Ptr -> JS_IO ()) . believe_me 

export
exit : Nat -> JS_IO ()
exit code = jscall "process.exit(%0)" (Int -> JS_IO ()) (cast code)
