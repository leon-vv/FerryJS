module FerryJS.Util

import FerryJS

export
log : a -> JS_IO ()
log = jscall "console.log(%0)" (Ptr -> JS_IO ()) . believe_me 
