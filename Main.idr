import FerryJS
import Record

array : JS_IO Ptr
array = jscall "[1, 2, 3]" (JS_IO Ptr)

object : JS_IO Ptr
object = jscall "{a: 10, b: \"abc\"}" (JS_IO Ptr)

schema : Schema
schema = [("a", Int), ("b", String)]

log : Ptr -> JS_IO ()
log = jscall "console.log(%0)" (Ptr -> JS_IO ())

main : JS_IO ()
main = do
  arr <- array 
  (let lst = fromJSUnsafe {to=List Int} arr
   in do
     printLn' lst
     log (toJS lst))

  obj <- object
  (let rec = fromJSUnsafe {to=Record schema} obj
   in do
     printLn' (showRecord rec)
     log (toJS rec))
