import FerryJS
import FerryJS.Util

import Record

array : JS_IO Ptr
array = jscall "[1, 2, 3]" (JS_IO Ptr)

object : JS_IO Ptr
object = jscall "{a: 10, b: \"abc\"}" (JS_IO Ptr)

schema : Schema
schema = [("a", Int), ("b", String)]

nestedArray : JS_IO Ptr
nestedArray = jscall "[1, [3, 5]]" (JS_IO Ptr)

main : JS_IO ()
main = do
  arr <- array 
  (let lst = toIdrisUnsafe {to=List Int} arr
   in do
     printLn' lst
     log (toJS lst)) 

  obj <- object
  (let rec = toIdrisUnsafe {to=Record schema} obj
   in do
     printLn' (showRecord rec)
     log (toJS rec))

  nestedArr <- nestedArray
  (let tup = toIdrisUnsafe {to=(Int, Int, Int)} nestedArr
   in do
     printLn' tup
     log (toJS tup))
