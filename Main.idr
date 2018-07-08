import FerryJS
import FerryJS.Util

import Record

array : JS_IO Ptr
array = jscall "[1, 2, 3]" (JS_IO Ptr)

object : JS_IO Ptr
object = jscall "{id: 10}" (JS_IO Ptr)

schema : Schema
schema = [("id", Int)]

object2 : JS_IO Ptr
object2 = jscall "{id:'2', name:'some name'}" (JS_IO Ptr)

schema2 : Schema
schema2 = [("id", String), ("name", String), ("done", Maybe String)]

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
  
  obj2 <- object2
  (let rec2 = toIdrisUnsafe {to=Record schema2} obj2
   in do
     printLn' (showRecord rec2)
     log (toJS rec2))

  nestedArr <- nestedArray
  (let tup = toIdrisUnsafe {to=(Int, Int, Int)} nestedArr
   in do
     printLn' tup
     log (toJS tup))
