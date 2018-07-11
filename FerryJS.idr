{- This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/. -}

module FerryJS

import Record

import Debug.Error

-- To use the error function
%language ElabReflection

%default total

||| Small wrapper around the `foreign` function.
||| Inspired by the IdrisScript package.
%inline
export
jscall : (fname : String) -> (ty : Type) ->
          {auto fty : FTy FFI_JS [] ty} -> ty
jscall fname ty = foreign FFI_JS fname ty

||| Use `JSON.stringify()` to convert the JavaScript value to a string.
||| Can be useful for debugging.
export
stringify : Ptr -> String
stringify = unsafePerformIO . jscall "JSON.stringify(%0)" (Ptr -> JS_IO String)

-- Cannot be declared as ToIdris a = (Ptr -> Maybe a)
-- because a chance exists that, when using auto, Idris
-- builds a 'random' function of that type.
||| A proof that a given Idris type can be converted
||| from a certain JavaScript value. If the JavaScript
||| value is not of the expected form the conversion 
||| function should return `Nothing`.
public export
data ToIdris : Type -> Type where
  ToIdrisFn : (Ptr -> Maybe t) -> ToIdris t

-- Unsafe
toBool : Ptr -> Bool
toBool ptr = believe_me {b=Int} ptr >= 1

%inline
check : String -> Ptr -> Bool
check c ptr = unsafePerformIO $ map toBool $ jscall c (Ptr -> JS_IO Ptr) ptr

%inline
withCheck : String -> (Ptr -> t) -> ToIdris t
withCheck c f = ToIdrisFn (\ptr => if check c ptr then Just (f ptr)
                                                  else Nothing)
                    
%inline
withTypeOfCheck : String -> (Ptr -> t) -> ToIdris t
withTypeOfCheck type = withCheck ("typeof (%0) == \"" ++ type ++ "\"")

%hint
export
toIdrisPtr : ToIdris Ptr
toIdrisPtr = ToIdrisFn (Just . id)

%hint
export
toIdrisString : ToIdris String
toIdrisString = withTypeOfCheck "string" believe_me

%hint
export
toIdrisInt : ToIdris Int
toIdrisInt = withTypeOfCheck "number" believe_me

%hint
export
toIdrisNat : ToIdris Nat
toIdrisNat = ToIdrisFn (\ptr => 
  let (ToIdrisFn toInt) = toIdrisInt
  in do
    i <- toInt ptr
    (if i >= 0 then Just . cast $ i
               else Nothing))

%hint
export
toIdrisDouble : ToIdris Double
toIdrisDouble = withTypeOfCheck "number" believe_me

%hint
export
toIdrisBool : ToIdris Bool
toIdrisBool = withTypeOfCheck "boolean" toBool 

%hint
export
toIdrisMaybe : ToIdris a -> ToIdris (Maybe a)
toIdrisMaybe (ToIdrisFn f) =  ToIdrisFn (\ptr =>
  if check "(%0) === null || (%0) === undefined" ptr then Just Nothing
                                                 else Just (f ptr))

indexArray : Ptr -> Nat -> Ptr
indexArray ptr idx = unsafePerformIO $ jscall "(%0)[%1]" (Ptr -> Int -> JS_IO Ptr) ptr (cast idx)

-- Accessing length only throws an error if the value is null or undefined
length : Ptr -> Maybe Nat
length ptr = 
  let (ToIdrisFn toNat) = toIdrisNat
  in if check "(%0) == undefined || (%0) == null" ptr
        then Nothing
        else unsafePerformIO . map toNat . jscall "(%0).length" (Ptr -> JS_IO Ptr) $ ptr

%hint
export
toIdrisList : ToIdris a -> ToIdris (List a)
toIdrisList (ToIdrisFn f) = ToIdrisFn (\ptr => (length ptr) >>= (convert ptr Z))
                where convert : Ptr -> Nat -> Nat -> Maybe (List a)
                      convert ptr idx length =
                        if idx == length then Just []
                        else (::) <$> f (indexArray ptr idx)
                                  <*> assert_total (convert ptr (S idx) length)

%hint
export
toIdrisTuple : ToIdris a -> ToIdris b -> ToIdris (a, b)
toIdrisTuple (ToIdrisFn f1) (ToIdrisFn f2) =
  ToIdrisFn (\ptr =>
    if length ptr == Just 2
      then case (f1 (indexArray ptr 0), f2 (indexArray ptr 1)) of
              (Just val1, Just val2) => Just (val1, val2)
              _ => Nothing
      else Nothing)


isObjectCheck : String
isObjectCheck = "typeof (%0) == \"object\" && (%0) != null"

%hint
export
toIdrisRecNil : ToIdris (Record [])
toIdrisRecNil = withCheck isObjectCheck (const RecNil) 

accessObject : Ptr -> String -> Ptr
accessObject p = unsafePerformIO . jscall "(%0)[%1]" (Ptr -> String -> JS_IO Ptr) p

export
%hint
toIdrisRecord : ToIdris t -> ToIdris (Record xs) -> ToIdris (Record ((k, t)::xs))
toIdrisRecord {k} (ToIdrisFn ft) (ToIdrisFn fxs) = ToIdrisFn (\ptr =>
              let rec = fxs ptr
              in let fieldPtr = accessObject ptr k
              in RecCons k <$> (ft fieldPtr) <*> rec)

||| If the compiler can prove that the requested
||| Idris type can be converted from a JavaScript value,
||| try the conversion.
export
toIdris : {auto ti: ToIdris to} -> Ptr -> Maybe to
toIdris {ti=ToIdrisFn f} ptr = f ptr

||| Throw a runtime error if the conversion fails.
export
partial
toIdrisUnsafe : {auto ti: ToIdris to} -> Ptr -> to
toIdrisUnsafe {ti} ptr = case toIdris {ti=ti} ptr of
                          Just to => to
                          Nothing => error "Failed to convert JavaScript value to Idris"

-- See comment above 'ToIdris'
||| A proof that values of an Idris type can be converted to JavaScript.
public export
data ToJS : Type -> Type where
  ToJSFn : (t -> Ptr) -> ToJS t

%hint
export
toJSPtr : ToJS Ptr
toJSPtr = ToJSFn id

%hint
export
toJSInt : ToJS Int
toJSInt = ToJSFn believe_me

%hint
export
toJSString : ToJS String
toJSString = ToJSFn believe_me

%hint
export
toJSDouble : ToJS Double
toJSDouble = ToJSFn believe_me

%inline
pureJSValue : String -> Ptr
pureJSValue expr = unsafePerformIO $ jscall expr (JS_IO Ptr)

%hint
export
toJSBool : ToJS Bool
toJSBool = ToJSFn (\b =>
                if b
                  then pureJSValue "true"
                  else pureJSValue "false")

%hint
export
toJSMaybe : ToJS a -> ToJS (Maybe a)
toJSMaybe (ToJSFn f) = ToJSFn (\m => case m of
              Nothing => pureJSValue "null"
              Just x => f x)

empty_ : JS_IO Ptr
empty_ = jscall "new Array()" (JS_IO Ptr)

push : Ptr -> Ptr -> JS_IO ()
push = jscall "(%0).push(%1)" (Ptr -> Ptr -> JS_IO ())

-- This functions has no side effects since
-- the array is created here and not referenced
-- from othe parts of the code. Allocating memory
-- is not considered a side effect. For these reasons
-- we may use unsafePerformIO.
%hint
export
toJSList : ToJS a -> ToJS (List a)
toJSList (ToJSFn f) =
  ToJSFn (unsafePerformIO . foldl append empty_)
    where append : JS_IO Ptr -> a -> JS_IO Ptr
          append io val = do
            arr <- io
            push arr (f val)
            pure arr

%hint
export
toJSTuple : ToJS a -> ToJS b -> ToJS (a, b)
toJSTuple (ToJSFn f1) (ToJSFn f2) =
  ToJSFn (\(a, b) =>
    unsafePerformIO $ do
      arr <- empty_
      push arr (f1 a)
      push arr (f2 b)
      pure arr)


%hint
export
toJSRecNil : ToJS (Record [])
toJSRecNil = ToJSFn (\RecNil => pureJSValue "{}")

setObjectField : Ptr -> String -> Ptr -> JS_IO ()
setObjectField = jscall "(%0)[%1] = %2" (Ptr -> String -> Ptr -> JS_IO ()) 

-- See comment above the 'ToJS (List a)' implementation
%hint
export
toJSRecord : ToJS t -> ToJS (Record xs) -> ToJS (Record ((k, t)::xs))
toJSRecord (ToJSFn ft) (ToJSFn fxs) = 
  ToJSFn (\(RecCons key val rest) => let obj = fxs rest
                                      in unsafePerformIO $ do
                                        setObjectField obj key (ft val)
                                        pure obj)
export
toJS : {auto tjs: ToJS t} -> t -> Ptr
toJS {tjs=ToJSFn f} val = f val



