import Record

%default total

%inline
export
jscall : (fname : String) -> (ty : Type) ->
          {auto fty : FTy FFI_JS [] ty} -> ty
jscall fname ty = foreign FFI_JS fname ty

public export
interface FromJS to where
  fromJS : Ptr -> to

export
implementation FromJS String where
  fromJS = believe_me

export
implementation FromJS Int where
  fromJS = (cast {from=Double} {to=Int}) . believe_me

export
implementation FromJS Double where
  fromJS = believe_me

export
implementation FromJS Bool where
  fromJS ptr = (believe_me {b=Int} ptr) >= 1

export
indexArray : Ptr -> Nat -> Ptr
indexArray ptr idx = unsafePerformIO $ jscall "(%0)[%1]" (Ptr -> Int -> JS_IO Ptr) ptr (cast idx)

export
length : Ptr -> Nat
length ptr = cast $ unsafePerformIO $ jscall "%0.length" (Ptr -> JS_IO Int) ptr

export
FromJS a => FromJS (List a) where
  fromJS ptr =  convert ptr Z (length ptr)
    where convert : FromJS a => Ptr -> Nat -> Nat -> List a
          convert ptr idx length =
            if idx == length then []
            else
              (let val = (fromJS {to=a} (indexArray ptr idx))
               in val :: assert_total (convert ptr (S idx) length))

export
FromJS (Record []) where
  fromJS _ = RecNil

accessObject : Ptr -> String -> Ptr
accessObject ptr str =
  let io = jscall "(%0)[%1]" (Ptr -> String -> JS_IO Ptr) ptr str
  in unsafePerformIO io

export
(FromJS (Record xs), FromJS t) => FromJS (Record ((k, t)::xs)) where
  fromJS {k} {t} {xs} ptr =
    let rec = fromJS {to=Record xs} ptr
    in let fieldPtr = accessObject ptr k
    in RecCons k (fromJS {to=t} fieldPtr) rec

public export
interface ToJS from where
  toJS : from -> Ptr

export
implementation ToJS String where
  toJS = believe_me

export
implementation ToJS Int where
  toJS = believe_me

export
implementation ToJS Double where
  toJS = believe_me

export
implementation ToJS Bool where
  toJS True = unsafePerformIO $ jscall "true" (JS_IO Ptr)
  toJS False = unsafePerformIO $ jscall "false" (JS_IO Ptr)

empty_ : JS_IO Ptr
empty_ = jscall "new Array()" (JS_IO Ptr)

push : Ptr -> Ptr -> JS_IO ()
push = jscall "%0.push(%1)" (Ptr -> Ptr -> JS_IO ())

-- This functions has no side effects since
-- the array is created here and not referenced
-- from othe parts of the code. Allocating memory
-- is not considered a side effect. For these reasons
-- we may use unsafePerformIO.
export
ToJS a => ToJS (List a) where
  toJS {a} lst = (unsafePerformIO $ foldl append empty_ lst)
                  where append : ToJS a => JS_IO Ptr -> a -> JS_IO Ptr
                        append io val = do
                          arr <- io
                          push arr (toJS val)
                          pure arr

export
implementation ToJS (Record []) where
  toJS RecNil = unsafePerformIO $ jscall "{}" (JS_IO Ptr)

setObjectField : Ptr -> String -> Ptr -> JS_IO ()
setObjectField = jscall "(%0)[%1] = %2" (Ptr -> String -> Ptr -> JS_IO ()) 

-- See comment above the 'ToJS (List a)' implementation
export
(ToJS (Record xs), ToJS t) => ToJS (Record ((k, t)::xs)) where
  toJS (RecCons key val rest) = let obj = toJS rest
                                in unsafePerformIO $ do
                                  setObjectField obj key (toJS val)
                                  pure obj




