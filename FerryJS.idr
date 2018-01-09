import Record

%default total

%inline
export
jscall : (fname : String) -> (ty : Type) ->
          {auto fty : FTy FFI_JS [] ty} -> ty
jscall fname ty = foreign FFI_JS fname ty

public export
data FromJS : Type -> Type where
  FromJSFun : (Ptr -> t) -> FromJS t

export
%hint
fromJSToString : FromJS String
fromJSToString = FromJSFun (believe_me)

export
%hint
fromJSToInt : FromJS Int
fromJSToInt = FromJSFun believe_me

export
fromJSToDouble : FromJS Double
fromJSToDouble = FromJSFun believe_me

export
fromJSToBool : FromJS Bool
fromJSToBool = FromJSFun (\ptr => (believe_me {b=Int} ptr) >= 1)

export
indexArray : Ptr -> Nat -> Ptr
indexArray ptr idx = unsafePerformIO $ jscall "(%0)[%1]" (Ptr -> Int -> JS_IO Ptr) ptr (cast idx)

export
length : Ptr -> Nat
length ptr = cast $ unsafePerformIO $ jscall "%0.length" (Ptr -> JS_IO Int) ptr

export
%hint
fromJSToList : FromJS a -> FromJS (List a)
fromJSToList (FromJSFun f) = FromJSFun (\ptr => convert ptr Z (length ptr))
                where convert : Ptr -> Nat -> Nat -> List a
                      convert ptr idx length =
                        if idx == length then []
                        else
                          (let val = f (indexArray ptr idx)
                          in val :: assert_total (convert ptr (S idx) length))

export
%hint
fromJSRecNil : FromJS (Record [])
fromJSRecNil = FromJSFun (const RecNil)

accessObject : Ptr -> String -> Ptr
accessObject ptr str =
  let io = jscall "(%0)[%1]" (Ptr -> String -> JS_IO Ptr) ptr str
  in unsafePerformIO io

export
%hint
fromJSRecord : FromJS t -> FromJS (Record xs) -> FromJS (Record ((k, t)::xs))
fromJSRecord {k} (FromJSFun ft) (FromJSFun fxs) = FromJSFun (\ptr =>
              let rec = fxs ptr
              in let fieldPtr = accessObject ptr k
              in RecCons k (ft fieldPtr) rec)

export
fromJS : {auto fjs: FromJS to} -> Ptr -> to
fromJS {fjs=FromJSFun f} ptr = f ptr

public export
data ToJS : Type -> Type where
  ToJSFun : (t -> Ptr) -> ToJS t

%hint
export
fromIntToJS : ToJS Int
fromIntToJS = ToJSFun believe_me

%hint
export
fromStringToJS : ToJS String
fromStringToJS = ToJSFun believe_me

%hint
export
fromDoubleToJS : ToJS Double
fromDoubleToJS = ToJSFun believe_me

%hint
export
fromBoolToJS : ToJS Bool
fromBoolToJS = ToJSFun convert
  where convert : Bool -> Ptr
        convert True = unsafePerformIO $ jscall "true" (JS_IO Ptr)
        convert False = unsafePerformIO $ jscall "false" (JS_IO Ptr)

empty_ : JS_IO Ptr
empty_ = jscall "new Array()" (JS_IO Ptr)

push : Ptr -> Ptr -> JS_IO ()
push = jscall "%0.push(%1)" (Ptr -> Ptr -> JS_IO ())

-- This functions has no side effects since
-- the array is created here and not referenced
-- from othe parts of the code. Allocating memory
-- is not considered a side effect. For these reasons
-- we may use unsafePerformIO.
%hint
export
fromListToJS : ToJS a -> ToJS (List a)
fromListToJS (ToJSFun f) =
  ToJSFun (unsafePerformIO . foldl append empty_)
    where append : JS_IO Ptr -> a -> JS_IO Ptr
          append io val = do
            arr <- io
            push arr (f val)
            pure arr

%hint
export
fromRecNilToJS : ToJS (Record [])
fromRecNilToJS= ToJSFun (\RecNil => unsafePerformIO $ jscall "{}" (JS_IO Ptr))

setObjectField : Ptr -> String -> Ptr -> JS_IO ()
setObjectField = jscall "(%0)[%1] = %2" (Ptr -> String -> Ptr -> JS_IO ()) 

-- See comment above the 'ToJS (List a)' implementation
%hint
export
fromRecordToJS : ToJS t -> ToJS (Record xs) -> ToJS (Record ((k, t)::xs))
fromRecordToJS (ToJSFun ft) (ToJSFun fxs) = 
  ToJSFun (\(RecCons key val rest) => let obj = fxs rest
                                      in unsafePerformIO $ do
                                        setObjectField obj key (ft val)
                                        pure obj)
export
toJS : {auto tjs: ToJS t} -> t -> Ptr
toJS {tjs=ToJSFun f} val = f val



