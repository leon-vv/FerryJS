import Record

%default total

%inline
export
jscall : (fname : String) -> (ty : Type) ->
          {auto fty : FTy FFI_JS [] ty} -> ty
jscall fname ty = foreign FFI_JS fname ty

export
stringify : Ptr -> String
stringify = unsafePerformIO . jscall "JSON.stringify(%0)" (Ptr -> JS_IO String)

public export
data FromJS : Type -> Type where
  FromJSFun : (Ptr -> Maybe t) -> FromJS t

-- Unsafe
toBool : Ptr -> Bool
toBool ptr = believe_me {b=Int} ptr >= 1

%inline
check : String -> Ptr -> Bool
check c ptr = unsafePerformIO $ map toBool $ jscall c (Ptr -> JS_IO Ptr) ptr

%inline
withCheck : String -> (Ptr -> t) -> FromJS t
withCheck c f = FromJSFun (\ptr => if check c ptr then Just (f ptr)
                                                  else Nothing)
                    
%inline
withTypeOfCheck : String -> (Ptr -> t) -> FromJS t
withTypeOfCheck type = withCheck ("typeof %0 == \"" ++ type ++ "\"")

%hint
export
fromJSPtr : FromJS Ptr
fromJSPtr = FromJSFun (Just . id)

%hint
export
fromJSToString : FromJS String
fromJSToString = withTypeOfCheck "string" believe_me

%hint
export
fromJSToInt : FromJS Int
fromJSToInt = withTypeOfCheck "number" believe_me

%hint
export
fromJSToDouble : FromJS Double
fromJSToDouble = withTypeOfCheck "number" believe_me

%hint
export
fromJSToBool : FromJS Bool
fromJSToBool = withTypeOfCheck "boolean" toBool 

indexArray : Ptr -> Nat -> Ptr
indexArray ptr idx = unsafePerformIO $ jscall "(%0)[%1]" (Ptr -> Int -> JS_IO Ptr) ptr (cast idx)

length : Ptr -> Nat
length ptr = cast $ unsafePerformIO $ jscall "%0.length" (Ptr -> JS_IO Int) ptr

arrayCheck : Ptr -> Bool
arrayCheck = check ("typeof %0 == \"object\" && %0.constructor.name == \"Array\"")

%hint
export
fromJSToList : FromJS a -> FromJS (List a)
fromJSToList (FromJSFun f) = FromJSFun (\ptr => if arrayCheck ptr
                                                  then convert ptr Z (length ptr)
                                                  else Nothing)
                where convert : Ptr -> Nat -> Nat -> Maybe (List a)
                      convert ptr idx length =
                        if idx == length then Just []
                        else (::) <$> f (indexArray ptr idx)
                                  <*> assert_total (convert ptr (S idx) length)

%hint
export
fromJSToTuple : FromJS a -> FromJS b -> FromJS (a, b)
fromJSToTuple (FromJSFun f1) (FromJSFun f2) =
  FromJSFun (\ptr =>
    if arrayCheck ptr && length ptr == 2
       then case (f1 (indexArray ptr 0), f2 (indexArray ptr 1)) of
                 (Just val1, Just val2) => Just (val1, val2)
                 _ => Nothing
        else Nothing)


isObjectCheck : String
isObjectCheck = "typeof %0 == \"object\" && %0 != null"

%hint
export
fromJSRecNil : FromJS (Record [])
fromJSRecNil = withCheck isObjectCheck (const RecNil) 

accessObject : Ptr -> String -> Ptr
accessObject p = unsafePerformIO . jscall "(%0)[%1]" (Ptr -> String -> JS_IO Ptr) p

export
%hint
fromJSRecord : FromJS t -> FromJS (Record xs) -> FromJS (Record ((k, t)::xs))
fromJSRecord {k} (FromJSFun ft) (FromJSFun fxs) = FromJSFun (\ptr =>
              let rec = fxs ptr
              in let fieldPtr = accessObject ptr k
              in RecCons k <$> (ft fieldPtr) <*> rec)

export
partial
fromJSUnsafe : {auto fjs: FromJS to} -> Ptr -> to
fromJSUnsafe {fjs=FromJSFun f} ptr = case (f ptr) of
                                          Just to => to

export
fromJS : {auto fjs: FromJS to} -> Ptr -> Maybe to
fromJS {fjs=FromJSFun f} ptr = f ptr

public export
data ToJS : Type -> Type where
  ToJSFun : (t -> Ptr) -> ToJS t

%hint
export
toJSPtr : ToJS Ptr
toJSPtr = ToJSFun id

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
fromTupleToJS : ToJS a -> ToJS b -> ToJS (a, b)
fromTupleToJS (ToJSFun f1) (ToJSFun f2) =
  ToJSFun (\(a, b) =>
    unsafePerformIO $ do
      arr <- empty_
      push arr (f1 a)
      push arr (f2 b)
      pure arr)


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



