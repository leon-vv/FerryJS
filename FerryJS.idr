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

-- Could not be declared as ToIdris a = (Ptr -> Maybe a)
-- because a chance exists that, when using auto, Idris
-- builds a 'random' function of that type.
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
withTypeOfCheck type = withCheck ("typeof %0 == \"" ++ type ++ "\"")

%hint
export
fromJSPtr : ToIdris Ptr
fromJSPtr = ToIdrisFn (Just . id)

%hint
export
fromJSToString : ToIdris String
fromJSToString = withTypeOfCheck "string" believe_me

%hint
export
fromJSToInt : ToIdris Int
fromJSToInt = withTypeOfCheck "number" believe_me

%hint
export
fromJSToDouble : ToIdris Double
fromJSToDouble = withTypeOfCheck "number" believe_me

%hint
export
fromJSToBool : ToIdris Bool
fromJSToBool = withTypeOfCheck "boolean" toBool 

%hint
export
fromJSToMaybe : ToIdris a -> ToIdris (Maybe a)
fromJSToMaybe (ToIdrisFn f) = withCheck "%0 != null" f


indexArray : Ptr -> Nat -> Ptr
indexArray ptr idx = unsafePerformIO $ jscall "(%0)[%1]" (Ptr -> Int -> JS_IO Ptr) ptr (cast idx)

length : Ptr -> Nat
length ptr = cast $ unsafePerformIO $ jscall "%0.length" (Ptr -> JS_IO Int) ptr

arrayCheck : Ptr -> Bool
arrayCheck = check ("typeof %0 == \"object\" && %0.constructor.name == \"Array\"")
    
%hint
export
fromJSToList : ToIdris a -> ToIdris (List a)
fromJSToList (ToIdrisFn f) = ToIdrisFn (\ptr => if arrayCheck ptr
                                                  then convert ptr Z (length ptr)
                                                  else Nothing)
                where convert : Ptr -> Nat -> Nat -> Maybe (List a)
                      convert ptr idx length =
                        if idx == length then Just []
                        else (::) <$> f (indexArray ptr idx)
                                  <*> assert_total (convert ptr (S idx) length)

%hint
export
fromJSToTuple : ToIdris a -> ToIdris b -> ToIdris (a, b)
fromJSToTuple (ToIdrisFn f1) (ToIdrisFn f2) =
  ToIdrisFn (\ptr =>
    if arrayCheck ptr && length ptr == 2
       then case (f1 (indexArray ptr 0), f2 (indexArray ptr 1)) of
                 (Just val1, Just val2) => Just (val1, val2)
                 _ => Nothing
        else Nothing)


isObjectCheck : String
isObjectCheck = "typeof %0 == \"object\" && %0 != null"

%hint
export
fromJSRecNil : ToIdris (Record [])
fromJSRecNil = withCheck isObjectCheck (const RecNil) 

accessObject : Ptr -> String -> Ptr
accessObject p = unsafePerformIO . jscall "(%0)[%1]" (Ptr -> String -> JS_IO Ptr) p

export
%hint
fromJSRecord : ToIdris t -> ToIdris (Record xs) -> ToIdris (Record ((k, t)::xs))
fromJSRecord {k} (ToIdrisFn ft) (ToIdrisFn fxs) = ToIdrisFn (\ptr =>
              let rec = fxs ptr
              in let fieldPtr = accessObject ptr k
              in RecCons k <$> (ft fieldPtr) <*> rec)

export
partial
fromJSUnsafe : {auto fjs: ToIdris to} -> Ptr -> to
fromJSUnsafe {fjs=ToIdrisFn f} ptr = case (f ptr) of
                                          Just to => to

export
fromJS : {auto fjs: ToIdris to} -> Ptr -> Maybe to
fromJS {fjs=ToIdrisFn f} ptr = f ptr

-- See comment above 'ToIdris'
public export
data ToJS : Type -> Type where
  ToJSFn : (t -> Ptr) -> ToJS t

%hint
export
toJSPtr : ToJS Ptr
toJSPtr = ToJSFn id

%hint
export
fromIntToJS : ToJS Int
fromIntToJS = ToJSFn believe_me

%hint
export
fromStringToJS : ToJS String
fromStringToJS = ToJSFn believe_me

%hint
export
fromDoubleToJS : ToJS Double
fromDoubleToJS = ToJSFn believe_me

%inline
pureJSValue : String -> Ptr
pureJSValue expr = unsafePerformIO $ jscall expr (JS_IO Ptr)

%hint
export
fromBoolToJS : ToJS Bool
fromBoolToJS = ToJSFn (\b =>
                if b
                  then pureJSValue "true"
                  else pureJSValue "false")

%hint
export
fromMaybeToJS : ToJS a -> ToJS (Maybe a)
fromMaybeToJS (ToJSFn f) = ToJSFn (\m => case m of
              Nothing => pureJSValue "null"
              Just x => f x)

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
fromListToJS (ToJSFn f) =
  ToJSFn (unsafePerformIO . foldl append empty_)
    where append : JS_IO Ptr -> a -> JS_IO Ptr
          append io val = do
            arr <- io
            push arr (f val)
            pure arr

%hint
export
fromTupleToJS : ToJS a -> ToJS b -> ToJS (a, b)
fromTupleToJS (ToJSFn f1) (ToJSFn f2) =
  ToJSFn (\(a, b) =>
    unsafePerformIO $ do
      arr <- empty_
      push arr (f1 a)
      push arr (f2 b)
      pure arr)


%hint
export
fromRecNilToJS : ToJS (Record [])
fromRecNilToJS= ToJSFn (\RecNil => pureJSValue "{}")

setObjectField : Ptr -> String -> Ptr -> JS_IO ()
setObjectField = jscall "(%0)[%1] = %2" (Ptr -> String -> Ptr -> JS_IO ()) 

-- See comment above the 'ToJS (List a)' implementation
%hint
export
fromRecordToJS : ToJS t -> ToJS (Record xs) -> ToJS (Record ((k, t)::xs))
fromRecordToJS (ToJSFn ft) (ToJSFn fxs) = 
  ToJSFn (\(RecCons key val rest) => let obj = fxs rest
                                      in unsafePerformIO $ do
                                        setObjectField obj key (ft val)
                                        pure obj)
export
toJS : {auto tjs: ToJS t} -> t -> Ptr
toJS {tjs=ToJSFn f} val = f val



