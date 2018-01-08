import Record

%inline
jscall : (fname : String) -> (ty : Type) ->
          {auto fty : FTy FFI_JS [] ty} -> ty
jscall fname ty = foreign FFI_JS fname ty


interface FromJS to where
  fromJS : Ptr -> to

implementation FromJS String where
  fromJS = believe_me

implementation FromJS Int where
  fromJS = (cast {from=Double} {to=Int}) . believe_me

implementation FromJS Double where
  fromJS = believe_me

implementation FromJS Bool where
  fromJS ptr = (believe_me {b=Int} ptr) >= 1

private
indexArray : Ptr -> Nat -> Ptr
indexArray ptr idx = unsafePerformIO $ jscall "(%0)[%1]" (Ptr -> Int -> JS_IO Ptr) ptr (cast idx)

private
length : Ptr -> Nat
length ptr = cast $ unsafePerformIO $ jscall "%0.length" (Ptr -> JS_IO Int) ptr

FromJS a => FromJS (List a) where
  fromJS ptr =  convert ptr (length ptr)
    where convert : FromJS a => Ptr -> Nat -> List a
          convert ptr O = []
          convert ptr (S n) = (fromJS {to=a} (indexArray ptr n)) :: convert ptr n

FromJS (Record []) where
  fromJS _ = RecNil

accessObject : Ptr -> String -> Ptr
accessObject ptr str =
  let io = jscall "(%0)[%1]" (Ptr -> String -> JS_IO Ptr) ptr str
  in unsafePerformIO io

(FromJS (Record xs), FromJS t) => FromJS (Record ((k, t)::xs)) where
  fromJS {k} {t} {xs} ptr =
    let rec = fromJS {to=Record xs} ptr
    in let fieldPtr = accessObject ptr k
    in RecCons k (fromJS {to=t} fieldPtr) rec



