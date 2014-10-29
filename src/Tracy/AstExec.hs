-----------------------------------------------------------------------------
--
-- Module      :  AstExec
-- Copyright   :  Michael Kirkedal Thomsen, 2014
-- License     :  AllRightsReserved
--
-- Maintainer  :  Michael Kirkedal Thomsen kirkedal@informatik.uni-bremen.de
-- Stability   :  none?
-- Portability :
--
-- |Execution of constricts of abstract syntax tree
--
-----------------------------------------------------------------------------

module Tracy.AstExec where

import Tracy.Ast
import Tracy.Types
import qualified BitVector as BV

import Data.Maybe (fromMaybe)
import qualified Data.Array.IArray as A

emptyArray :: Integer -> Array
emptyArray i = A.array (0,i-1) [(j, BV.bitVec 32 0) | j <- [0..i-1]]

arrayFromList :: [BV.BitVector] -> Array
arrayFromList a = A.array (0,len-1) $ zip [0..len-1] a
  where
    len = toInteger $ length a

--(!) array -> index -> value
(!) :: Value -> Value -> Maybe Value
(!) (VArray t p arr) value = Just $ 
  if (d <= v) == (v <= l)
    then VVal t p $ arr A.! v
    else VUnassigned $ TVal t p
  where
    (d, l) = A.bounds arr
    v = fromIntegral $ valueToInteger value
(!) _ _                = Nothing

(//) :: Value -> (Value, Value) -> Value
(//) (VArray      t p fVal) (v, VVal _vt _vp b) = VArray t p (fVal A.// [(valueToInteger v,b)])
(//) v                  v2                = error $ "Wrong array types in update:" ++ show v ++ " - " ++ show v2

typeMax :: TypeVal -> TypeVal -> TypeVal
typeMax TLong  _      = TLong
typeMax _      TLong  = TLong
typeMax TInt   _      = TInt
typeMax _      TInt   = TInt
typeMax TShort _      = TShort
typeMax _      TShort = TShort
typeMax _      _      = TChar

polCast :: TypePolarity -> TypePolarity -> TypePolarity
polCast Signed Signed = Signed
polCast _      _      = Unsigned

valueToInteger :: Value -> Integer
valueToInteger (VVal _ Signed v)   = BV.int v
valueToInteger (VVal _ Unsigned v) = BV.uint v
valueToInteger (VBool True)        = 1
valueToInteger (VBool False)       = 0
valueToInteger _                   = error "valueToInteger is not defined for Unassigned and Void."

valueToType :: Value -> Type
valueToType (VVal  typ pol _)  = TVal typ pol
valueToType (VArray typ pol v) = TArray (TVal typ pol) (Just $ snd $ A.bounds v)
valueToType (VUnassigned t)    = t
valueToType (VBool _)          = TBool
valueToType (VVoid)            = TVoid
valueToType (VPointer typ _)   = TPointer typ
valueToType (VString _)        = TString

boolToValue :: VarSize -> Bool -> Value
boolToValue size True  = VVal TInt Signed $ BV.bitVec (sint size) 1
boolToValue size False = VVal TInt Signed $ BV.bitVec (sint size) 0

constToValue :: TypePolarity -> VarSize -> Integer -> Value
constToValue pol size val    = VVal TInt pol $ BV.bitVec (sint size) val

valueToBool :: Value -> Bool
valueToBool (VVal _ _ v) = 
  if   BV.int v == 1       -- Signed and unsigned have same representation of 1
  then True
  else False
valueToBool (VBool b) = b
valueToBool _         = error "Value cannot be converted to Bool."

toUnsigned :: Value -> Maybe Value
toUnsigned (VVal typ _ b) = Just $ VVal typ Unsigned b
toUnsigned  _             = Nothing

toSigned :: Value -> Maybe Value
toSigned (VVal typ _ b) = Just $ VVal typ Signed b
toSigned _              = Nothing

--- Does not support cast of size...
genCast :: Type -> Value -> Value
genCast (TVal _ Signed)   v = fromMaybe (error "Type error") $ toSigned v
genCast (TVal _ Unsigned) v = fromMaybe (error "Type error") $ toUnsigned v
genCast (TArray typ _) v@(VArray t p vals) = v -- Update with type check and size check
genCast (TPointer typ) v@(VPointer t ident) = v -- Update with type check and size check
genCast t                 v = error $ "No general cast of value '" ++ show v ++ "' to type '" ++ show t ++ "'."

isBoolBinOp :: BinOp -> Bool
isBoolBinOp And   = True
isBoolBinOp Or    = True
isBoolBinOp Eq    = True
isBoolBinOp Neq   = True
isBoolBinOp ULeq  = True
isBoolBinOp UGeq  = True
isBoolBinOp ULth  = True
isBoolBinOp UGth  = True
isBoolBinOp Leq   = True
isBoolBinOp Geq   = True
isBoolBinOp Lth   = True
isBoolBinOp Gth   = True
isBoolBinOp _     = False

instance Ord Value where
  compare v1@(VVal _t1 _p1 _v1) v2@(VVal _t2 _p2 _v2) = compare (valueToInteger v1) (valueToInteger v2)
--  compare  v                (VUnassigned _)   = valueToBool v
  compare v1 v2 = error $ "Wrong types in compare: " ++ show v1 ++ " and " ++ show v2

instance Num Value where
  (+) (VVal t1 p1 v1) (VVal t2 p2 v2) = VVal (typeMax t1 t2) (polCast p1 p2) $ (+) v1 v2
  (+) _ _ = error "Wrong types in +"

  (*) (VVal t1 p1 v1) (VVal t2 p2 v2) = VVal (typeMax t1 t2) (polCast p1 p2) $ (*) v1 v2
  (*) _ _ = error "Wrong types in *"

  abs (VVal t p v) = VVal t p $ abs v
  abs _            = error "Wrong type in abs"

  signum (VVal t p v) = VVal t p $ signum v
  signum _            = error "Wrong type in signum"

  fromInteger i = VVal TInt Signed $ fromInteger i

  negate (VVal t p v) = VVal t p $ negate v
  negate _        = error "Wrong type in negate"

division :: Value -> Value -> Value
division val1@(VVal t1 p1 v1) val2@(VVal t2 p2 v2) = 
  VVal t1 p1 (v1 `div` v2)

and :: Value -> Value -> Maybe Value
and val1@(VVal t1 p1 v1) val2@(VVal t2 p2 v2) = 
  case (valueToInteger val1, valueToInteger val2) of
    (0,_)     -> Just $ VVal typ pol $ BV.bitVec size 0
    (_,0)     -> Just $ VVal typ pol $ BV.bitVec size 0
    otherwise -> Just $ VVal typ pol $ BV.bitVec size 1
  where
    typ = typeMax t1 t2
    size = max (BV.size v1) (BV.size v2)
    pol  = polCast p1 p2
and (VBool b1) (VBool b2) = Just $ VBool $ Prelude.and [b1,b2]
and _           _             = Nothing

or :: Value -> Value -> Maybe Value
or val1@(VVal t1 p1 v1) val2@(VVal t2 p2 v2) = 
  case (valueToInteger val1, valueToInteger val2) of
    (1,_)     -> Just $ VVal typ pol $ BV.bitVec size 1
    (_,1)     -> Just $ VVal typ pol $ BV.bitVec size 1
    otherwise -> Just $ VVal typ pol $ BV.bitVec size 0
  where
    typ = typeMax t1 t2
    size = max (BV.size v1) (BV.size v2)
    pol  = polCast p1 p2
or (VBool b1) (VBool b2) = Just $ VBool $ Prelude.or [b1,b2]
or _           _       = Nothing

not :: Value -> Maybe Value
not val@(VVal t p v) = 
  if valueToInteger val == 1 
  then Just $ VVal t p $ BV.bitVec size 0
  else Just $ VVal t p $ BV.bitVec size 1
  where 
    size = BV.size v
not (VBool b) = Just $ VBool $ Prelude.not b
not _         = Nothing

(.|.) :: Value -> Value -> Maybe Value
(.|.) (VVal  t1 p1 v1) (VVal t2 p2 v2) = Just $ VVal (typeMax t1 t2) (polCast p1 p2) $ v1 BV..|. v2
(.|.) (VBool b1)       (VBool b2)      = Just $ VBool $ Prelude.or [b1,b2]
(.|.) _ _ = Nothing

(.^.) :: Value -> Value -> Maybe Value
(.^.) (VVal  t1 p1 v1) (VVal t2 p2 v2) = Just $ VVal (typeMax t1 t2) (polCast p1 p2) $ BV.xor v1 v2
(.^.) (VBool b1)       (VBool b2)      = Just $ VBool $ b1 /= b2
(.^.) _ _ = Nothing

(.&.) :: Value -> Value -> Maybe Value
(.&.) (VVal  t1 p1 v1) (VVal t2 p2 v2) = Just $ VVal (typeMax t1 t2) (polCast p1 p2) $ v1 BV..&. v2
(.&.) (VBool b1)       (VBool b2)      = Just $ VBool $ Prelude.and [b1,b2]
(.&.) _ _ = Nothing

(<<.) :: Value -> Value -> Maybe Value
(<<.) (VVal t1 p1 v1) v2 = Just $ VVal t1 p1 $ BV.shiftL v1 $ fromInteger $ valueToInteger v2
(<<.) _ _ = Nothing

(>>.) :: Value -> Value -> Maybe Value
(>>.) (VVal t1 p1 v1) v2 = Just $ VVal t1 p1 $ BV.shiftR v1 $ fromInteger $ valueToInteger v2
(>>.) _ _ = Nothing

not_ :: Value -> Maybe Value
not_ (VVal t p v1) = Just $ VVal t p $ BV.not_ v1
not_ (VBool b)     = Just $ VBool $ Prelude.not b
not_ _ = Nothing

bitIdx :: Value -> Value -> Maybe Value
bitIdx (VVal t1 p1 v1) (VVal _ Unsigned v2) = Just $ VVal t1 p1 $ (BV.zeros (size-1-iv2)) BV.# (BV.fromBits [(v1 BV.@. iv2)]) BV.# (BV.zeros iv2)
  where
    iv2 = fromIntegral $ BV.uint v2
    size = BV.size v1
bitIdx _ _ = Nothing

