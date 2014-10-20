{-# OPTIONS_GHC -funbox-strict-fields #-}

{-# LANGUAGE BangPatterns #-}

-- |
-- Module    : Data.BitVector
-- Copyright : (c) Iago Abal, 2012
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>
--
-- Implementation of bit-vectors as wrappers over 'Integer'.
--
-- * Bit-vectors are interpreted as unsigned integers
--   (i.e. natural numbers) except for some very specific cases.
--
-- * Bit-vectors are /size-polymorphic/ insofar as most operations treat
--   a bit-vector of size /k/ as of size /n/ for /n >= k/ if required.
--
-- For documentation purposes we will write @[n]k@ to denote a bit-vector
-- of size @n@ representing the natural number @k@.
--
module BitVector 
  ( -- * Bit-vectors
    BitVector
  , BV
  , size, width
  , nat, uint, int
    -- * Creation
  , bitVec
  , ones, zeros
    -- * Comparison
  , (==.), (/=.)
    -- * Indexing
  , (@.), (@@)
  , (!.)
  , least, most
  , msb, lsb, msb1
  -- * Arithmetic
  , sdiv, srem, smod
  , lg2
  -- * List-like operations
  , (#)
  , zeroExtend, signExtend
  , foldl_, foldr_
  , reverse_
  , replicate_
  -- * Bitwise operations
  , module Data.Bits
  , not_, nand, nor, xnor
  , (<<.), (>>.), ashr, (<<<.), (>>>.)
  -- * Conversion
  , fromBits
  , toBits
  -- * Utilities
  , maxNat
  , integerWidth
  ) where

import Control.Exception ( assert )

import Data.Bits
import Data.Ord

----------------------------------------------------------------------
--- Bit-vectors

-- | Big-endian /pseudo size-polymorphic/ bit-vectors.
--
data BV
    = BV {
      size :: !Int      -- ^ The /size/ of a bit-vector.
    , nat  :: !Integer  -- ^ The value of a bit-vector, as a natural number.
    }

-- | An alias for 'BV'.
--
type BitVector = BV

-- | An alias for 'size'.
--
width :: BV -> Int
width = size

-- | An alias for 'nat'.
--
uint :: BV -> Integer
uint = nat

-- | 2's complement value of a bit-vector.
int :: BV -> Integer
int u | msb u     = - nat(-u)
      | otherwise = nat u

instance Show BV where
  show (BV n a) = "[" ++ show n ++ "]" ++ show a

----------------------------------------------------------------------
--- Construction

-- | Create a bit-vector given a size and an integer value.
--
-- >>> bitVec 4 3
-- [4]3
--
-- This function also handles negative values.
--
-- >>> bitVec 4 (-1)
-- [4]15
--
bitVec :: Integral a => Int -> a -> BV
bitVec n a | a >= 0    = BV n $ fromIntegral a
           | otherwise = negate $ BV n $ fromIntegral (-a)
{-# INLINE bitVec #-}

-- | Create a mask of ones.
--
ones :: Int -> BV
ones n = BV n $ 2^n - 1
{-# INLINE ones #-}

-- | Create a mask of zeros.
--
zeros :: Int -> BV
zeros n = BV n 0
{-# INLINE zeros #-}

----------------------------------------------------------------------
--- Comparison

instance Eq BV where
  (BV _ a) == (BV _ b) = a == b

instance Ord BV where
  compare = comparing nat

-- | Fixed-size equality.
--
-- In contrast with '==', which is /size-polymorphic/, this equality
-- requires both bit-vectors to be of equal size.
--
-- >>> [n]k ==. [m]k
-- False
--
-- >>> [n]k == [n]k
-- True
--
(==.) :: BV -> BV -> Bool
(BV n a) ==. (BV m b) = n == m && a == b

-- | Fixed-size inequality.
--
-- The negated version of '==.'.
--
(/=.) :: BV -> BV -> Bool
u /=. v = not $ u ==. v

----------------------------------------------------------------------
--- Indexing

-- | Bit indexing.
--
-- @u \@. i@ stands for the /i/-th bit of /u/.
--
-- >>> [4]2 @. 0
-- False
--
-- >>> [4]2 @. 1
-- True
--
(@.) :: BV -> Int -> Bool
(BV _ a) @. i = testBit a i
{-# INLINE (@.) #-}

-- | Bit-string extraction.
--
-- @u \@\@ (j,i) == fromBits (map (u \@.) [j,j-1..i])@
--
-- >>> [4]7 @@ (3,1)
-- [3]3
--
(@@) :: BV -> (Int,Int) -> BV
(BV _ a) @@ (j,i) = assert (j >= i) $
    BV m $ (a `shiftR` i) `mod` 2^m
  where m = j - i + 1

-- | Reverse bit-indexing.
--
-- Index from the end of the sequenc
--
-- @u !. i == u \@. (size u - i - 1) @
--
-- >>> [3]3 !. 0
-- False
--
(!.) :: BV -> Int -> Bool
(BV n a) !. i = assert (i < n) $ testBit a (n-i-1)
{-# INLINE (!.) #-}

-- | Take least significant bits.
--
-- @least m u == u \@\@ (m-1,0)@
--
least :: Int -> BV -> BV
least m (BV _ a) = assert (m >= 1) $
  BV m $ a `mod` 2^m

-- | Take most significant bits.
--
-- @most m u == u \@\@ (n-1,n-m)@
--
most :: Int -> BV -> BV
most m (BV n a) = assert (m >= 1 && m <= n) $
  BV m $ a `shiftR` (n-m)

-- | Most significant bit.
--
-- @msb u == u !. 0@
--
msb :: BV -> Bool
msb = (!. 0)
{-# INLINE msb #-}

-- | Least significant bit.
--
-- @lsb u == u \@. 0@
--
lsb :: BV -> Bool
lsb = (@. 0)
{-# INLINE lsb #-}

-- | Most significant 1-bit.
--
-- /Pre/: input must be non-zero.
--
-- >>> msb1 [4]2
-- 1
--
-- >>> msb1 [4]4
-- 2
--
msb1 :: BV -> Int
msb1 (BV _ 0) = error "Data.BitVector.msb1: zero bit-vector"
msb1 (BV n a) = go (n-1)
  where go i | testBit a i = i
             | otherwise   = go (i-1)

----------------------------------------------------------------------
--- Arithmetic

instance Num BV where
  (BV n1 a) + (BV n2 b) = BV n $ (a + b) `mod` 2^n
    where n = max n1 n2
  (BV n1 a) * (BV n2 b) = BV n $ (a * b) `mod` 2^n
    where n = max n1 n2
  negate (BV n a) = BV n $ 2^n - a
  abs u | msb u     = negate u
        | otherwise = u
  signum u = bitVec 2 $ int u
  fromInteger i = bitVec (integerWidth i) i

instance Real BV where
  toRational = toRational . nat

instance Enum BV where
  toEnum = fromIntegral
  fromEnum (BV _ a) = assert (a < max_int) $ fromIntegral a
    where max_int = toInteger (maxBound::Int)

instance Integral BV where
  quotRem (BV n1 a) (BV n2 b) = (BV n q,BV n r)
    where n = max n1 n2
          (q,r) = quotRem a b
  divMod = quotRem
  toInteger = nat

-- | 2's complement signed division.
--
sdiv :: BV -> BV -> BV
sdiv u@(BV n1 _) v@(BV n2 _) = bitVec n q
  where n = max n1 n2
        q = int u `quot` int v

-- | 2's complement signed remainder (sign follows dividend).
--
srem :: BV -> BV -> BV
srem u@(BV n1 _) v@(BV n2 _) = bitVec n r
  where n = max n1 n2
        r = int u `rem` int v

-- | 2's complement signed remainder (sign follows divisor).
--
smod :: BV -> BV -> BV
smod u@(BV n1 _) v@(BV n2 _) = bitVec n r
  where n = max n1 n2
        r = int u `mod` int v

-- | Ceiling logarithm base 2.
--
-- /Pre/: input bit-vector must be non-zero.
--
lg2 :: BV -> BV
lg2 (BV _ 0) = error "Data.BitVector.lg2: zero bit-vector"
lg2 (BV n 1) = BV n 0
lg2 (BV n a) = BV n $ toInteger $ integerWidth (a-1)

----------------------------------------------------------------------
--- List-like operations

-- | Concatenation of two bit-vectors.
--
(#) :: BV -> BV -> BV
(BV n a) # (BV m b) = BV (n + m) ((a `shiftL` m) + b)
{-# INLINABLE (#) #-}

-- | Logical extension.
--
-- >>> zeroExtend 3 [1]1
-- [4]1
--
zeroExtend :: Int -> BV -> BV
zeroExtend d (BV n a) = BV (n+d) a
{-# INLINE zeroExtend #-}

-- | Arithmetic extension.
--
-- >>> signExtend 2 [2]1
-- [4]1
--
-- >>> signExtend 2 [2]3
-- [4]15
--
signExtend :: Int -> BV -> BV
signExtend d (BV n a)
  | testBit a (n-1) = BV (n+d) $ (maxNat d `shiftL` n) + a
  | otherwise       = BV (n+d) a

-- |
-- @foldl_ f z (fromBits [un, ..., u1, u0]) == ((((z \`f\` un) \`f\` ...) \`f\` u1) \`f\` u0)@
--
-- @foldl_ f e = fromBits . foldl f e . toBits@
--
foldl_ :: (a -> Bool -> a) -> a -> BV -> a
foldl_ f e (BV n a) = go (n-1) e
  where go i !x | i >= 0    = let !b = testBit a i in go (i-1) $ f x b
                | otherwise = x
{-# INLINE foldl_ #-}

-- |
-- @foldr_ f z (fromBits [un, ..., u1, u0]) == un `f` (... `f` (u1 \`f\` (u0 \`f\` z)))@
--
-- @foldr_ f e = fromBits . foldr f e . toBits@
--
foldr_ :: (Bool -> a -> a) -> a -> BV -> a
foldr_ f e (BV n a) = go (n-1) e
 where go i !x | i >= 0    = let !b = testBit a i in f b (go (i-1) x)
               | otherwise = x
{-# INLINE foldr_ #-}

-- |
-- @reverse_ == fromBits . reverse . toBits@
--
reverse_ :: BV -> BV
reverse_ bv@(BV n _) = BV n $ snd $ foldl_ go (1,0) bv
  where go (v,acc) b | b         = (v',acc+v)
                     | otherwise = (v',acc)
          where v' = 2*v

-- |
-- /Pre/: if @replicate_ n u@ then @n > 0@ must hold.
--
-- @replicate_ n == fromBits . concat . replicate n . toBits @
--
replicate_ :: Int -> BV -> BV
replicate_ 0 _ = error "Data.BitVector.replicate_: cannot replicate 0-times"
replicate_ n u = go (n-1) u
  where go 0 !acc = acc
        go k !acc = go (k-1) (u # acc)

----------------------------------------------------------------------
--- Bitwise operations

instance Bits BV where
  (BV n1 a) .&. (BV n2 b) = BV n $ a .&. b
    where n = max n1 n2
  (BV n1 a) .|. (BV n2 b) = BV n $ a .|. b
    where n = max n1 n2
  (BV n1 a) `xor` (BV n2 b) = BV n $ a `xor` b
    where n = max n1 n2
  complement (BV n a) = BV n $ 2^n - 1 - a 
  bit i = BV (i+1) (2^i)
  testBit (BV n a) i | i < n     = testBit a i
                     | otherwise = False
  bitSize = undefined
  isSigned = const False
  shiftL (BV n a) k
    | k > n     = BV n 0
    | otherwise = BV n $ shiftL a k `mod` 2^n
  shiftR (BV n a) k
    | k > n     = BV n 0
    | otherwise = BV n $ shiftR a k
  rotateL bv       0 = bv
  rotateL (BV n a) k
    | k == n    = BV n a
    | k > n     = rotateL (BV n a) (k `mod` n)
    | otherwise = BV n $ h + l
    where s = n - k
          l = a `shiftR` s
          h = (a `shiftL` k) `mod` 2^n
  rotateR bv       0 = bv
  rotateR (BV n a) k
    | k == n    = BV n a
    | k > n     = rotateR (BV n a) (k `mod` n)
    | otherwise = BV n $ h + l
    where s = n - k
          l = a `shiftR` k
          h = (a `shiftL` s) `mod` 2^n
  popCount (BV n a) = n

-- | An alias for 'complement'.
--
not_ :: BV -> BV
not_ = complement
{-# INLINE not_ #-}

-- | Negated '.&.'.
--
nand :: BV -> BV -> BV
nand u v = not_ $ u .&. v
{-# INLINE nand #-}

-- | Negated '.|.'.
--
nor :: BV -> BV -> BV
nor u v = not_ $ u .|. v
{-# INLINE nor #-}

-- | Negated 'xor'.
--
xnor :: BV -> BV -> BV
xnor u v = not_ $ u `xor` v
{-# INLINE xnor #-}

-- | Left shift.
--
(<<.) :: BV -> BV -> BV
bv@BV{size=n} <<. (BV _ k)
  | k >= fromIntegral n  = BV n 0
  | otherwise            = bv `shiftL` (fromIntegral k)
{-# INLINE (<<.) #-}

-- | Logical right shift.
--
(>>.) :: BV -> BV -> BV
bv@BV{size=n} >>. (BV _ k)
  | k >= fromIntegral n  = BV n 0
  | otherwise            = bv `shiftR` (fromIntegral k)
{-# INLINE (>>.) #-}

-- | Arithmetic right shift
--
ashr :: BV -> BV -> BV
ashr u v | msb u     = not_ ((not_ u) >>. v)
         | otherwise = u >>. v

-- | Rotate left.
--
(<<<.) :: BV -> BV -> BV
bv@BV{size=n} <<<. (BV _ k)
  | k >= n'   = bv `rotateL` (fromIntegral $ k `mod` n')
  | otherwise = bv `rotateL` (fromIntegral k)
  where n' = fromIntegral n
{-# INLINE (<<<.) #-}

-- | Rotate right.
--
(>>>.) :: BV -> BV -> BV
bv@BV{size=n} >>>. (BV _ k)
  | k >= n'   = bv `rotateR` (fromIntegral $ k `mod` n')
  | otherwise = bv `rotateR` (fromIntegral k)
  where n' = fromIntegral n
{-# INLINE (>>>.) #-}

----------------------------------------------------------------------
--- Conversion

-- | Create a bit-vector from a big-endian list of bits.
--
-- >>> fromBits [False, False, True]
-- [3]1
--
fromBits :: [Bool] -> BV
fromBits bs = BV n $ snd $ foldr go (1,0) bs
  where n = length bs
        go b (!v,!acc) | b         = (v',acc+v)
                       | otherwise = (v',acc)
          where v' = 2*v

-- | Create a big-endian list of bits from a bit-vector.
--
-- >>> toBits [4]11
-- [True, False, True, True]
--
toBits :: BV -> [Bool]
toBits (BV n a) = map (testBit a) [n-1,n-2..0]

----------------------------------------------------------------------
--- Utilities

-- | Greatest natural number representable with /n/ bits.
--
maxNat :: Integral a => Int -> a
maxNat n = 2^n - 1
{-# INLINE maxNat #-}

-- | Minimum width of a bit-vector to represent a given integer number.
--
-- >>> integerWith 4
-- 3
--
-- >>> integerWith (-4)
-- 4
--
integerWidth :: Integer -> Int
integerWidth !n
  | n >= 0    = go 1 1
  | otherwise = 1 + integerWidth (abs n)
  where go !k !k_max | k_max >= n = k
                     | otherwise  = go (k+1) (2*k_max+1)
{-# INLINE integerWidth #-}