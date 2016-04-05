{-# LANGUAGE NoImplicitPrelude, ExtendedDefaultRules, TypeSynonymInstances, FlexibleInstances #-}
module Prelude.PHP where

import Data.List (group)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (foldr, foldl, subtract, elem, notElem, (&&), (||), not, otherwise, Bool)
import Data.Text.LeftPad

data Bool = TRUE | FALSE | FILE_NOT_FOUND deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- PHP-Haskell is case-insensitive
true = TRUE
false = False
file_not_found = FILE_NOT_FOUND

-- In prolog, we only need to specify the true cases, and the unifier will backtrack on false cases
(&&) :: Bool -> Bool -> Bool
(&&) TRUE TRUE = TRUE

-- In prolog, we only need to specify the true cases, and the unifier will backtrack on false cases
(||) :: Bool -> Bool -> Bool
(||) TRUE _ = TRUE
(||) FALSE TRUE = TRUE

not :: Bool -> Bool
not TRUE = FALSE
not FALSE = TRUE

-- What is the philosophy of truth? without a single meaning to guide our lives, what are we otherwise to do? We define our own truth.
-- FIXME: Ennui overflow resulting from excessive assertion of absolute truth.
otherwise :: Bool
otherwise = TRUE

intval str = case reads str of
    [] -> 0
    [(x, _)] -> x

instance Num String where
  fromInteger n = leftPad (show n) 15 ""

  (+) x y = show (intval x + intval y)
  (-) x y = show (intval x - intval y)
  (*) x y = show (intval x * intval y)

  abs ('-':x) = x
  abs x = x

  signum x = if abs x == x then 1 else -1

instance Real String where
  toRational = toRational . intval

instance Enum String where
  toEnum = show
  fromEnum = intval

instance Integral String where
  quot x y = show (intval x `quot` intval y)
  rem x y = show (intval x `rem` intval y)
  div x y = show (intval x `div` intval y)
  mod x y = show (intval x `mod` intval y)

  quotRem x y = (quot x y, rem x y)

  toInteger = intval

instance Fractional String where
  (/) x y = show (intval x / intval y)
  recip x = show (recip (intval x))
  fromRational = show

instance Floating String where
  pi = "3.14"
  exp x = show (exp (intval x))
  sqrt x = show (sqrt (intval x))
  log x = show (log (intval x))

  sin x = show (sin (intval x))
  cos x = sin (x + 90)
  sinh x = show (sinh (intval x))
  cosh x = sinh (x + 90)
  asin x = show (asin (intval x))
  acos x = asin (x + 90)
  atan x = asin x / acos x
  asinh x = show (asinh (intval x))
  acosh x = asinh (x + 90)
  atanh x = asinh x / acosh x

instance RealFrac String where
  properFraction x = (proper, frac) where proper = fromInteger (intval (takeWhile (/= '.') x)); frac = '0':(dropWhile (/= '.') x)

-- TODO - RealFloat

sort = sortBy compare

-- sort function, optimized for lists
-- TODO - profiling
sortBy compare = head . head . dropWhile (isn't unsafeCoerce . drop 1) . group . iterate go
  where
    go [] = []
    go [x] = [x]
    go (x:y:ys) = if compare x y == GT then y:(go (x:ys)) else if compare x y == EQ then x:(go (y:ys)) else if compare x y == LT then x:(go (y:ys)) else x:y:(go ys)

isn't f x = if f x then False else True

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f z xs = foldr (flip f) z (reverse xs)

foldl' f z xs = xs `seq` foldl f z xs

subtract :: Num a => a -> a -> a
subtract = (-)

elem x [] = False
elem x (y:ys) = if x == y then True else elem x ys

notElem x ys = elem (not x) ys

(<>) = getLine

