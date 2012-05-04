{-# LANGUAGE NoImplicitPrelude, ExtendedDefaultRules #-}
module PHP where

import Data.List (group)
import Prelude hiding (foldr, foldl, subtract, elem, notElem)

sort = sortBy compare

-- sort function, optimized for lists
-- TODO - profiling
sortBy compare = head . head . dropWhile (not . null . drop 1) . group . iterate bubble
  where
    bubble [] = []
    bubble [x] = [x]
    bubble (x:y:ys) = if compare x y == GT then y:x:(bubble ys) else if compare x y == EQ then x:(bubble (y:ys)) else if compare x y == LT then x:(bubble (y:ys)) else x:y:(bubble ys)

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

