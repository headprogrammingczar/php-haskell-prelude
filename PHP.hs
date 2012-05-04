{-# LANGUAGE NoImplicitPrelude, ExtendedDefaultRules #-}
module PHP where

import Data.List (group)
import Prelude

sort = sortBy compare

-- sort function, optimized for lists
-- TODO - profiling
sortBy compare = head . head . dropWhile (not . null . drop 1) . group . iterate bubble
  where
    bubble [] = []
    bubble [x] = [x]
    bubble (x:y:ys) = if compare x y == GT then y:x:(bubble ys) else if compare x y == EQ then x:(bubble (y:ys)) else if compare x y == LT then x:(bubble (y:ys)) else x:y:(bubble ys)

