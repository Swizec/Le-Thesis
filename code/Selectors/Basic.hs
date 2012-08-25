
module Selectors.Basic (
  select,
  order
  ) where

import Data.List

select::(Num a, Ord a) => [(a, [Char])] -> [(a, [Char])]
select xs = top100k xs

-- gives the top 100k members
top100k::(Num a, Ord a) => [(a, [Char])] -> [(a, [Char])]
top100k xs = take 100000 $ order xs

-- the basic selector just picks the top 50% and passes them on
top50::(Num a, Ord a) => [(a, [Char])] -> [(a, [Char])]
top50 xs = take (length xs `div` 2) $ order xs

-- sorts an evaluated population
order::(Num a, Ord a) => [(a, [Char])] -> [(a, [Char])]
order xs = sortBy (\a b -> compare (fst a) (fst b)) xs
