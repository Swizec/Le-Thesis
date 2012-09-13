
module Selectors.Basic (
  select,
  order,
  best
  ) where

import Data.List

import Config

select::(Num a, Ord a) => [(a, [Char])] -> [(a, [Char])]
select xs = top Config.max_population xs

-- gives the top N members
top::(Num a, Ord a) => Int -> [(a, [Char])] -> [(a, [Char])]
top n xs = take n $ order xs

-- the basic selector just picks the top 50% and passes them on
top50::(Num a, Ord a) => [(a, [Char])] -> [(a, [Char])]
top50 xs = take (length xs `div` 2) $ order xs

-- sorts an evaluated population
order::(Num a, Ord a) => [(a, [Char])] -> [(a, [Char])]
order xs = sortBy (\a b -> compare (fst a) (fst b)) xs

-- returns the best from an evaluated population
best::(Num a, Ord a) => [(a, [Char])] -> (a, [Char])
best xs = minimumBy (\a b -> compare (fst a) (fst b)) xs
