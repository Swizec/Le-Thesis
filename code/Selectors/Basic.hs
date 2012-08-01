
module Selectors.Basic (
  select
  ) where

import Data.List

-- the basic selector just picks the top 50% and passes them on
select::(Num a, Ord a) => [(a, [Char])] -> [(a, [Char])]
select xs = take (length xs `div` 2) $ sortBy (\a b -> compare (fst a) (fst b)) xs
