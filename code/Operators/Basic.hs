
module Operators.Basic (
  mutate
  ) where

import System.Random

mutate::(RandomGen g) => g -> [Char] -> (Bool, g)
mutate gen s = should gen

should::(RandomGen g) => g -> (Bool, g)
should gen =
  let (i, g) = next gen
  in if fromIntegral (i `mod` 10) / 10 > 0.2 then (False, g)
     else (True, g)
