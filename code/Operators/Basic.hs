
module Operators.Basic (
  mutate
  ) where

import System.Random

mutate::(RandomGen g) => [Char] -> g -> ([Char], g)
mutate s gen
  | typ == 2 = (change place s, gen')
  | typ == 1 = (remove place s, gen')
  | typ == 0 = (add place s, gen')
  where (typ, place, gen') = choose (length s) gen

-- chooses the type of mutation and the character where it happens
choose::(RandomGen g) => Int -> g -> (Int, Int, g)
choose len gen =
  let (typ, g) = randomR (0, 2) gen
      (place, g') = randomR (0, len-1) gen
  in (typ, place, g')

change::Int -> [Char] -> [Char]
change p s = "change"

remove::Int -> [Char] -> [Char]
remove p s = "remove"

add::Int -> [Char] -> [Char]
add p s = "add"
