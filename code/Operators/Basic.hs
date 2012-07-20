
module Operators.Basic (
  mutate
  ) where

import System.Random

mutate::(RandomGen g) => g -> [Char] -> ([Char], g)
mutate gen s
  | typ == 2 = (change place s, gen')
  | typ == 1 = (remove place s, gen')
  | typ == 0 = (add place s, gen')
  where (typ, place, gen') = choose gen $ length s

-- chooses the type of mutation and the character where it happens
choose::(RandomGen g) => g -> Int -> (Int, Int, g)
choose gen len =
  let (typ, g) = randomR (0, 2) gen
      (place, g') = randomR (0, len-1) gen
  in (typ, place, g')

change::Int -> [Char] -> [Char]
change p s = "change"

remove::Int -> [Char] -> [Char]
remove p s = "remove"

add::Int -> [Char] -> [Char]
add p s = "add"
