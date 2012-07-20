
module Operators.Basic (
  mutate
  ) where

import System.Random
import Data.List

mutate::(RandomGen g) => [Char] -> g -> ([Char], g)
mutate s gen
  | typ == 2 = (change place s gen', gen')
  | typ == 1 = (remove place s, gen')
  | typ == 0 = (add place s gen', gen')
  where (typ, place, gen') = choose (length s) gen

-- chooses the type of mutation and the character where it happens
choose::(RandomGen g) => Int -> g -> (Int, Int, g)
choose len gen =
  let (typ, g) = randomR (0, 2) gen
      (place, g') = randomR (0, len-1) gen
  in (typ, place, g')

change::(RandomGen g) => Int -> [Char] -> g -> [Char]
change p s gen =
  let chars = " abcdefghijklmnopqrtstuvwxyzABCDEFGHIJKLMNOPQRTSUVWXYZ"
      (c, gen') = randomR (0, (-1 +)$length chars) gen
      (left, right) = splitAt p s
  in left ++ chars!!c:right

remove::Int -> [Char] -> [Char]
remove p s =
  let (left, right) = splitAt p s
  in left ++ tail right

add::(RandomGen g) => Int -> [Char] -> g -> [Char]
add p s gen =
  let chars = " abcdefghijklmnopqrtstuvwxyzABCDEFGHIJKLMNOPQRTSUVWXYZ"
      (c, gen') = randomR (0, (-1 +)$length chars) gen
      (left, right) = splitAt p s
  in left ++ chars!!c:tail right
