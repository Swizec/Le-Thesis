
module Operators.Basic (
  mutate
  ) where

import System.Random
import Data.List

mutate::(RandomGen g) => [Char] -> g -> ([Char], g)
mutate s gen
  | should' = mutate' s gen'
  | otherwise = (s, gen')
  where (should', gen') = should gen

mutate'::(RandomGen g) => [Char] -> g -> ([Char], g)
mutate' s gen
  | typ == 2 = change place s gen'
  | typ == 1 = (remove place s, gen')
  | typ == 0 = add place s gen'
  where (typ, place, gen') = choose (length s) gen

-- chooses the type of mutation and the character where it happens
choose::(RandomGen g) => Int -> g -> (Int, Int, g)
choose len gen =
  let (typ, g) = randomR (0, 2) gen
      (place, g') = randomR (0, len-1) gen
  in (typ, place, g')

-- replaces character at p with random character
change::(RandomGen g) => Int -> [Char] -> g -> ([Char], g)
change p s gen =
  let (c, gen') = randChar gen
      (left, right) = splitAt p s
  in (left ++ c:right, gen')

-- removes character at p
remove::Int -> [Char] -> [Char]
remove p s =
  let (left, right) = splitAt p s
  in left ++ tail right

-- adds random character at p
add::(RandomGen g) => Int -> [Char] -> g -> ([Char], g)
add p s gen =
  let (c, gen') = randChar gen
      (left, right) = splitAt p s
  in (left ++ c:tail right, gen')

randChar::(RandomGen g) => g -> (Char, g)
randChar gen =
  let chars = " abcdefghijklmnopqrtstuvwxyzABCDEFGHIJKLMNOPQRTSUVWXYZ"
      (c, gen') = randomR (0, (-1 +)$length chars) gen
  in (chars!!c, gen')

should::(RandomGen g) => g -> (Bool, g)
should gen =
  let (i, g) = next gen
  in (fromIntegral (i `mod` 10) / 10 > 0.3,
      g)
