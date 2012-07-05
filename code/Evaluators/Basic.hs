
module Evaluators.Basic (
  evaluate,
  levenshtein
) where

import Data.Array


evaluate::(Num a) => [Char] -> (a, [Char])
evaluate s = (fromIntegral $ levenshtein s "Hello World", s)


-- calculate levenshtein distance between two strings
levenshtein::[Char] -> [Char] -> Int
levenshtein "" "" = 0
levenshtein "" s2 = length s2
levenshtein s1 "" = length s1
levenshtein s1 s2
   | last s1 == last s2 = levenshtein (init s1) (init s2)
   | otherwise = minimum [1 + levenshtein (init s1) s2,
                          1 + levenshtein s1 (init s2),
                          1 + levenshtein (init s1) (init s2)]
