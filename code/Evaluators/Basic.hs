
module Evaluators.Basic (
  evaluate,
  levenshtein
) where

import Data.Array


evaluate::(Num a) => [Char] -> (a, [Char])
evaluate s = (fromIntegral $ levenshtein s "Hello World", s)


-- calculate levenshtein distance between two strings
levenshtein::[Char] -> [Char] -> Int
-- this part is mostly a speed optimiziation
levenshtein s1 s2
  | length s1 > length s2 = levenshtein s2 s1
  | length s1 < length s2 =
    let d = length s2 - length s1
    in d + levenshtein s1 (take (length s2 - d) s2)
-- the meat of the algorithm
levenshtein "" "" = 0
levenshtein s1 s2
  | last s1 == last s2 = levenshtein (init s1) (init s2)
  | otherwise = minimum [1 + levenshtein (init s1) s2,
                         1 + levenshtein s1 (init s2),
                         1 + levenshtein (init s1) (init s2)]
