
module Evaluators.Basic (
  evaluate
) where

import Data.Array


evaluate::(Num a) => [Char] -> (a, [Char])
--evaluate s = (fromIntegral $ levenshtein s "Hello World", s)
evaluate s = (fromIntegral $ lev''' s "Hello World", s)


-- calculate levenshtein distance between two strings
--levenshtein::[Char] -> [Char] -> Int
-- this part is mostly a speed optimiziation
--levenshtein s1 s2
--  | length s1 > length s2 = levenshtein s2 s1
--  | length s1 < length s2 =
--    let d = length s2 - length s1
--    in d + levenshtein s1 (take (length s2 - d) s2)
-- the meat of the algorithm
--levenshtein "" "" = 0
--levenshtein s1 s2
--  | last s1 == last s2 = levenshtein (init s1) (init s2)
--  | otherwise = minimum [1 + levenshtein (init s1) s2,
--                         1 + levenshtein s1 (init s2),
--                         1 + levenshtein (init s1) (init s2)]

--- taken from http://www.reddit.com/r/programming/comments/w4gs6/levenshtein_distance_in_haskell/
lev''' :: (Eq a) => [a] -> [a] -> Int
lev''' xs ys = levMemo ! (n, m)
  where levMemo = array ((0,0),(n,m)) [((i,j),lev i j) | i <- [0..n], j <- [0..m]]
        n = length xs - 1
        m = length ys - 1
        xa = listArray (0, n) xs
        ya = listArray (0, m) ys
        lev 0 v = v
        lev u 0 = u
        lev u v
          | xa ! u == ya ! v = levMemo ! (u-1, v-1)
          | otherwise        = 1 + minimum [levMemo ! (u, v-1),
                                            levMemo ! (u-1, v),
                                            levMemo ! (u-1, v-1)]
