
module Evaluators.Basic (
  evaluate
) where

import Data.Array
import Control.Parallel.Strategies


evaluate::(Num a) => [Char] -> (a, [Char])
--evaluate s = (fromIntegral $ levenshtein s "Hello World", s)
evaluate s
  -- make sure string doesn't get too short
  | len >= 11 = (fromIntegral $ lev''' s "Hello World", s)
  | len < 11 = (100000, s)
  where len = length s


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


-- nonquadratic space version of the above
-- from http://www.reddit.com/r/programming/comments/w4gs6/levenshtein_distance_in_haskell/c5aejpf
levInit xa ya = (0, array (0,m) [(j,j) | j <- [0..m]])
  where (0,m) = bounds ya

levStep xa ya (i,ra) = withStrategy rdeepseq ra' `seq` (i+1,ra')
  where ra' = array (0,m) [(j, lev j) | j <- [0..m]]
        (0,m) = bounds ya
        lev 0 = i
        lev j | xa ! i == ya ! j  = ra ! (j-1)
              | otherwise         = 1 + minimum [ra ! j, ra ! (j-1), ra' ! (j-1)]

strictList = foldr (\x xs -> x `seq` x:xs) []

lev xs ys | n < m     = lev ys xs
          | otherwise = case (strictList $ iterate (levStep xa ya) (levInit xa ya)) !! n of
                          (i,a) -> a ! m
  where n = length xs - 1
        m = length ys - 1
        xa = listArray (0,n) xs
        ya = listArray (0,m) ys
