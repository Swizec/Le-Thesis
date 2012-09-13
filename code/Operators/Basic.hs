
module Operators.Basic (
  mutate,
  breed
  ) where

import System.Random
import Data.List

import qualified Evaluators.Basic as Evaluator

-- runs mutation on a whole population
mutate :: (RandomGen g) => (g, [[Char]]) -> (g, [[Char]])
mutate (gen, xs) = mapAccumL mutate1 gen xs

-- mutates single element several times
mutate1::(RandomGen g) => g -> [Char] -> (g, [Char])
mutate1 gen s = fst $ mapAccumL (\(g, s) _ -> (mutate' g s, 0)) (gen, s) [1]

-- possibly mutate a single string
mutate'::(RandomGen g) => g -> [Char] -> (g, [Char])
mutate' gen s
  | should' = do_operation gen' s
  | otherwise = (gen', s)
  where (gen', should') = should gen (fst $ Evaluator.evaluate s)

-- do a mutation
do_operation::(RandomGen g) => g -> [Char] -> (g, [Char])
do_operation gen s
  | typ == 2 = change place s gen'
  | typ == 1 = (gen', remove place s)
  | typ == 0 = add place s gen'
  where (typ, place, gen') = choose (length s) gen

-- chooses the type of mutation and the character where it happens
choose::(RandomGen g) => Int -> g -> (Int, Int, g)
choose len gen =
  let (typ, g) = randomR (0, 2) gen
      (place, g') = randomR (0, len-1) gen
  in (typ, place, g')

-- replaces character at p with random character
change::(RandomGen g) => Int -> [Char] -> g -> (g, [Char])
change p s gen =
  let (gen', c) = randChar gen
      (left, right) = splitAt p s
  in (gen', left ++ c:right)

-- removes character at p
remove::Int -> [Char] -> [Char]
remove p s =
  let (left, right) = splitAt p s
  in left ++ tail right

-- adds random character at p
add::(RandomGen g) => Int -> [Char] -> g -> (g, [Char])
add p s gen =
  let (gen', c) = randChar gen
      (left, right) = splitAt p s
  in (gen', left ++ c:tail right)

randChar::(RandomGen g) => g -> (g, Char)
randChar gen =
  let chars = " abcdefghijklmnopqrtstuvwxyzABCDEFGHIJKLMNOPQRTSUVWXYZ"
      (c, gen') = randomR (0, (-1 +)$length chars) gen
  in (gen', chars!!c)

should::(RandomGen g, Fractional a, Ord a) => g -> a -> (g, Bool)
should gen fitness =
  let (i, g) = next gen
  in (g, 1-1/(fitness/(1-(fromIntegral (i `mod` 10) / 10))) > 0.8)


-- performs breeding on a population
breed::(RandomGen g) => g -> [[Char]] -> (g, [[Char]])
breed gen xs =
--  let pairs = map (\ [a,b] -> (a,b)) $ filter (\a -> length a == 2) . (!!0) $
--              map (subsequences) $ permutations xs
  let pairs = (zip xs $ repeat xs!!0)++(zip xs $ repeat xs!!1)
      (gen', bred) = mapAccumL breedTwo gen pairs
  in (gen', xs ++ foldr (\ (a,b) acc -> a:b:acc) [] bred)

-- breeds two strings
breedTwo::(RandomGen g) => g -> ([Char], [Char]) -> (g, ([Char], [Char]))
breedTwo gen (a, b) =
  let (len, gen') = randomR (0, length a `div` 2) gen
      (s1, gen'') = randomR (0, length a - len) gen'
      (s2, gen''') = randomR (0, length b - len) gen''
      (l1, r1) = splitAt s1 a
      (l2, r2) = splitAt s2 b
  in (gen''',
      (l1++fst (splitAt len r2)++snd (splitAt len r1),
      l2++fst (splitAt len r1)++snd (splitAt len r2)))
