
import qualified Initiators.Basic as Initiator
import qualified Evaluators.Basic as Evaluator
import qualified Operators.Basic as Operators
import qualified Selectors.Basic as Selector

import System.Random
import Data.List

evaluate :: (Num a) => [[Char]] -> [(a, [Char])]
evaluate [x] = [Evaluator.evaluate x]
evaluate (x:xs) = (Evaluator.evaluate x):evaluate xs

mutate :: (RandomGen g) => g -> [[Char]] -> [([Char], g)]
mutate gen [x] = [Operators.mutate x gen]
mutate gen (x:xs) =
  let (x', gen') = Operators.mutate x gen
  in (x', gen'):mutate gen' xs

mutate' :: (RandomGen g) => g -> [[Char]] -> (g, [[Char]])
mutate' gen xs =
  let mutated = mutate gen xs
  in (snd $ last mutated, map (fst) mutated)

breed'::(RandomGen g) => g -> ([Char], [Char]) -> (g, ([Char], [Char]))
breed' gen (a,b) =
  let (a', b', gen') = Operators.breed a b gen
  in (gen', (a',b'))

breed::(RandomGen g, Num a, Ord a) => g -> [(a, [Char])] -> [[Char]]
breed gen xs =
  let pairs = map (\ [a,b] -> (a,b)) $ filter (\a -> length a == 2) . (!!0) $
              map (subsequences) $ permutations $
              map (snd) $ Selector.select xs
      (gen', bred) = mapAccumL breed' gen pairs
  in (map (snd) xs) ++ foldr (\ (a,b) acc -> a:b:acc) [] bred


-- main loop
population :: (RandomGen g) => Int -> g -> (g, [[Char]])
population 0 gen = mutate' gen $ take 6 $ Initiator.start_population gen
population n gen =
  let (gen', xs) = population (n-1) gen
--      (new_pop, gen'') = breed gen' $ Selector.select . evaluate xs
  in (gen', map (snd) $! Selector.select $ evaluate xs)

main = do
  randomGen <- newStdGen

--  print $ Operators.breed "Hello World" "This is silly" randomGen

  print $ length $ breed randomGen $ evaluate $ snd $ population 0 randomGen

--  print $ evaluate $ snd $ population 0 randomGen
--  print $ evaluate $ snd $ population 1 randomGen
--  print $ evaluate $ snd $ population 2 randomGen
--  print $ evaluate $ snd $ population 3 randomGen
--  print $ evaluate $ snd $ population 4 randomGen

--  print $ mutate' randomGen $ population 0 randomGen

--  print $ Operator.mutate "Hello World" randomGen

--  print $ Selector.select $! evaluate $! mutate randomGen $! population 0 randomGen
