
import qualified Initiators.Basic as Initiator
import qualified Evaluators.Basic as Evaluator
import qualified Operators.Basic as Operators
import qualified Selectors.Basic as Selector

import System.Random

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

-- main loop
population :: (RandomGen g) => Int -> g -> (g, [[Char]])
population 0 gen = mutate' gen $ take 30 $ Initiator.start_population gen
population n gen =
  let (gen', xs) = population (n-1) gen
  in (gen', map (snd) $! Selector.select $ evaluate $ xs)

main = do
  randomGen <- newStdGen

  print $ Operators.breed "Hello World" "This is silly" randomGen

--  print $ evaluate $ snd $ population 0 randomGen
--  print $ evaluate $ snd $ population 1 randomGen
--  print $ evaluate $ snd $ population 2 randomGen
--  print $ evaluate $ snd $ population 3 randomGen
--  print $ evaluate $ snd $ population 4 randomGen

--  print $ mutate' randomGen $ population 0 randomGen

--  print $ Operator.mutate "Hello World" randomGen

--  print $ Selector.select $! evaluate $! mutate randomGen $! population 0 randomGen
