
import qualified Initiators.Basic as Initiator
import qualified Evaluators.Basic as Evaluator
import qualified Operators.Basic as Operator
import qualified Selectors.Basic as Selector

import System.Random

population :: (RandomGen g) => Int -> g -> [[Char]]
population 0 gen = take 30 $ Initiator.start_population gen
population n gen = map (snd) $! Selector.select $! evaluate $! mutate gen $! population (n-1) gen

evaluate :: (Num a) => [[Char]] -> [(a, [Char])]
evaluate [x] = [Evaluator.evaluate x]
evaluate (x:xs) = (Evaluator.evaluate x):evaluate xs

--mutate::(RandomGen g, Num a) => g - > [(a, [Char])] -> [[Char]]
--mutate gen

mutate :: (RandomGen g) => g -> [[Char]] -> [[Char]]
mutate gen [x] = [fst $ Operator.mutate x gen]
mutate gen (x:xs) =
  let (x', gen') = Operator.mutate x gen
  in x':mutate gen' xs

main = do
  randomGen <- newStdGen

  print $ evaluate $! population 3 randomGen

  --print $ Operator.mutate "Hello World" randomGen

  --print $ Selector.select $! evaluate $! mutate randomGen $! population 0 randomGen
