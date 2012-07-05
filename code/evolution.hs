
import Initiators.Basic
import qualified Evaluators.Basic as Evaluator

import System.Random

population :: (RandomGen g) => g -> Int -> [[Char]]
population gen 0 = take 2 $ start_population gen

evaluate :: (Num a) => [[Char]] -> [(a, [Char])]
evaluate [x] = [Evaluator.evaluate x]
evaluate (x:xs) = (Evaluator.evaluate x):evaluate xs

main = do
  randomGen <- newStdGen

  print $ evaluate $ population randomGen 0
