
import qualified Initiators.Basic as Initiator
import qualified Evaluators.Basic as Evaluator
import qualified Operators.Basic as Operator

import System.Random

population :: (RandomGen g) => Int -> g -> [[Char]]
population 0 gen = take 30 $ Initiator.start_population gen

evaluate :: (Num a) => [[Char]] -> [(a, [Char])]
evaluate [x] = [Evaluator.evaluate x]
evaluate (x:xs) = (Evaluator.evaluate x):evaluate xs

main = do
  randomGen <- newStdGen

  print $ Operator.mutate "Hello World" randomGen

--  print $ mutate randomGen $ evaluate $ population 0 randomGen
