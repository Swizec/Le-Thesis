
import qualified Initiators.Basic as Initiator
import qualified Evaluators.Basic as Evaluator
import qualified Operators.Basic as Operator

import System.Random

population :: (RandomGen g) => g -> Int -> [[Char]]
population gen 0 = take 30 $ Initiator.start_population gen

evaluate :: (Num a) => [[Char]] -> [(a, [Char])]
evaluate [x] = [Evaluator.evaluate x]
evaluate (x:xs) = (Evaluator.evaluate x):evaluate xs

main = do
  randomGen <- newStdGen

  print $ Operator.mutate randomGen "Hello World"

--  print $ mutate randomGen $ evaluate $ population randomGen 0
