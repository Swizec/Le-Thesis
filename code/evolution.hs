
import qualified Initiators.Basic as Initiator
import qualified Evaluators.Basic as Evaluator
import qualified Operators.Basic as Operators
import qualified Selectors.Basic as Selector

import System.Random
import Data.List

evaluate :: (Num a) => [[Char]] -> [(a, [Char])]
evaluate [x] = [Evaluator.evaluate x]
evaluate (x:xs) = (Evaluator.evaluate x):evaluate xs

mutate :: (RandomGen g) => (g, [[Char]]) -> (g, [[Char]])
mutate (gen, xs) = mapAccumL Operators.mutate gen xs

breed::(RandomGen g, Num a, Ord a) => g -> [(a, [Char])] -> (g, [[Char]])
breed gen xs = Operators.breed gen $ map (snd) $ Selector.select xs

-- main loop
population :: (RandomGen g) => Int -> g -> (g, [[Char]])
population 0 gen = mutate (gen, take 30 $ Initiator.start_population gen)
population n gen =
  let (gen', xs) = mutate $ population (n-1) gen
  in breed gen' $! evaluate xs
--  in (gen'',  bred)

main = do
  randomGen <- newStdGen

  print $ (!!0) . Selector.select . evaluate . snd $ population 5 randomGen
