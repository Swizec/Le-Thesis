
import qualified Initiators.Basic as Initiator
import qualified Evaluators.Basic as Evaluator
import qualified Operators.Basic as Operators
import qualified Selectors.Basic as Selector

import System.Random
import Data.List

evaluate :: (Num a) => [[Char]] -> [(a, [Char])]
evaluate xs = map Evaluator.evaluate xs

mutate :: (RandomGen g) => (g, [[Char]]) -> (g, [[Char]])
mutate (gen, xs) = mapAccumL Operators.mutate gen xs

breed::(RandomGen g, Num a, Ord a) => g -> [(a, [Char])] -> (g, [[Char]])
breed gen xs = Operators.breed gen $ map (snd) $ xs

-- main loop
population :: (RandomGen g) => Int -> g -> (g, [[Char]])
population 0 gen = mutate (gen, take 30 $ Initiator.start_population gen)
population n gen =
  let pop = population (n-1) gen
  in step pop

-- if solution found, just escalate current population
-- otherwise do an evolution step
step::(RandomGen g) => (g, [[Char]]) -> (g, [[Char]])
step pop
  | best == 0 = pop
  | otherwise = let (gen', xs) = mutate pop
                    (gen'', bred) = breed gen' $ evaluate xs
                in (gen'', (map (snd)) . Selector.select $ evaluate bred)
  where best = fst . Selector.best $ evaluate (snd pop)

main = do
  randomGen <- newStdGen

--  print $ (take 10) . Selector.select . evaluate . snd $ population 20 randomGen
  let pop = Selector.order . evaluate . snd $ population 50 randomGen
    in print (length pop, head pop, last pop)
--  print $ fst $ minimumBy (\a b -> compare (fst a) (fst b)) $ evaluate . snd $ population 10 randomGen
