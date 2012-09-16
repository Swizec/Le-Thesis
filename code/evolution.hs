
import qualified Initiators.Basic as Initiator
import qualified Initiators.MarkovChain as MCInitiator
import qualified Evaluators.Basic as Evaluator
import qualified Operators.Basic as Operators
import qualified Selectors.Basic as Selector
import Config

import System.Random
import Data.List

evaluate :: (Num a) => [[Char]] -> [(a, [Char])]
evaluate xs = map Evaluator.evaluate xs

breed::(RandomGen g, Num a, Ord a) => g -> [(a, [Char])] -> (g, [[Char]])
breed gen xs = Operators.breed gen $ map (snd) $ xs

-- main loop
population :: (RandomGen g) => Int -> g -> (g, [[Char]])
population 0 gen = Operators.mutate (gen, take Config.initial_population $ Initiator.start_population gen)
population n gen =
  let pop = population (n-1) gen
  in step pop

-- if solution found, just escalate current population
-- otherwise do an evolution step
step::(RandomGen g) => (g, [[Char]]) -> (g, [[Char]])
step pop
  | best == 0 = pop
  | otherwise = let (gen', xs) = Operators.mutate pop
                    (gen'', bred) = breed gen' $ evaluate xs
                in (gen'', (map (snd)) . Selector.select $ evaluate bred)
  where best = fst . Selector.best $ evaluate (snd pop)

main = do
  randomGen <- newStdGen

  bla <- MCInitiator.start_population randomGen "a"
  print $ take 15 bla
--  let pop = Selector.order . evaluate . snd $ population Config.max_epochs randomGen
--    in print (take 20 pop, last pop)
