
import qualified Initiators.Basic as Basic

import System.Random

population :: (RandomGen g) => g -> Int -> [[Char]]
population gen 0 = take 25 $ Basic.start_population gen

main = do
  randomGen <- newStdGen

  print $ population randomGen 0
