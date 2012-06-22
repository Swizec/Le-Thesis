
import qualified Initiators.Basic as Basic

import System.Random

population :: Int -> [a] -> [Int]
population 0 _ = Basic.start_population 5

randomStuff g = randomRs ('a', 'z') g

-- work (r:rs) =
--  let n = truncate r + 1
--      (xs, ys) = splitAt n rs
--  in xs : work ys

main = do
  print $ population 0 []
  print $ take 5 $ randomStuff (mkStdGen 42)
