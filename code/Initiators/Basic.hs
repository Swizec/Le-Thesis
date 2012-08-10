
module Initiators.Basic (
  start_population
  ) where

import System.Random

start_population :: (RandomGen g) => g -> [[Char]]
start_population gen =
  let chars = " abcdefghijklmnopqrtstuvwxyzABCDEFGHIJKLMNOPQRTSUVWXYZ"
      ns = randomRs (0, length chars-1) gen
  in [take 50 $ drop (x*50) $ map (chars !!) ns | x <- [0..]]

