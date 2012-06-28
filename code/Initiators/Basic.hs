
module Initiators.Basic (
  start_population
  ) where

import System.Random

start_population :: (RandomGen g) => g -> [[Char]]
start_population gen =
  [take 50 $ randomRs ('a', 'z') gen | x <- [0..]]
