
module Initiators.Basic (
  start_population
  ) where

import System.Random

start_population gen =
  [take 5 $ randomRs ('a', 'z') gen | x <- [0..]]
