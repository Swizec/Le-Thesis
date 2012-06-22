
module Initiators.Basic (
  start_population
  ) where

import System.Random

type Rand a = StdGen -> (a, StdGen)

start_population n =
  [x | x <- [1..n]]
