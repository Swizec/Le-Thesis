

module Initiators.MarkovChain (
  start_population
  ) where

import System.Random
import Data.HashMap

import Config

--start_population::(RandomGen g) => g -> [[Char]]
start_population gen = do
  return . chain .tokenize =<< corpus

corpus = readFile "data/ring-o-rosies.txt"

tokenize::String -> [String]
tokenize s = words s

chain::[String] -> Map String [String]
chain [now, last] =
  insert now [last] $ singleton last []
chain (token:xs) =
  insertWith (\new old -> new++old) token [xs!!0] $ chain xs

-- read corpus data
-- build markov chain
-- spit out data
