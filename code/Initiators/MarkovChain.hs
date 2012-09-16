

module Initiators.MarkovChain (
  start_population
  ) where

import System.Random
import Data.HashMap
import Data.List.Split as Split
import Data.Char

import Config

--start_population::(RandomGen g) => g -> [[Char]]
start_population gen start = do
  return . (produce gen start). chain . tokenize =<< corpus
--    in return $ produce gen markov start

corpus = readFile "data/ring-o-rosies.txt"

tokenize::String -> [String]
tokenize s = Prelude.filter (\x -> x /= " " && x /= "") $
             Split.split (whenElt
                          (\x -> isSeparator x || isPunctuation x || x == '\n'))
             s

chain::[String] -> Map String [String]
chain [now, last] =
  insert now [last] $ singleton last []
chain (token:xs) =
  insertWith (\new old -> new++old) token [xs!!0] $ chain xs

next_token::(RandomGen g) => g -> Map String [String] -> String -> (g, String)
next_token gen map s =
  let choices = findWithDefault [] s map
      (i, gen') = randomR (0, length choices - 1) gen
  in (gen', choices!!i)

produce::(RandomGen g) => g -> String -> Map String [String] -> [String]
produce gen "" map = [""]
produce gen s map =
  let (gen', next) = next_token gen map s
  in next:(produce gen' next map)

-- read corpus data
-- build markov chain
-- spit out data
