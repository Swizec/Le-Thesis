
module Initiators.MarkovChain (
  start_population
  ) where

import System.Random
import Data.HashMap
import Data.List.Split as Split
import Data.Char

import Config

-- read corpus data
-- build markov chain
-- spit out data
start_population::(RandomGen g) => g -> String -> IO String
start_population gen start = do
  return . (foldr detokenize "") . (take Config.seed_length) . (produce gen start). chain . tokenize =<< readFile Config.seed_data

tokenize::String -> [String]
tokenize s = Prelude.filter (\x -> x /= " " && x /= "") $
             Split.split (whenElt
                          (\x -> isSeparator x || isPunctuation x || x == '\n')) $
             Prelude.map toLower s

detokenize::String -> String -> String
detokenize a b
  | punctuation a || punctuation b = a++b
  | otherwise = a++" "++b
  where punctuation = (\x -> length x > 0 && isPunctuation (x!!0))

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
produce gen s map =
  let (gen', next) = next_token gen map s
  in s:(produce gen' next map)
