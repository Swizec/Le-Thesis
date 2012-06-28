
module Evaluators.Basic (
  evaluate
) where


evaluate::(Num a) => [Char] -> (a, [Char])
evaluate s = (fromIntegral $ levenshtein s "Hello World", s)

-- calculate levenshtein distance between two strings
levenshtein::[Char] -> [Char] -> Int
levenshtein s1 s2
  | length s2 < length s1 = levenshtein s2 s1
levenshtein (c1:s1) (c2:s2)
  | c1 == c2 = levenshtein s1 s2
  | otherwise = 1+levenshtein s1 s2
levenshtein [] s2 = length s2
