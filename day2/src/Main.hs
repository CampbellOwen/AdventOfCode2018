module Main where

import qualified Data.Map.Strict as Map

hasExactlyTwoThreeSame :: String -> (Bool, Bool)
-- hasExactlyTwoThreeSame str = (1 == (length $ filter (\((c, x)  :: (Char, Int)) -> x == 2) env), 1 == (length $ filter (\(c, x) -> x == 3)))
hasExactlyTwoThreeSame str = (twos > 0, threes > 0)
  where env = (Map.toList $ countChars str Map.empty )
        twos = length $ filter (\(_, x) -> x == 2) env
        threes = length $ filter (\(_, x) -> x == 3) env


countChars :: String -> Map.Map Char Int -> Map.Map Char Int
countChars [] env = env
countChars (c:more) env = case Map.lookup c env of
  Just x -> countChars more (Map.insert c (x+1) env)
  _ -> countChars more (Map.insert c 1 env)

pairs :: [String] -> [(String, String)]
pairs strs = [(strs !! i, strs !! j) | i <- [0..len-2], j <- [1..len-1], i < j ]
  where len = length strs

numDiffs :: String -> String -> Int
numDiffs _ [] = 0
numDiffs [] _ = 0
numDiffs (c1:s1) (c2:s2) 
  | c1 == c2 = numDiffs s1 s2
  | c1 /= c2 = 1 + (numDiffs s1 s2)

solve1 :: [String] -> Int
solve1 ids = two * three
  where twothree = map hasExactlyTwoThreeSame ids
        two = length $ filter (\(two, three) -> two) twothree
        three = length $ filter(\(two, three) -> three) twothree

solve2 :: [String] -> [(String, String)]
solve2 ids = filter (\(s1, s2) -> (numDiffs s1 s2) == 1) $ pairs ids

main :: IO ()
main = do
  input <- readFile "input"
  let ids = lines input
  -- putStrLn $ show $ solve1 ids
  putStrLn $ show $ solve2 ids