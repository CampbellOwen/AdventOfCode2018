module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe

parse :: String -> Int
parse ('+':str) = read str
parse str = read str

-- sumWithState :: Int -> (Int, Map.Map Int Int) -> ((Int, Map.Map Int Int), Int)
-- sumWithState x (y, env)

-- sums :: [Int] -> [Int]
-- sums = foldl (\sums x-> (sums ++ [((last sums) + x )])) [0]

-- dup :: Ord a => [a] -> Maybe a
-- dup xs = dup' xs Set.empty
--   where dup' [] _ = Nothing
--         dup' (x:xs) s = if Set.member x s 
--                            then Just x
--                            else dup' xs (Set.insert x s)

-- solve = dup . sums . concat . replicate 50

-- For each elem - add to sum, add new sum to Map, repeat

type SumState = (Map.Map Int Int, Int)

solve :: [Int] -> SumState -> Int
solve (x:xs) (env, sum) =
  let sum' = x+sum
  in case Map.lookup sum' env of
    Just x -> if x < 1 then solve xs (Map.insert sum' (x+1) env, sum') else sum'
    _ -> solve xs (Map.insert sum' 1 env, sum')

main :: IO ()
main = do
  input <- readFile "input2"
  putStrLn $ show $ sum $ map (parse) $ lines input
  putStrLn $ show $ solve (cycle $ map (parse) $ lines input) (Map.fromList [(0,1)], 0)
