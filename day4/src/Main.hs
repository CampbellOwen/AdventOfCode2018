module Main where

import Data.List
import Data.List.Split

import qualified Data.Map.Strict as Map

type GuardEnv = Map.Map Int (Int, [Int])

data Record = Guard {iD :: Int, time :: Time}
            | Sleep {time :: Time}
            | Wake {time :: Time} deriving(Show)

type Time = (Int, Int, Int, Int, Int)


getEnv :: [Record] -> Int -> GuardEnv -> GuardEnv
getEnv [] _ env = env
getEnv ((Guard{iD = gId}):more) _ env = getEnv more gId env
getEnv ((Sleep{time = sTime}):(Wake{time = wTime}):more) currId env = case Map.lookup currId env of
  Just (accum, mins) -> getEnv more currId $ Map.insert currId (accum+sleepTime, mins ++ slept) env
  _ -> getEnv more currId $ Map.insert currId (sleepTime, slept) env

  where sleepTime = numMinutes wTime sTime
        (_, _, _, _, minutes) = sTime
        slept = [minutes..minutes+sleepTime-1]

sortTimes :: [Time] -> [Time]
sortTimes ts = sortBy compareTimes ts

compareTimes :: Time -> Time -> Ordering
compareTimes (y1, mo1, d1, h1, m1) (y2, mo2, d2, h2, m2)
  | y1 > y2 = GT
  | y1 < y2 = LT
  | mo1 > mo2 = GT
  | mo1 < mo2 = LT
  | d1 > d2 = GT
  | d1 < d2 = LT
  | h1 > h2 = GT
  | h1 < h2 = LT
  | m1 > m2 = GT
  | m1 < m2 = LT
  | otherwise = EQ

sortRecords :: [Record] -> [Record]
sortRecords rs = sortBy compareRecords rs

compareRecords r1 r2 = compareTimes (time r1) (time r2)

sleepiestGuard :: GuardEnv -> (Int, (Int, [Int]))
sleepiestGuard = head . reverse . sortBy (\f s -> compare (fst $ snd f) (fst $ snd s)) . Map.toList 

sleepiestMinute :: (Int, (Int, [Int])) -> Int
sleepiestMinute (_, (_, minutes)) = head $ last $ sortBy (\x y -> compare (length x) (length y)) $ group $ sort minutes

parse :: String -> Record
parse str = case head tokens of
  "wakes" -> Wake {time = timestamp}
  "falls" -> Sleep {time = timestamp}
  "Guard" -> Guard {iD = read $ tail (tokens !! 1), time = timestamp}
  where cleaned = filter clean str
        (year:month:rest) = splitOn ['-'] cleaned
        (day:time:tokens) = splitOn [' '] $ head rest
        [hour, minute] = splitOn [':'] time
        timestamp = (read year, read month, read day, read hour, read minute)

numMinutes :: Time -> Time -> Int
numMinutes (y1, mo1, d1, h1, m1) (y2, mo2, d2, h2, m2) = m1 - m2

clean :: Char -> Bool
clean c = c /= '[' && c /= ']'

consistentGuard :: GuardEnv -> (Int, (Int, [Int]))
consistentGuard = last . (sortBy comp) . Map.toList
  where numSame = length . last . sortBy (\x y -> compare (length x) (length y)) . group . sort
        comp (_, (_,t1)) (_, (_,t2)) = compare (numSame t1) (numSame t2)


main :: IO ()
main = do
  input <- readFile "input"
  let records = sortRecords $ map parse $ lines input
      env = getEnv records 0 Map.empty
      guard = sleepiestGuard env
      minute = sleepiestMinute guard
  putStrLn ((show guard) ++ (show minute))
