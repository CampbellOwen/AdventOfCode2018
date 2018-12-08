module Main where

import qualified Data.Map.Strict as Map
import Data.List
import Data.Maybe

data Coord = Coord { x:: Int, y :: Int} | None deriving(Show, Eq)

instance Ord Coord where
  c@Coord{x= x', y=y'} `compare` c'@Coord{x=x'', y=y''} 
    | c == c' = EQ
    | x' > x'' && y' > y'' = GT
    | otherwise = LT
  None `compare` None = EQ
  None `compare` _ = LT
  _ `compare` None = GT


isFinite :: [Coord] -> Coord -> Bool
isFinite _ None = False
-- isFinite coords pt = (any greaterX coords) && (any greaterY coords) && (any smallerX coords) && (any smallerY coords)
--   where greaterX pt' = x pt' > x pt
--         greaterY pt' = y pt' > y pt
--         smallerX pt' = x pt' < x pt
--         smallerY pt' = y pt' < y pt
isFinite coords pt = (x pt > left) && (x pt < right) && (y pt < top) && (y pt > bottom)
  where left = leftBound coords
        right = rightBound coords
        top = upperBound coords
        bottom = lowerBound coords

finiteAreas :: [Coord] -> [Coord]
finiteAreas cs = filter (isFinite cs) cs

leftBound :: [Coord] -> Int
leftBound cs = x $ head $ sortBy (\c c' -> compare (x c) (x c')) cs

rightBound :: [Coord] -> Int
rightBound cs = x $ head $ reverse $ sortBy (\c c' -> compare (x c) (x c')) cs

upperBound :: [Coord] -> Int
upperBound cs = y $ head $ reverse $ sortBy (\c c' -> compare (y c) (y c')) cs

lowerBound :: [Coord] -> Int
lowerBound cs = y $ head $ sortBy (\c c' -> compare (y c) (y c')) cs

manhattan :: Coord -> Coord -> Int
manhattan c c' = abs (x c - x c') + abs (y c - y c')

closest :: [Coord] -> Coord -> Coord
closest cs c = case length closestList of
  1 -> fst $ head closestList
  _ -> None
  where closestList = head $ groupBy (\(_, x) (_, x') -> x == x') $ sortBy (\(_,x) (_,x') -> compare x x') $ map (\(c1, c2) -> (c1, manhattan c1 c2)) $ zip cs $  replicate (length cs) c

type DistMap = Map.Map Coord Coord

createMap :: [Coord] -> [Coord]
createMap cs = map (closest cs) locations
  where locations = [Coord a b | a <- [(leftBound cs) .. (rightBound cs)], b <- [(lowerBound cs) .. (upperBound cs)]]

count :: [Coord] -> Int
-- count cs = last $ sequence $ (map length . group . sort . filter (isFinite cs))  $ createMap cs
count cs = last $ map length $ group $ sort $ filter (isFinite cs) $  createMap cs

parse :: [Char] -> Coord
parse s = Coord{x=a, y=b}
  where a = read $ takeWhile (/=',') s
        b = read $ tail $ dropWhile (/=' ') s


radiusPoints :: Coord -> Int -> [Coord]
radiusPoints c@(Coord _ _) 0 = [c]
radiusPoints (Coord x y) r =  [Coord a (y-r) | a <- xrange]
                           ++ [Coord a (y+r) | a <- xrange]
                           ++ [Coord (x-r) b | b <- yrange]
                           ++ [Coord (x+r) b | b <- yrange]
  where xrange = [x - r .. x + r]
        yrange = [y - r + 1 .. y + r - 1]

sizePatch :: [Coord] -> [Int] -> Coord -> Int
sizePatch coords rs c = length $ filter (==c) $ map (closest coords) $ concat $ map (radiusPoints c) rs

countSize :: [Coord] -> [Int] -> Coord -> Int
countSize _ [] _ = 0
countSize coords (r:rs) c = if len == 0 then 0 else len + countSize coords rs c
  where len = length $ filter (==c) $ map (closest coords) $ radiusPoints c r 

main :: IO ()
main = do
  input <- readFile "input"
  let coords = map parse $ lines input
      maxR = minimum [rightBound coords - leftBound coords, upperBound coords - lowerBound coords] `div` 2
      r = [0..maxR]
      
  putStrLn $ show $ maximum $ map (countSize coords r) $ filter (isFinite coords) coords
  
