module Main where

import qualified Data.Map.Strict as Map
import Data.List

type Rectangle = (String, Int, Int, Int, Int)

type Coord = (Int, Int)

type CoverageMap = Map.Map Coord Int

rectangleToCoords :: Rectangle -> [Coord]
rectangleToCoords (_, x, y, w, h) = 
  [
    (x', y') 
    | x' <- [x..x+(w-1)] 
    , y' <- [y..y+(h-1)]
  ]

coveredSpots :: [Rectangle] -> CoverageMap -> CoverageMap
coveredSpots [] env = env
coveredSpots (r:more) env = coveredSpots more (foldl updateMap env coords) 
  where
    coords = rectangleToCoords r
    updateMap env coord = case Map.lookup coord env of
      Just x -> Map.insert coord (x+1) env
      _ -> Map.insert coord 1 env

overlapArea :: CoverageMap -> Int
overlapArea env = length $ filter (\(coord, num) -> num > 1) $ Map.toList env

readRectangle :: String -> Rectangle
readRectangle str = rect
  where id = takeWhile (/=' ') str
        s = drop 2 $ dropWhile (/='@') str
        x = read $ takeWhile (/=',') s
        y = read $ takeWhile (/=':') $ tail $ dropWhile (/=',') s
        w = read $ takeWhile (/='x') $ tail $ dropWhile (/=' ') s
        h = read $ reverse $ takeWhile (/='x') $ reverse str 
        rect = (id, x, y, w, h)


rectangleOverlaps :: CoverageMap -> Rectangle -> Bool
rectangleOverlaps env r@(id, _, _, _, _) = any (overlap env) coords
  where coords = rectangleToCoords r
        overlap env coord = case Map.lookup coord env of
          Just 1 -> False
          Nothing -> False
          Just x -> True


main :: IO ()
main = do
  input <- readFile "input"
  let rectangles = map readRectangle $ lines input
      coveragemap = coveredSpots rectangles Map.empty
  putStrLn $ show $ overlapArea $ coveragemap
  mapM_ (putStrLn . show) $ filter (not . rectangleOverlaps coveragemap) rectangles
