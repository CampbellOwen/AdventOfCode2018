module Main where

import Data.Char

type State = Bool

sameType :: Char -> Char -> Bool
sameType c c1
  | c == c1 = True
  | c == toLower c1 = True
  | c == toUpper c1 = True
  | otherwise = False

samePolarity :: Char -> Char -> Bool
samePolarity c c1 = ((isUpper c) && (isUpper c1)) || ((isLower c) && (isLower c1))

pass :: String -> Int -> String
pass s i 
  | i > (length s) - 2 = s
  | i < 0 = pass s 0 
  | otherwise = if sameType c c1 && (not $ samePolarity c c1 )
                then pass (left ++ rest) (i-1)
                else pass s (i+1)
    where (left, (c:c1:rest)) = splitAt i s
          right = (c:c1:rest)

pairs :: [(Char, Char)]
pairs = zip lowers $ map toUpper lowers
    where lowers = ['a'..'z']

main :: IO ()
main = do
  input <- readFile "input"
  putStrLn $ show $ length $ pass input 0
  let inputs = map (\(c, c1) -> filter (\c' -> c' /= c && c' /= c1) input) pairs
      results = map (length . (flip pass 0)) inputs
  putStrLn $ show $ minimum results 
