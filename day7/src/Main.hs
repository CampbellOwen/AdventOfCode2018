module Main where
import qualified Data.Set as Set
import Data.List
data Req = Req{ first :: Step, second :: Step} deriving(Show, Eq)

type Step = Char

dependency :: [Req] -> Step -> [Step]
dependency rs c = map (\(Req f _) -> f) $ filter req rs
  where req (Req _ s) = s == c

            --   Satisfied -> Remaining
possibleNext :: [Step] -> [Req] -> Step
-- possibleNext satisfied rest = map (\(Req f s) -> s) $ filter (\(Req f s) -> f `elem` satisfied) rest
possibleNext satisfied rest = head $ sort $ map fst $ filter (\(_,deps) -> (all (flip elem satisfied)) deps) dependencies 
  where next = map second rest
        dependencies = map (\c -> (c, dependency rest c)) next


performStep :: [Step] -> Step -> [Req] -> ([Step], [Req])
performStep ss s rs = ((s:ss), reqs)
  where reqs = filter (\(Req f s') -> s /= s') rs

type StepSet = Set.Set Step

getAllSteps :: [Req] -> StepSet -> StepSet
getAllSteps [] env = env
getAllSteps ((Req f s):rs) env = getAllSteps rs $ Set.insert s $ Set.insert f env

getFirstSteps :: [Step] -> [Req] -> [Step]
getFirstSteps [] _ = []
getFirstSteps (ss) rs = filter (not . (flip elem allWithDependency)) ss
  where allWithDependency = map second rs

solve1 :: [Req] -> [Char]
solve1 rs = solve [start] rs
  where allSteps = Set.toList $ getAllSteps rs Set.empty
        start = head $ sort $ getFirstSteps allSteps rs
        solve ss [] = ss
        solve ss rs' = solve ss' rs''
          where (ss', rs'') = performStep ss (possibleNext ss rs') rs'


parse :: [Char] -> Req
parse s = Req (s !! 5) (s !! 36)

main :: IO ()
main = do
  input <- readFile "input"
  let rs = map parse $ lines input

  putStrLn $ reverse $ solve1 rs