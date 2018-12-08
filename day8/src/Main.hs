module Main where

type Metadata = Int

data Tree a = Tree [Tree a] a | Leaf a deriving(Show)

-- instance Foldable Tree a where
--   foldMap f (Tree [] meta) = foldr meta
--   foldMap f (Tree sub meta) = Tree (foldr $ 

-- parse :: [String] -> Tree [Metadata]
-- parse (nchild:nmeta:rest) 
--   | nchild == 0 = Leaf $ take nmeta rest 
--   | otherwise = Tree (parseN nchild rest) $ take nmeta
--   where parseN 0 r = ([], r)
--         parseN n r = let (cs, r') = parseN (n-1) r
--                       in ()

parse :: [String] -> (Tree [Metadata], [String])
parse (nchild:nmeta:rest) 
  | numChild == 0 = (Leaf (map read $ take (numMeta) rest), drop (numMeta) rest)
  | otherwise = (Tree children (map read $ take numMeta more), drop numMeta more )
   where numMeta = read nmeta
         numChild = read nchild
         (children, more) = parseN numChild rest

createTree :: [String] -> Tree [Metadata]
createTree s = fst $ parse s

getMetaSum :: Tree [Metadata] -> Int
getMetaSum (Leaf meta) = sum meta
getMetaSum (Tree children meta) = (sum $ map getMetaSum children) + (sum meta)

parseN 0 rs = ([], rs)
parseN n rs = let (tree, rs') = parse rs
                  (trees, rs'') = parseN (n-1) rs'
              in (tree:trees, rs'')

part2Sum :: Tree [Metadata] -> Int
part2Sum (Leaf meta) = sum meta
part2Sum t@(Tree children meta) = sum $ map part2Sum $ map ((!!) children) $ filter (validIndex t) $ map ((+) (-1)) meta 

validIndex :: Tree [Metadata] -> Int -> Bool
validIndex (Leaf _) _ = False
validIndex (Tree children _) i 
  | i >= 0 && i < length children = True
  | otherwise = False

main :: IO ()
main = do
  input <- readFile "input"
  let t = createTree $ words input
  putStrLn $ show $ getMetaSum t
  putStrLn $ show $ part2Sum t
