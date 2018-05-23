module Polyomino (Polyomino, genFixed) where

import qualified Data.Set as Set

type Cell = (Int, Int)
type Shape = Set.Set Cell

newtype Polyomino = Polyomino
  { shape       :: Shape
  } deriving (Eq, Ord)

-- Show

instance Show Polyomino where
  show (Polyomino s) = "\n" ++ unlines [line r | r <- [n,n-1..0]]
    where n        = Set.size s - 1
          line r   = [star r c | c <- [0..n]]
          star r c = if Set.member (r,c) s then '*' else ' '

-- Constructing Polyominos

shapeFromList :: [Cell] -> Shape
shapeFromList = translateOrigin . Set.fromList

genFixed :: Int -> Set.Set Polyomino
genFixed = Set.map wrap . genFixedShapes
  where wrap s = Polyomino { shape = s }

genFixedShapes :: Int -> Set.Set Shape
genFixedShapes n
  | n <= 0    = Set.empty
  | n == 1    = Set.singleton $ shapeFromList [(0,0)]
  | otherwise = filterSmall buildFromSmall
    where filterSmall    = Set.filter (\s -> Set.size s == n)
          buildFromSmall = Set.foldr (\s ss -> Set.union ss $ addCells s) Set.empty (genFixedShapes $ n - 1)

addCells :: Shape -> Set.Set Shape
addCells s = foldr insertAll Set.empty s
  where insertAll c = insertCell addRightCell c .
                      insertCell addLeftCell c  .
                      insertCell addDownCell c  .
                      insertCell addUpCell c
        insertCell cellFn c = Set.insert $ translateOrigin $ cellFn c s

addUpCell :: Cell -> Shape -> Shape
addUpCell (x,y) = Set.insert (x, y + 1)

addDownCell :: Cell -> Shape -> Shape
addDownCell (x,y) = Set.insert (x, y - 1)

addLeftCell :: Cell -> Shape -> Shape
addLeftCell (x,y) = Set.insert (x - 1, y)

addRightCell :: Cell -> Shape -> Shape
addRightCell (x,y) = Set.insert (x + 1, y)

-- Moving Polyominos

translateOrigin :: Shape -> Shape
translateOrigin p = translate (-mx, -my) p
  where (mx, my) = foldr1 (\(rx, ry) (sx, sy) -> (min sx rx, min sy ry)) p

translate :: (Int, Int) -> Shape -> Shape
translate (x,y) = Set.map (\(a,b) -> (a + x, y + b))
