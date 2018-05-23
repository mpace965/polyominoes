module Polyomino (Polyomino, genFixed, printP) where

import qualified Data.Set as Set

type Cell = (Int, Int)
type Polyomino = Set.Set Cell

-- Print

printP :: Polyomino -> String
printP p = unlines [line r | r <- [n,n-1..0]]
  where n        = Set.size p - 1
        line r   = [star r c | c <- [0..n]]
        star r c = if Set.member (r,c) p then '*' else ' '

-- Constructing Polyominos

fromList :: [Cell] -> Polyomino
fromList = translateOrigin . Set.fromList

genFixed :: Int -> Set.Set Polyomino
genFixed n
  | n <= 0    = Set.empty
  | n == 1    = Set.singleton $ fromList [(0,0)]
  | otherwise = filterSmall buildFromSmall
    where filterSmall    = Set.filter (\s -> Set.size s == n)
          buildFromSmall = Set.foldr (\p ps -> Set.union ps (addCells p)) Set.empty (genFixed $ n - 1)

addCells :: Polyomino -> Set.Set Polyomino
addCells p = foldr insertAll Set.empty p
  where insertAll c = insertCell addRightCell c .
                      insertCell addLeftCell c  .
                      insertCell addDownCell c  .
                      insertCell addUpCell c
        insertCell cellFn c = Set.insert (translateOrigin $ cellFn c p)

addUpCell :: Cell -> Polyomino -> Polyomino
addUpCell (x,y) = Set.insert (x, y + 1)

addDownCell :: Cell -> Polyomino -> Polyomino
addDownCell (x,y) = Set.insert (x, y - 1)

addLeftCell :: Cell -> Polyomino -> Polyomino
addLeftCell (x,y) = Set.insert (x - 1, y)

addRightCell :: Cell -> Polyomino -> Polyomino
addRightCell (x,y) = Set.insert (x + 1, y)

-- Moving Polyominos

translateOrigin :: Polyomino -> Polyomino
translateOrigin p = translate (-mx, -my) p
  where (mx, my) = foldr1 (\(rx, ry) (sx, sy) -> (min sx rx, min sy ry)) p

translate :: (Int, Int) -> Polyomino -> Polyomino
translate (x,y) = Set.map (\(a,b) -> (a + x, y + b))
