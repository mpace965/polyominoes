module Polyomino (Polyomino, genFixed, genOneSided) where

import           Data.List
import qualified Data.Set  as Set

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

wrap :: Set.Set Shape -> Set.Set Polyomino
wrap = Set.map (\s -> Polyomino { shape = s})

genFixed :: Int -> Set.Set Polyomino
genFixed = wrap . genFixedShapes

genOneSided :: Int -> Set.Set Polyomino
genOneSided = wrap . genOneSidedShapes

genOneSidedShapes :: Int -> Set.Set Shape
genOneSidedShapes = Set.fromList . nubBy oneSidedEq . Set.toList . genFixedShapes
  where oneSidedEq a b = a == rotate90 b || a == rotate180 b || a == rotate270 b

genFixedShapes :: Int -> Set.Set Shape
genFixedShapes n
  | n <= 0    = Set.empty
  | n == 1    = Set.singleton $ shapeFromList [(0,0)]
  | otherwise = filterSmall buildFromSmall
    where filterSmall    = Set.filter (\s -> Set.size s == n)
          buildFromSmall = Set.foldr (\s ss -> Set.union ss $ addCells s) Set.empty (genFixedShapes $ n - 1)

addCells :: Shape -> Set.Set Shape
addCells s = foldr insertAll Set.empty s
  where insertAll = Set.union . newShapes
        newShapes c = Set.fromList $ map (\cellFn -> translateOrigin $ cellFn c s) [addRightCell, addLeftCell, addUpCell, addDownCell]

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
translateOrigin s = translate (-mx, -my) s
  where (mx, my) = foldr1 (\(rx, ry) (sx, sy) -> (min sx rx, min sy ry)) s

translate :: (Int, Int) -> Shape -> Shape
translate (x,y) = Set.map (\(a,b) -> (a + x, y + b))

rotate :: (Int, Int, Int, Int) -> Shape -> Shape
rotate r = translateOrigin . Set.map (rotateCell r)

rotate90 :: Shape -> Shape
rotate90 = translateOrigin . Set.map rotateCell90

rotate180 :: Shape -> Shape
rotate180 = translateOrigin . Set.map rotateCell180

rotate270 :: Shape -> Shape
rotate270 = translateOrigin . Set.map rotateCell270

-- Moving Points

rotateCell :: (Int, Int, Int, Int) -> (Int, Int) -> (Int, Int)
rotateCell (w, x, y, z) (a, b) = (w * a + x * b, y * a + z * b)

rotateCell90 :: (Int, Int) -> (Int, Int)
rotateCell90 = rotateCell (0, -1, 1, 0)

rotateCell180 :: (Int, Int) -> (Int, Int)
rotateCell180 = rotateCell (-1, 0, 0, -1)

rotateCell270 :: (Int, Int) -> (Int, Int)
rotateCell270 = rotateCell (0, 1, -1, 0)
