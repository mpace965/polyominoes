module Polyomino (Polyomino, genFixed, genOneSided) where

import           Data.List

type Cell = (Int, Int)
type Shape = [Cell]

data Polyomino = Polyomino Int Shape
  deriving (Eq, Ord)

-- Show

instance Show Polyomino where
  show (Polyomino n s) = unlines [line r | r <- [n,n-1..1]]
    where line r   = concat [star r c | c <- [1..n]]
          star r c = if (r - 1, c - 1) `elem` s then "■ " else "□ "

-- Constructing Polyominos

genOneSided :: Int -> [Polyomino]
genOneSided = nubBy oneSidedEq . genFixed
  where oneSidedEq (Polyomino _ s) (Polyomino _ s') = s == rotate90 s' || s == rotate180 s' || s == rotate270 s'

genFixed :: Int -> [Polyomino]
genFixed n
  | n <= 0    = []
  | n == 1    = [Polyomino 1 [(0,0)]]
  | otherwise = nub $ filterSmall buildFromSmall
    where filterSmall    = filter (\(Polyomino _ s) -> length s == n)
          buildFromSmall = concatMap (\(Polyomino i s) -> map (Polyomino $ i + 1) (addCells s)) (genFixed $ n - 1)

addCells :: Shape -> [Shape]
addCells s = map translateOrigin $ concatMap newShapes s
  where newShapes c = map (\cellFn -> nub $ cellFn c s) [addRightCell, addLeftCell, addUpCell, addDownCell]

addUpCell :: Cell -> Shape -> Shape
addUpCell (x,y) = (:) (x, y + 1)

addDownCell :: Cell -> Shape -> Shape
addDownCell (x,y) = (:) (x, y - 1)

addLeftCell :: Cell -> Shape -> Shape
addLeftCell (x,y) = (:) (x - 1, y)

addRightCell :: Cell -> Shape -> Shape
addRightCell (x,y) = (:) (x + 1, y)

-- Moving Polyominos

translateOrigin :: Shape -> Shape
translateOrigin s = sort $ translate (-mx, -my) s
  where (mx, my) = foldr1 (\(rx, ry) (sx, sy) -> (min sx rx, min sy ry)) s

translate :: (Int, Int) -> Shape -> Shape
translate (x,y) = map (\(a,b) -> (a + x, y + b))

rotate :: (Int, Int, Int, Int) -> Shape -> Shape
rotate r = translateOrigin . map (rotateCell r)

rotate90 :: Shape -> Shape
rotate90 = translateOrigin . map rotateCell90

rotate180 :: Shape -> Shape
rotate180 = translateOrigin . map rotateCell180

rotate270 :: Shape -> Shape
rotate270 = translateOrigin . map rotateCell270

-- Moving Points

rotateCell :: (Int, Int, Int, Int) -> (Int, Int) -> (Int, Int)
rotateCell (w, x, y, z) (a, b) = (w * a + x * b, y * a + z * b)

rotateCell90 :: (Int, Int) -> (Int, Int)
rotateCell90 = rotateCell (0, -1, 1, 0)

rotateCell180 :: (Int, Int) -> (Int, Int)
rotateCell180 = rotateCell (-1, 0, 0, -1)

rotateCell270 :: (Int, Int) -> (Int, Int)
rotateCell270 = rotateCell (0, 1, -1, 0)
