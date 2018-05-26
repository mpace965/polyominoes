module Polyomino (Polyomino, genFixed, genOneSided) where

import           Data.List

type Cell = (Int, Int)
type Shape = [Cell]

data Freedom = OneSided | Fixed
data Polyomino = Polyomino Freedom Int Shape

instance Show Polyomino where
  show (Polyomino _ n s) = unlines [line r | r <- [n,n-1..1]]
    where line r   = concat [star r c | c <- [1..n]]
          star r c = if (r - 1, c - 1) `elem` s then "■ " else "□ "

instance Eq Polyomino where
  (==) (Polyomino OneSided n s) other@(Polyomino OneSided n' s') =
    n == n' && (s == s' || s == other90 || s == other180 || s == other270)
    where (Polyomino _ _ other90)  = rotate90 other
          (Polyomino _ _ other180) = rotate180 other
          (Polyomino _ _ other270) = rotate270 other
  (==) (Polyomino Fixed n s) (Polyomino Fixed n' s') = n == n' && s == s'
  (==) _ _ = False

-- Constructing Polyominos

genOneSided :: Int -> [Polyomino]
genOneSided = nub . map (\(Polyomino Fixed n s) -> Polyomino OneSided n s) . genFixed

genFixed :: Int -> [Polyomino]
genFixed n
  | n <= 0    = []
  | n == 1    = [Polyomino Fixed 1 [(0,0)]]
  | otherwise = nub $ filterSmall buildFromSmall
    where filterSmall    = filter (\(Polyomino Fixed _ s) -> length s == n)
          buildFromSmall = concatMap (\(Polyomino Fixed i s) -> map (Polyomino Fixed $ i + 1) (addCells s)) (genFixed $ n - 1)

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

rotateCell :: (Int, Int, Int, Int) -> (Int, Int) -> (Int, Int)
rotateCell (w, x, y, z) (a, b) = (w * a + x * b, y * a + z * b)

rotate :: (Int, Int, Int, Int) -> Polyomino -> Polyomino
rotate r (Polyomino OneSided n s) = Polyomino OneSided n $ translateOrigin $ map (rotateCell r) s
rotate _ p = p

rotate90 :: Polyomino -> Polyomino
rotate90 = rotate (0, -1, 1, 0)

rotate180 :: Polyomino -> Polyomino
rotate180 = rotate (-1, 0, 0, -1)

rotate270 :: Polyomino -> Polyomino
rotate270 = rotate (0, 1, -1, 0)
