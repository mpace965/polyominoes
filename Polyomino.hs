module Polyomino (Polyomino, genFree, genOneSided, genFixed) where

import           Data.List

type Cell = (Int, Int)
type Shape = [Cell]

data Freedom = Free | OneSided | Fixed
data Polyomino = Polyomino Freedom Int Shape

instance Show Polyomino where
  show (Polyomino _ n s) = unlines [line r | r <- [n,n-1..1]]
    where line r   = concat [star r c | c <- [1..n]]
          star r c = if (r - 1, c - 1) `elem` s then "■ " else "□ "

isAnyShape :: Polyomino -> [Polyomino] -> Bool
isAnyShape (Polyomino _ _ s) = any (\(Polyomino _ _ s') -> s == s')

instance Eq Polyomino where
  (==) p@(Polyomino Free n _) other@(Polyomino Free n' _) =
    n == n' && (isAnyShape p (rotationSet other) ||
                isAnyShape p (reflectionSet other) ||
                isAnyShape p (concatMap rotationSet (reflectionSet other))) -- TODO debug wrong numbers at n >= 7
  (==) p@(Polyomino OneSided n _) other@(Polyomino OneSided n' _) = n == n' && isAnyShape p (rotationSet other)
  (==) (Polyomino Fixed n s) (Polyomino Fixed n' s') = n == n' && s == s'
  (==) _ _ = False

-- Constructing Polyominoes

genFree :: Int -> [Polyomino]
genFree = nub . map (\(Polyomino Fixed n s) -> Polyomino Free n s) . genFixed

genOneSided :: Int -> [Polyomino]
genOneSided = nub . map (\(Polyomino Fixed n s) -> Polyomino OneSided n s) . genFixed

genFixed :: Int -> [Polyomino]
genFixed n
  | n <= 0    = []
  | n == 1    = [Polyomino Fixed 1 [(0,0)]]
  | otherwise = nub $ concatMap (\(Polyomino Fixed i s) -> map (Polyomino Fixed $ i + 1) (addCells s n)) (genFixed $ n - 1)

addCells :: Shape -> Int -> [Shape]
addCells s n = map translateOrigin $ filter (\s' -> length s' == n) $ concatMap newShapes s
  where newShapes c = map (\cellFn -> nub $ cellFn c s) [addRightCell, addLeftCell, addUpCell, addDownCell]

addUpCell :: Cell -> Shape -> Shape
addUpCell (x,y) = (:) (x, y + 1)

addDownCell :: Cell -> Shape -> Shape
addDownCell (x,y) = (:) (x, y - 1)

addLeftCell :: Cell -> Shape -> Shape
addLeftCell (x,y) = (:) (x - 1, y)

addRightCell :: Cell -> Shape -> Shape
addRightCell (x,y) = (:) (x + 1, y)

-- Moving Polyominoes

translateOrigin :: Shape -> Shape
translateOrigin s = sort $ translate (-mx, -my) s
  where (mx, my) = foldr1 (\(rx, ry) (sx, sy) -> (min sx rx, min sy ry)) s

translate :: (Int, Int) -> Shape -> Shape
translate (x,y) = map (\(a,b) -> (a + x, y + b))

multiply2x2 :: (Int, Int, Int, Int) -> (Int, Int) -> (Int, Int)
multiply2x2 (w, x, y, z) (a, b) = (w * a + x * b, y * a + z * b)

reflect :: (Int, Int, Int, Int) -> Polyomino -> Polyomino
reflect r (Polyomino Free n s) = Polyomino Free n $ translateOrigin $ map (multiply2x2 r) s
reflect _ p = p

reflectX :: Polyomino -> Polyomino
reflectX = reflect (-1, 0, 0, 1)

reflectY :: Polyomino -> Polyomino
reflectY = reflect (1, 0, 0, -1)

reflectXY :: Polyomino -> Polyomino
reflectXY = reflect (-1, 0, 0, -1)

reflectionSet :: Polyomino -> [Polyomino]
reflectionSet p = [p, reflectX p, reflectY p, reflectXY p]

rotate :: (Int, Int, Int, Int) -> Polyomino -> Polyomino
rotate r (Polyomino Free n s) =  Polyomino Free n $ translateOrigin $ map (multiply2x2 r) s
rotate r (Polyomino OneSided n s) = Polyomino OneSided n $ translateOrigin $ map (multiply2x2 r) s
rotate _ p = p

rotate90 :: Polyomino -> Polyomino
rotate90 = rotate (0, -1, 1, 0)

rotate180 :: Polyomino -> Polyomino
rotate180 = rotate (-1, 0, 0, -1)

rotate270 :: Polyomino -> Polyomino
rotate270 = rotate (0, 1, -1, 0)

rotationSet :: Polyomino -> [Polyomino]
rotationSet p = [p, rotate90 p, rotate180 p, rotate270 p]
