{-|
Module: Polyomino
Description: Functionality for generating polyominos.
-}

module Polyomino (Polyomino, genFree, genOneSided, genFixed) where

import           Data.List

--------------------------------------------------------------------------------

-- | The coordinate in a matrix of cells
type Cell = (Int, Int)

-- | Shape is a list of cells which are a list of the coordinates that make up the polyomino
type Shape = [Cell]

-- | The Freedom types
data Freedom = Free | OneSided | Fixed

-- | Polyomino (Freedom, Size, Shape)
data Polyomino = Polyomino Freedom Int Shape

--------------------------------------------------------------------------------

instance Show Polyomino where
    show (Polyomino _ n s) = unlines [line r | r <- [n,n-1..1]]
      where
        line r = concat [block r c | c <- [1..n]]
        block r c = if (r - 1, c - 1) `elem` s then "██" else "░░"

instance Eq Polyomino where
    (==) p1@(Polyomino Free n1 _) p2@(Polyomino Free n2 _) = -- Free Polynomios
        n1 == n2 && ( -- when number of cells are the same
        isAnyShape p1 (rotationSet p2) || -- if p2 is a rotation of p1
        isAnyShape p1 (reflectionSet p2) || -- if p2 is a reflection of p1
        isAnyShape p1 (concatMap rotationSet (reflectionSet p2))) -- if p2 is a combination of reflections or rotations
    (==) p1@(Polyomino OneSided n1 _) p2@(Polyomino OneSided n2 _) = -- One-Sided Polyominos
        n1 == n2 && -- when number of cells are the same
        isAnyShape p1 (rotationSet p2) -- if p2 is a rotation of p1
    (==) (Polyomino Fixed n1 s1) (Polyomino Fixed n2 s2) = -- Fixed Polyominos
        n1 == n2 && -- when number of cells are the same
        s1 == s2 -- when shape is exactly the same
    (==) _ _ = False -- otherwise, they're not equal

--------------------------------------------------------------------------------

-- | Tests if the shapes of the polyominoes in the list are the same shape as the primary polyomino
isAnyShape :: Polyomino -> [Polyomino] -> Bool
isAnyShape (Polyomino _ _ s) = any (\(Polyomino _ _ s') -> s == s')

--------------------------------------------------------------------------------
-- GENERATION
--------------------------------------------------------------------------------

-- | Generate all the free polyominos of a given size
genFree :: Int -> [Polyomino]
genFree = nub . map (\(Polyomino Fixed n s) -> Polyomino Free n s) . genFixed

-- | Generate all the one-sided polyominos of a given size
genOneSided :: Int -> [Polyomino]
genOneSided = nub . map (\(Polyomino Fixed n s) -> Polyomino OneSided n s) . genFixed

-- | Generate all the fixed polyominos of a given size
genFixed :: Int -> [Polyomino]
genFixed n
    | n <= 0    = [] -- if size is 0, there are no polyominos
    | n == 1    = [Polyomino Fixed 1 [(0,0)]] -- if size is 1, there is only one polyomino
    | otherwise = nub $ concatMap addAndWrap (genFixed (n - 1))
      where
        addAndWrap (Polyomino Fixed i s) = map (Polyomino Fixed (i + 1)) (addCells s n)
        addAndWrap _ = []

addCells :: Shape -> Int -> [Shape]
addCells s n = map translateOrigin $ filter (\s' -> length s' == n) $ concatMap newShapes s
  where
    newShapes c = map (\cellFn -> nub $ cellFn c s) [addRightCell, addLeftCell, addUpCell, addDownCell]

addUpCell :: Cell -> Shape -> Shape
addUpCell (x,y) = (:) (x, y + 1)

addDownCell :: Cell -> Shape -> Shape
addDownCell (x,y) = (:) (x, y - 1)

addLeftCell :: Cell -> Shape -> Shape
addLeftCell (x,y) = (:) (x - 1, y)

addRightCell :: Cell -> Shape -> Shape
addRightCell (x,y) = (:) (x + 1, y)

--------------------------------------------------------------------------------
-- TRANSFORMING
--------------------------------------------------------------------------------

-- TRANSLATING

-- | Translates the cells so that all cells in the shape are >= to (0, 0)
translateOrigin :: Shape -> Shape
translateOrigin s = sort $ translate (-mx, -my) s
  where
    (mx, my) = foldr1 (\(rx, ry) (sx, sy) -> (min sx rx, min sy ry)) s

-- | Translates the shape by a certain vector
translate :: (Int, Int) -> Shape -> Shape
translate (x,y) = map (\(a,b) -> (a + x, y + b))

-- | 2x2 matrix multiplication with a 1x2 vector
multiply2x2 :: (Int, Int, Int, Int) -> (Int, Int) -> (Int, Int)
multiply2x2 (w, x, y, z) (a, b) = (w * a + x * b, y * a + z * b)

-- REFLECTIONS

-- | Generate a list of reflections for the polyomino
reflectionSet :: Polyomino -> [Polyomino]
reflectionSet p = [p, reflectX p, reflectY p, reflectXY p]

-- | Reflects the polyomino over the X axis
reflectX :: Polyomino -> Polyomino
reflectX = reflect (-1, 0, 0, 1)

-- | Reflects the polyomino over the Y axis
reflectY :: Polyomino -> Polyomino
reflectY = reflect (1, 0, 0, -1)

-- | Reflects the polyomino over the diagonal
reflectXY :: Polyomino -> Polyomino
reflectXY = reflect (-1, 0, 0, -1)

-- | Reflects the polyomino
reflect :: (Int, Int, Int, Int) -> Polyomino -> Polyomino
reflect r (Polyomino Free n s) = Polyomino Free n $ translateOrigin $ map (multiply2x2 r) s
reflect _ p = p

-- ROTATIONS

-- | Generate a list of rotations for the polyomino
rotationSet :: Polyomino -> [Polyomino]
rotationSet p = [p, rotate90 p, rotate180 p, rotate270 p]

-- | Rotates the polyomino by 90 degrees
rotate90 :: Polyomino -> Polyomino
rotate90 = rotate (0, -1, 1, 0)

-- | Rotates the polyomino by 180 degrees
rotate180 :: Polyomino -> Polyomino
rotate180 = rotate (-1, 0, 0, -1)

-- | Rotates the polyomino by 270 degrees
rotate270 :: Polyomino -> Polyomino
rotate270 = rotate (0, 1, -1, 0)

-- | Rotates the polyomino
rotate :: (Int, Int, Int, Int) -> Polyomino -> Polyomino
rotate r (Polyomino Free n s) =  Polyomino Free n $ translateOrigin $ map (multiply2x2 r) s
rotate r (Polyomino OneSided n s) = Polyomino OneSided n $ translateOrigin $ map (multiply2x2 r) s
rotate _ p = p
