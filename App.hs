module App where

import qualified Data.Set  as Set
import           Polyomino

main :: IO()
main = interact $ (\[x] -> unlines $ map printP $ Set.toList (genFixed (read x :: Int))) . lines
