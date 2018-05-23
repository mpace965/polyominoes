module App where

import qualified Data.Set  as Set
import           Polyomino

main :: IO()
main = interact $ show . (\[x] -> Set.toList (genFixed (read x :: Int))) . lines
