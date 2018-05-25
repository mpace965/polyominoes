module App where

import qualified Data.Set  as Set
import           Polyomino

gen :: (Int -> Set.Set Polyomino) -> String -> [Polyomino]
gen polyFn n = Set.toList $ polyFn (read n :: Int)

main :: IO ()
main = interact $ unlines . map show . gen genFixed . head . lines
