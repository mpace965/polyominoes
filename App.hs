module App where

import           Polyomino

main :: IO ()
main = interact $ unlines . map show . genOneSided . (\x -> read x :: Int) . head . lines
