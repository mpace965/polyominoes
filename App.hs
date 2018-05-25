module App where

import           Polyomino

-- TODO Once Polyomino typeclasses are implemented, allow for selection of
--      polynominos to generate

main :: IO ()
main = do
  putStr "Generate polynominos of length: "
  n <- getLine
  putStr $ unlines $ map show $ genOneSided (read n :: Int)
