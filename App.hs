module App where

import           Polyomino

genFn :: String -> (Int -> [Polyomino])
genFn s = case s of
            "free"      -> genFree
            "one-sided" -> genOneSided
            "fixed"     -> genFixed
            _           -> const []

main :: IO ()
main = do
  putStr "Generate polyominoes of length: "
  n <- getLine
  putStr "Polyominoes of type {free, one-sided, fixed}: "
  freedom <- getLine
  putStr $ unlines $ map show $ genFn freedom (read n :: Int)
