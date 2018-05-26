module App where

import           Polyomino

genFn :: String -> (Int -> [Polyomino])
genFn s = case s of
            "fixed"     -> genFixed
            "one-sided" -> genOneSided
            _           -> const []

main :: IO ()
main = do
  putStr "Generate polyominos of length: "
  n <- getLine
  putStr "Polyominos of type {fixed, one-sided}: "
  freedom <- getLine
  putStr $ unlines $ map show $ genFn freedom (read n :: Int)
