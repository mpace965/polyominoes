{-|
Module: Main
Description: User Interaction
-}
module Main where

import System.IO

import Polyomino

--------------------------------------------------------------------------------

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- to make executable run properly
    putStr "Generate polyominoes of length: "
    n <- getLine
    putStr "Polyominoes of type {free, one-sided, fixed}: "
    freedom <- getLine
    putStr $ unlines $ map show $ genFn freedom (read n :: Int)

--------------------------------------------------------------------------------

-- | Gets the generation function from the string option describing it
genFn :: String -> (Int -> [Polyomino])
genFn s = case s of
            "free"      -> genFree
            "one-sided" -> genOneSided
            "fixed"     -> genFixed
            _           -> const []
