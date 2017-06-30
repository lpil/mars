module Main where

import Control.Exploration (runInstructionSets)
import Data.Instruction (parse)
import Data.Robot (x, y)
import System.IO.Strict as Strict

main :: IO ()
main = Strict.getContents >>= putStr . explore . parse
  where
    explore Nothing = "Error: Invalid input"
    explore (Just a) = unlines . map report $ uncurry runInstructionSets a
    report (Left robot) = report (Right robot) ++ " LOST"
    report (Right robot) = show (x robot) ++ " " ++ show (y robot)
