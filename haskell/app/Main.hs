module Main where

import Data.Char (toUpper)
import System.IO.Strict as Strict

main :: IO ()
main = Strict.getContents >>= putStr . map toUpper
