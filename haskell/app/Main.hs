module Main where

import Data.Char (isSpace)
import System.IO.Strict as Strict

main :: IO ()
main = Strict.getContents >>= putStr . dropWhile isSpace . reverse
