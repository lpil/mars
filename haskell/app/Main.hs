module Main where

main :: IO ()
main = putStrLn "Hello, Mars!"
-- step :: [Instruction] -> World -> Either World World
-- step [] world = Right world
-- step (instruction:rest) world =
--   execute instruction >>= step rest
