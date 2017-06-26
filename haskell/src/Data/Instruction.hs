module Data.Instruction
  ( Instruction(..)
  , InstructionSet(..)
  , Bearing(..)
  , parse
  ) where

import Data.Char (isDigit)
import Data.List.Split (chunksOf)

data Bearing
  = North
  | East
  | South
  | West
  deriving (Eq, Show)

data Instruction
  = Advance
  | TurnLeft
  | TurnRight
  deriving (Eq, Show)

data InstructionSet = InstructionSet
  { startPosition :: (Integer, Integer)
  , startDirection :: Bearing
  , steps :: [Instruction]
  } deriving (Eq, Show)

parse :: String -> Maybe [InstructionSet]
parse = sequence . fmap parseChunk . chunksOf 2 . lines

parseChunk :: [String] -> Maybe InstructionSet
parseChunk [line1, line2] = do
  (p, d) <- parseLine1 line1
  s <- parseLine2 line2
  Just InstructionSet {startPosition = p, startDirection = d, steps = s}
parseChunk _ = Nothing

parseLine1 :: String -> Maybe ((Integer, Integer), Bearing)
parseLine1 [x, ' ', y, ' ', d] = do
  x' <- parsePos x
  y' <- parsePos y
  d' <- parseDir d
  Just ((x', y'), d')
parseLine1 _ = Nothing

parseDir :: Char -> Maybe Bearing
parseDir 'N' = Just North
parseDir 'E' = Just East
parseDir 'S' = Just South
parseDir 'W' = Just West
parseDir _ = Nothing

parsePos :: Char -> Maybe Integer
parsePos c
  | isDigit c = Just $ read [c]
  | otherwise = Nothing

parseLine2 :: String -> Maybe [Instruction]
parseLine2 = sequence . fmap toInstruction
  where
    toInstruction 'F' = Just Advance
    toInstruction 'L' = Just TurnLeft
    toInstruction 'R' = Just TurnRight
    toInstruction _ = Nothing
