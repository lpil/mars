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
parseChunk [[x, ' ', y, ' ', d], instructions] = do
  x' <- parsePos x
  y' <- parsePos y
  d' <- parseDir d
  s <- sequence $ fmap parseInstruction instructions
  Just InstructionSet {startPosition = (x', y'), startDirection = d', steps = s}
parseChunk _ = Nothing

parseDir :: Char -> Maybe Bearing
parseDir 'N' = Just North
parseDir 'E' = Just East
parseDir 'S' = Just South
parseDir 'W' = Just West
parseDir _ = Nothing

parseInstruction :: Char -> Maybe Instruction
parseInstruction 'F' = Just Advance
parseInstruction 'L' = Just TurnLeft
parseInstruction 'R' = Just TurnRight
parseInstruction _ = Nothing

parsePos :: Char -> Maybe Integer
parsePos c
  | isDigit c = Just $ read [c]
  | otherwise = Nothing
