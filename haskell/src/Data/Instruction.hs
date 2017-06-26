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
  , startBearing :: Bearing
  , steps :: [Instruction]
  } deriving (Eq, Show)

parse :: String -> Maybe [InstructionSet]
parse = sequence . fmap parseChunk . chunksOf 2 . lines

parseChunk :: [String] -> Maybe InstructionSet
parseChunk [[x, ' ', y, ' ', b], instructions] = do
  x' <- parseInt x
  y' <- parseInt y
  b' <- parseBearing b
  s <- sequence $ fmap parseInstruction instructions
  Just InstructionSet {startPosition = (x', y'), startBearing = b', steps = s}
parseChunk _ = Nothing

parseBearing :: Char -> Maybe Bearing
parseBearing 'N' = Just North
parseBearing 'E' = Just East
parseBearing 'S' = Just South
parseBearing 'W' = Just West
parseBearing _ = Nothing

parseInstruction :: Char -> Maybe Instruction
parseInstruction 'F' = Just Advance
parseInstruction 'L' = Just TurnLeft
parseInstruction 'R' = Just TurnRight
parseInstruction _ = Nothing

parseInt :: Char -> Maybe Integer
parseInt c
  | isDigit c = Just $ read [c]
  | otherwise = Nothing
