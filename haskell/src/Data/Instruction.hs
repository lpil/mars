module Data.Instruction
  ( Instruction(..)
  , InstructionSet(..)
  , Bearing(..)
  , parse
  , parseInstructions
  ) where

import Data.Char (isDigit)
import Data.List.Split (chunksOf)
import Data.Position (Position(..))

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
  { startPosition :: Position
  , startBearing :: Bearing
  , steps :: [Instruction]
  } deriving (Eq, Show)

parse :: String -> Maybe (Position, [InstructionSet])
parse (x:' ':y:rest) = do
  x' <- parseInt x
  y' <- parseInt y
  sets <- parseInstructions rest
  Just (Position (x', y'), sets)
parse _ = Nothing

parseInstructions :: String -> Maybe [InstructionSet]
parseInstructions =
  sequence . fmap parseChunk . chunksOf 2 . filter (/= "") . lines

parseChunk :: [String] -> Maybe InstructionSet
parseChunk [[x, ' ', y, ' ', b], instructions] = do
  x' <- parseInt x
  y' <- parseInt y
  b' <- parseBearing b
  s <- sequence $ fmap parseInstruction instructions
  Just
    InstructionSet
    {startPosition = Position (x', y'), startBearing = b', steps = s}
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

parseInt :: Char -> Maybe Int
parseInt c
  | isDigit c = Just $ read [c]
  | otherwise = Nothing
