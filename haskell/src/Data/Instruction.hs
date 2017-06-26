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
parseChunk [line1, line2] =
  case (parseLine1 line1, parseLine2 line2) of
    (Just (p, d), Just s) ->
      Just InstructionSet {startPosition = p, startDirection = d, steps = s}
    _ -> Nothing
parseChunk _ = Nothing

parseLine1 :: String -> Maybe ((Integer, Integer), Bearing)
parseLine1 [x, ' ', y, ' ', d] =
  case (parsePos x, parsePos y, parseDir d) of
    (Just x', Just y', Just d') -> Just ((x', y'), d')
    _ -> Nothing
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
