module Data.Robot
  ( Bearing(..)
  , Instruction(..)
  , bearing
  , coordinates
  , mkRobot
  , execute
  ) where

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

data Robot =
  Robot Bearing
        (Integer, Integer)

bearing :: Robot -> Bearing
bearing (Robot bearing _) = bearing

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ coordinates) = coordinates

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

execute :: Instruction -> Robot -> Robot
execute TurnRight robot = turnRight robot
execute TurnLeft robot = turnLeft robot
execute Advance robot = advance robot

advance :: Robot -> Robot
advance (Robot North (x, y)) = Robot North (x, y + 1)
advance (Robot East (x, y)) = Robot East (x + 1, y)
advance (Robot South (x, y)) = Robot South (x, y - 1)
advance (Robot West (x, y)) = Robot West (x - 1, y)

turnLeft :: Robot -> Robot
turnLeft (Robot North c) = Robot West c
turnLeft (Robot West c) = Robot South c
turnLeft (Robot South c) = Robot East c
turnLeft (Robot East c) = Robot North c

turnRight :: Robot -> Robot
turnRight (Robot North c) = Robot East c
turnRight (Robot East c) = Robot South c
turnRight (Robot South c) = Robot West c
turnRight (Robot West c) = Robot North c
