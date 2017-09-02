module Data.Robot
  ( Robot
  , bearing
  , execute
  , new
  , x
  , y
  ) where

import Data.Instruction
import Data.Position

data Robot =
  Robot Bearing
        Position
  deriving (Show, Eq)

instance Positional Robot where
  position (Robot _ pos) = pos

bearing :: Robot -> Bearing
bearing (Robot bearing _) = bearing

x :: Robot -> Int
x (Robot _ (Position (x', _))) = x'

y :: Robot -> Int
y (Robot _ (Position (_, y'))) = y'

new :: Bearing -> Position -> Robot
new = Robot

execute :: Instruction -> Robot -> Robot
execute TurnRight robot = turnRight robot
execute TurnLeft robot = turnLeft robot
execute Advance robot = advance robot

advance :: Robot -> Robot
advance (Robot North (Position (x, y))) = Robot North $ Position (x, y + 1)
advance (Robot East (Position (x, y))) = Robot East $ Position (x + 1, y)
advance (Robot South (Position (x, y))) = Robot South $ Position (x, y - 1)
advance (Robot West (Position (x, y))) = Robot West $ Position (x - 1, y)

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
