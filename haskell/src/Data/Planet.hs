module Data.Planet
  ( Planet
  , new
  , Data.Planet.execute
  , robot
  ) where

import Data.Instruction (Instruction)
import Data.Robot as Robot
import Data.Set as Set

type Scents = Set (Integer, Integer)

data Planet = Planet
  { maxX :: Integer
  , maxY :: Integer
  , robot :: Robot
  , scents :: Scents
  } deriving (Show, Eq)

{- Construct a new Planet with the given size limits, and initial Robot.
-}
new :: (Integer, Integer) -> Robot -> Planet
new (x, y) robot =
  Planet {maxX = x, maxY = y, robot = robot, scents = Set.empty}

{- Execute a single Instruction on the robot, checking if the robot becomes
   out of bounds. A scent is recorded if a robot is lost.
-}
execute :: Instruction -> Planet -> Either Planet Planet
execute instruction p = checkBounds p . Robot.execute instruction $ robot p
  where
    putRobot robot planet = planet {robot = robot}
    checkBounds planet robot
      | inBounds planet = Right $ putRobot robot planet
      -- TODO: Check sent
      | otherwise = Left . addScent $ putRobot robot planet
    addScent planet@Planet {robot = robot, scents = scents} =
      planet {scents = Set.insert (Robot.coordinates robot) scents}

inBounds :: Planet -> Bool
inBounds Planet {robot = robot, maxX = x, maxY = y} =
  not $ rx < 0 || ry < 0 || rx > x || ry > y
  where
    rx = Robot.x robot
    ry = Robot.y robot
