{-# LANGUAGE RecordPuns #-}

module Data.Planet
  ( Planet
  , new
  , Data.Planet.execute
  , robot
  , putRobot
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

{-| Construct a new Planet with the given size limits, and initial Robot.
-}
new :: (Integer, Integer) -> Robot -> Planet
new (maxX, maxY) robot = Planet {maxX, maxY, robot, scents = Set.empty}

{-| Execute a single Instruction on the robot, checking if the robot becomes
    out of bounds. A scent is recorded if a robot is lost.
-}
execute :: Instruction -> Planet -> Either Planet Planet
execute instruction p = checkBounds p . Robot.execute instruction $ robot p
  where
    checkBounds planet robot
      | inBounds planet robot = Right $ putRobot robot planet
      | hasScent (Robot.coordinates robot) planet = Right planet
      | otherwise = Left . addScent $ putRobot robot planet
    hasScent coordinates = Set.member coordinates . scents
    addScent planet@Planet {robot, scents} =
      planet {scents = Set.insert (Robot.coordinates robot) scents}
    inBounds Planet {maxX, maxY} robot =
      not $ rx < 0 || ry < 0 || rx > maxX || ry > maxY
      where
        rx = Robot.x robot
        ry = Robot.y robot

{-| Replace a Planet's Robot.
-}
putRobot :: Robot -> Planet -> Planet
putRobot robot planet = planet {robot}
