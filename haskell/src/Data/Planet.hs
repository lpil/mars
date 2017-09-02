{-# LANGUAGE NamedFieldPuns #-}

module Data.Planet
  ( Planet
  , Data.Planet.new
  , Data.Planet.execute
  , robot
  , putRobot
  ) where

import qualified Data.Instruction
import Data.Instruction (Instruction)
import Data.Position (Position(..), Positional, position)
import qualified Data.Robot as Robot
import Data.Robot (Robot)
import qualified Data.Set as Set
import Data.Set (Set)

type Scents = Set Position

data Planet = Planet
  { maxX :: Int
  , maxY :: Int
  , robot :: Robot
  , scents :: Scents
  } deriving (Show, Eq)

{-| Construct a new Planet with the given size limits, and initial Robot.
-}
new :: Position -> Robot -> Planet
new (Position (maxX, maxY)) robot =
  Planet {maxX, maxY, robot, scents = Set.empty}

{-| Execute a single Instruction on the robot, checking if the robot becomes
    out of bounds. A scent is recorded if a robot is lost.
-}
execute :: Instruction -> Planet -> Either Planet Planet
execute instruction p = nextPlanet p . Robot.execute instruction $ robot p
  where
    nextPlanet planet robot
      | inBounds robot planet = Right $ putRobot robot planet
      | hasScent (position robot) planet = Right planet
      | otherwise = Left $ addScent robot planet

{-| Check whether a position is in bounds for a given planet
-}
inBounds :: Positional p => p -> Planet -> Bool
inBounds p Planet {maxX, maxY} =
  let (Position (px, py)) = position p
  in not $ px < 0 || py < 0 || px > maxX || py > maxY

{-| Check whether a Planet has a sent at a given position.
-}
hasScent :: Positional p => p -> Planet -> Bool
hasScent p = Set.member (position p) . scents

{-| Add a scene at a given position.
-}
addScent :: Positional p => p -> Planet -> Planet
addScent robot planet =
  let (Planet {scents}) = planet
      newScents = Set.insert (position robot) scents
  in planet {scents = newScents}

{-| Replace a Planet's Robot.
-}
putRobot :: Robot -> Planet -> Planet
putRobot robot planet = planet {robot}
