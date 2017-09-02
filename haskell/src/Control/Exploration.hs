{-# LANGUAGE NamedFieldPuns #-}

module Control.Exploration
  ( runInstructions
  , runInstructionSets
  ) where

import Data.Either.Extra (mapBoth, unwrap)
import Data.Instruction
import Data.List (scanl)
import Data.Planet as Planet
       (Planet, execute, new, putRobot, robot)
import Data.Position (Position)
import Data.Robot as Robot (new)
import Data.Robot (Robot)

type Bounds = Position

{-| Have a Robot execute a series of instructions on a Planet
-}
runInstructions :: [Instruction] -> Planet -> Either Planet Planet
runInstructions [] planet = Right planet
runInstructions (instruction:rest) planet =
  execute instruction planet >>= runInstructions rest

{-| Have a series of Robots execute a series of instructions on a Planet
    of the given dimensions.
-}
runInstructionSets :: Bounds -> [InstructionSet] -> [Either Robot Robot]
runInstructionSets _ [] = []
runInstructionSets bounds (set@InstructionSet {steps}:rest) =
  mapBoth Planet.robot <$> scanl go (Right $ newPlanet set) rest
  where
    newPlanet set = Planet.new bounds $ newRobot set
    newRobot InstructionSet {startBearing, startPosition} =
      Robot.new startBearing startPosition
    go planet set@InstructionSet {steps} =
      runInstructions steps . putRobot (newRobot set) $ unwrap planet
