module Control.Exploration
  ( runInstructions
  ) where

import Data.Instruction (Instruction)
import Data.Planet (Planet, execute)

runInstructions :: [Instruction] -> Planet -> Either Planet Planet
runInstructions [] planet = Right planet
runInstructions (instruction:rest) planet =
  execute instruction planet >>= runInstructions rest
