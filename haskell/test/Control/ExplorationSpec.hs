module Control.ExplorationSpec where

import Test.Hspec
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Control.Exploration
import Data.Either (isLeft, isRight)
import Data.Instruction (Bearing(..), Instruction(..))
import Data.Planet as Planet (new, robot)
import Data.Robot as Robot (bearing, coordinates, new)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} spec
  -- @sample_input """
  -- 5 3
  -- 1 1 E
  -- RFRFRFRF
  -- 3 2 N
  -- FRRFLLFFRRFLL
  -- 0 3 W
  -- LLFFFLFLFL
  -- """
  -- @sample_output """
  -- 1 1 E
  -- 3 3 N LOST
  -- 2 3 S
  -- """

spec :: Spec
spec =
  describe "runInstructions" $ do
    let mkPlanet = Planet.new (5, 3)
    let unwrap (Left a) = a
        unwrap (Right a) = a
    it "execs an successful instruction set" $ do
      let instructions =
            [ TurnRight
            , Advance
            , TurnRight
            , Advance
            , TurnRight
            , Advance
            , TurnRight
            , Advance
            ]
      let robot = Robot.new East (1, 1)
      let planet = mkPlanet robot
      let result = runInstructions instructions planet
      result `shouldSatisfy` isRight
      (Planet.robot . unwrap $ result) `shouldBe` robot
    it "execs an unsuccessful instruction set" $ do
      let instructions =
            [ Advance
            , TurnRight
            , TurnRight
            , Advance
            , TurnLeft
            , TurnLeft
            , Advance
            , Advance
            , TurnRight
            , TurnRight
            , Advance
            , TurnLeft
            , TurnLeft
            ]
      let robot = Robot.new North (3, 2)
      let planet = mkPlanet robot
      let result = runInstructions instructions planet
      result `shouldSatisfy` isLeft
      (Planet.robot . unwrap $ result) `shouldBe` Robot.new North (3, 4)
