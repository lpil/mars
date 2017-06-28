module Data.PlanetSpec where

import Test.Hspec
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Data.Either (isLeft, isRight)
import Data.Instruction (Bearing(..), Instruction(..))
import Data.Planet
import Data.Robot as Robot hiding (execute)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} spec

spec :: Spec
spec =
  describe "execute" $ do
    it "is Left when robot starts out of bounds north" $ do
      let robot = Robot.mkRobot East (0, 2)
      let planet = new (1, 1) robot
      execute TurnLeft planet `shouldSatisfy` isLeft
    it "is Left when robot starts out of bounds east" $ do
      let robot = Robot.mkRobot East (2, 0)
      let planet = new (1, 1) robot
      execute TurnLeft planet `shouldSatisfy` isLeft
    it "is Left when robot starts out of bounds south" $ do
      let robot = Robot.mkRobot East (0, -1)
      let planet = new (1, 1) robot
      execute TurnLeft planet `shouldSatisfy` isLeft
    it "is Left when robot starts out of bounds west" $ do
      let robot = Robot.mkRobot East (-1, 0)
      let planet = new (1, 1) robot
      execute TurnLeft planet `shouldSatisfy` isLeft
    it "progresses robot when in bounds" $ do
      let planet = new (1, 1) (Robot.mkRobot East (0, 0))
      let result = robot <$> execute TurnLeft planet
      result `shouldBe` Right (Robot.mkRobot North (0, 0))
