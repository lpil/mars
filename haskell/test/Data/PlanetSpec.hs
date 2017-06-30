module Data.PlanetSpec where

import Test.Hspec
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Data.Either (isLeft, isRight)
import Data.Instruction (Bearing(..), Instruction(..))
import Data.Planet as Planet
import Data.Robot as Robot hiding (execute)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} spec

spec :: Spec
spec =
  describe "execute" $ do
    it "is Left when robot moves out of bounds north" $ do
      let robot = Robot.new North (1, 1)
      let planet = Planet.new (1, 1) robot
      execute Advance planet `shouldSatisfy` isLeft
    it "is Left when robot moves out of bounds east" $ do
      let robot = Robot.new East (1, 1)
      let planet = Planet.new (1, 1) robot
      execute Advance planet `shouldSatisfy` isLeft
    it "is Left when robot moves out of bounds south" $ do
      let robot = Robot.new South (0, 0)
      let planet = Planet.new (1, 1) robot
      execute Advance planet `shouldSatisfy` isLeft
    it "is Left when robot moves out of bounds west" $ do
      let robot = Robot.new West (0, 0)
      let planet = Planet.new (1, 1) robot
      execute Advance planet `shouldSatisfy` isLeft
    it "progresses robot when in bounds" $ do
      let planet = Planet.new (1, 1) (Robot.new East (0, 0))
      let result = robot <$> execute TurnLeft planet
      result `shouldBe` Right (Robot.new North (0, 0))
    it "leaves scents that prevents robots being lost twice" $ do
      let robot = Robot.new North (1, 1)
      let planet = Planet.new (1, 1) robot
      let planetWithScent = execute Advance planet
      planetWithScent `shouldSatisfy` isLeft
      let result = execute Advance $ putRobot robot $ unwrap planetWithScent
      result `shouldSatisfy` isRight
  where
    unwrap (Left a) = a
    unwrap (Right a) = a
