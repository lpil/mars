module Data.RobotSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Data.Instruction
import Data.Position
import Data.Robot

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} spec

spec :: Spec
spec = do
  let testRobot dir = new dir $ Position (0, 0)
  describe "new" $ do
    it "A robot is created with a position and a direction" $ do
      let robot = new North $ Position (0, 0)
      position robot `shouldBe` Position (0, 0)
      bearing robot `shouldBe` North
    it "Negative positions are allowed" $ do
      let robot = new South $ Position (-1, -1)
      position robot `shouldBe` Position (-1, -1)
      bearing robot `shouldBe` South
  describe "execute" $ do
    it "TurnRight from North" $ do
      let robot = execute TurnRight (testRobot North)
      position robot `shouldBe` Position (0, 0)
      bearing robot `shouldBe` East
    it "TurnRight from North" $ do
      let robot = execute TurnRight (testRobot East)
      position robot `shouldBe` Position (0, 0)
      bearing robot `shouldBe` South
    it "TurnRight from North" $ do
      let robot = execute TurnRight (testRobot South)
      position robot `shouldBe` Position (0, 0)
      bearing robot `shouldBe` West
    it "TurnRight from North" $ do
      let robot = execute TurnRight (testRobot West)
      position robot `shouldBe` Position (0, 0)
      bearing robot `shouldBe` North
    it "TurnLeft from North" $ do
      let robot = execute TurnLeft (testRobot North)
      position robot `shouldBe` Position (0, 0)
      bearing robot `shouldBe` West
    it "TurnLeft from West" $ do
      let robot = execute TurnLeft (testRobot West)
      position robot `shouldBe` Position (0, 0)
      bearing robot `shouldBe` South
    it "TurnLeft from South" $ do
      let robot = execute TurnLeft (testRobot South)
      position robot `shouldBe` Position (0, 0)
      bearing robot `shouldBe` East
    it "TurnLeft from East" $ do
      let robot = execute TurnLeft (testRobot East)
      position robot `shouldBe` Position (0, 0)
      bearing robot `shouldBe` North
    it "Advance North" $ do
      let robot = execute Advance $ testRobot North
      position robot `shouldBe` Position (0, 1)
      bearing robot `shouldBe` North
    it "Advance East" $ do
      let robot = execute Advance $ testRobot East
      position robot `shouldBe` Position (1, 0)
      bearing robot `shouldBe` East
    it "Advance South" $ do
      let robot = execute Advance $ testRobot South
      position robot `shouldBe` Position (0, -1)
      bearing robot `shouldBe` South
    it "Advance West" $ do
      let robot = execute Advance $ testRobot West
      position robot `shouldBe` Position (-1, 0)
      bearing robot `shouldBe` West
