module Data.RobotSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Data.Instruction
import Data.Robot

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} spec

spec :: Spec
spec = do
  let testRobot dir = mkRobot dir (0, 0)
  describe "mkRobot" $ do
    it "A robot is created with a position and a direction" $ do
      let robot = mkRobot North (0, 0)
      coordinates robot `shouldBe` (0, 0)
      bearing robot `shouldBe` North
    it "Negative positions are allowed" $ do
      let robot = mkRobot South (-1, -1)
      coordinates robot `shouldBe` (-1, -1)
      bearing robot `shouldBe` South
  describe "execute" $ do
    it "TurnRight from North" $ do
      let robot = execute TurnRight (testRobot North)
      coordinates robot `shouldBe` (0, 0)
      bearing robot `shouldBe` East
    it "TurnRight from North" $ do
      let robot = execute TurnRight (testRobot East)
      coordinates robot `shouldBe` (0, 0)
      bearing robot `shouldBe` South
    it "TurnRight from North" $ do
      let robot = execute TurnRight (testRobot South)
      coordinates robot `shouldBe` (0, 0)
      bearing robot `shouldBe` West
    it "TurnRight from North" $ do
      let robot = execute TurnRight (testRobot West)
      coordinates robot `shouldBe` (0, 0)
      bearing robot `shouldBe` North
    it "TurnLeft from North" $ do
      let robot = execute TurnLeft (testRobot North)
      coordinates robot `shouldBe` (0, 0)
      bearing robot `shouldBe` West
    it "TurnLeft from West" $ do
      let robot = execute TurnLeft (testRobot West)
      coordinates robot `shouldBe` (0, 0)
      bearing robot `shouldBe` South
    it "TurnLeft from South" $ do
      let robot = execute TurnLeft (testRobot South)
      coordinates robot `shouldBe` (0, 0)
      bearing robot `shouldBe` East
    it "TurnLeft from East" $ do
      let robot = execute TurnLeft (testRobot East)
      coordinates robot `shouldBe` (0, 0)
      bearing robot `shouldBe` North
    it "Advance North" $ do
      let robot = execute Advance $ testRobot North
      coordinates robot `shouldBe` (0, 1)
      bearing robot `shouldBe` North
    it "Advance East" $ do
      let robot = execute Advance $ testRobot East
      coordinates robot `shouldBe` (1, 0)
      bearing robot `shouldBe` East
    it "Advance South" $ do
      let robot = execute Advance $ testRobot South
      coordinates robot `shouldBe` (0, -1)
      bearing robot `shouldBe` South
    it "Advance West" $ do
      let robot = execute Advance $ testRobot West
      coordinates robot `shouldBe` (-1, 0)
      bearing robot `shouldBe` West
