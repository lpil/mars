module Control.ExplorationSpec where

import Test.Hspec
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Control.Exploration
import Data.Either (isLeft, isRight)
import Data.Either.Extra (unwrap)
import Data.Instruction
import Data.Planet as Planet (new, robot)
import Data.Position
import Data.Robot as Robot (bearing, new)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} spec

spec :: Spec
spec =
  describe "runInstructions" $ do
    let mkPlanet = Planet.new (Position (5, 3))
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
      let robot = Robot.new East (Position (1, 1))
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
      let robot = Robot.new North (Position (3, 2))
      let planet = mkPlanet robot
      let result = runInstructions instructions planet
      result `shouldSatisfy` isLeft
      (Planet.robot . unwrap $ result) `shouldBe`
        Robot.new North (Position (3, 3))
    describe "runInstructionSets" $
      it "runs given example" $ do
        let sets =
              [ InstructionSet
                { startBearing = East
                , startPosition = Position (1, 1)
                , steps =
                    [ TurnRight
                    , Advance
                    , TurnRight
                    , Advance
                    , TurnRight
                    , Advance
                    , TurnRight
                    , Advance
                    ]
                }
              , InstructionSet
                { startBearing = North
                , startPosition = Position (3, 2)
                , steps =
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
                }
              , InstructionSet
                { startBearing = West
                , startPosition = Position (0, 3)
                , steps =
                    [ TurnLeft
                    , TurnLeft
                    , Advance
                    , Advance
                    , Advance
                    , TurnLeft
                    , Advance
                    , TurnLeft
                    , Advance
                    , TurnLeft
                    ]
                }
              ]
        let results = runInstructionSets (Position (5, 3)) sets
        let expected =
              [ Right $ Robot.new East $ Position (1, 1)
              , Left $ Robot.new North $ Position (3, 3)
              , Right $ Robot.new South $ Position (2, 3)
              ]
        results `shouldBe` expected
