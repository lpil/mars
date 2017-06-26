module Data.InstructionSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Data.Instruction

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} spec

spec :: Spec
spec =
  describe "parse" $ do
    it "does not parse invalid line 1 data" $ do
      let result = parse (unlines ["2 z N", "LRF"])
      result `shouldBe` Nothing
    it "does not parse invalid line 2 data" $ do
      let result = parse (unlines ["2 0 N", "Hello LRF"])
      result `shouldBe` Nothing
    it "does not parse unevent data" $ do
      let result = parse (unlines ["2 3 N"])
      result `shouldBe` Nothing
    it "parses a single result" $ do
      let result = parse (unlines ["2 3 N", "LRF"])
      result `shouldBe`
        Just
          [ InstructionSet
            { startPosition = (2, 3)
            , startDirection = North
            , steps = [TurnLeft, TurnRight, Advance]
            }
          ]
    it "parses multiple results" $ do
      let result =
            parse (unlines ["1 1 E", "RFRFRFRF", "3 2 N", "FRRFLLFFRRFLL"])
      result `shouldBe`
        Just
          [ InstructionSet
            { startPosition = (1, 1)
            , startDirection = East
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
            { startPosition = (3, 2)
            , startDirection = North
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
          ]
