module Data.Position
  ( Positional
  , Position(..)
  , position
  ) where

newtype Position =
  Position (Int, Int)
  deriving (Show, Eq, Ord)

class Positional a where
  position :: a -> Position

instance Positional Position where
  position = id
