module Data.Either.Extra
  ( unwrap
  , mapBoth
  ) where

unwrap :: Either a a -> a
unwrap (Right a) = a
unwrap (Left a) = a

mapBoth :: (a -> b) -> Either a a -> Either b b
mapBoth f (Right a) = Right (f a)
mapBoth f (Left a) = Left (f a)
