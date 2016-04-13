module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, print)

import Data.List ((:), List(Nil))
import Data.Coercible (class Coercible, coerce)

data N = Z | S N

instance showN :: Show N where
  show Z = "Z"
  show (S n) = "S" <> show n

instance coerceNInt :: Coercible N Int where
  coerce Z = 0
  coerce (S n) = 1 + coerce n

instance coerceNString :: Coercible N String where
  coerce = show

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let three = S (S (S Z))
  print $ coerce three :: String
  print $ coerce three :: Int
  print $ coerce 3 :: Number
  print $ coerce '3' :: String
  print $ coerce 3 :: Unit
  print $ coerce 3 :: Array Int
  print $ coerce (coerce 3 :: Unit) :: Array Unit
  print $ coerce ('a' : 'b' : 'c' : Nil) :: String
  print $ coerce ['a', 'b', 'c'] :: String
