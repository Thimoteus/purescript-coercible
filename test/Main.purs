module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)

import Data.List ((:), List(Nil))
import Control.Coercible (class Coercible, coerce)

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
  logShow $ coerce three :: String
  logShow $ coerce three :: Int
  logShow $ coerce 3 :: Number
  logShow $ coerce '3' :: String
  logShow $ coerce 3 :: Unit
  logShow $ coerce 3 :: Array Int
  logShow $ coerce (coerce 3 :: Unit) :: Array Unit
  logShow $ coerce ('a' : 'b' : 'c' : Nil) :: String
  logShow $ coerce ['a', 'b', 'c'] :: String
