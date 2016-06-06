module Control.Coercible
  ( class Coercible
  , coerce ) where

import Prelude

import Data.Int (toNumber)
import Data.String (fromCharArray, toCharArray, uncons)
import Data.Foldable (foldMap)
import Data.List (List(..), (:), reverse)
import Data.Maybe (Maybe(..))

class Coercible a b where
  coerce :: a -> b

instance coercibleId :: Coercible a a where
  coerce = id

instance coercibleUnit :: Coercible a Unit where
  coerce _ = unit

instance coercibleImage :: Coercible a (b -> a) where
  coerce = const

instance coercibleIntNumber :: Coercible Int Number where
  coerce = toNumber

instance coercibleCharString :: Coercible Char String where
  coerce = fromChar

instance coercibleArrayCharString :: Coercible (Array Char) String where
  coerce = fromCharArray

instance coercibleStringArrayChar :: Coercible String (Array Char) where
  coerce = toCharArray

instance coercibleFunctor :: (Functor f, Coercible a b) => Coercible (f a) (f b) where
  coerce = map coerce

instance coercibleApplicative :: Applicative f => Coercible a (f a) where
  coerce = pure

instance coercableListCharString :: Coercible (List Char) String where
  coerce = foldMap fromChar

instance coercibleStringListChar :: Coercible String (List Char) where
  coerce = reverse <<< coerce' Nil where
    coerce' acc str =
      case uncons str of
           Just { head, tail } -> coerce' (head : acc) str
           _ -> acc

fromChar :: Char -> String
fromChar c = fromCharArray [c]
