module Control.Coercible
  ( class Coercible
  , coerce ) where

import Prelude

import Data.Int (toNumber)
import Data.Char (toString)
import Data.String (fromCharArray, toCharArray, uncons)
import Data.Foldable (foldMap)
import Data.List (List(..), reverse)
import Data.Maybe (Maybe(..))

infixr 5 Cons as :

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
  coerce = toString

instance coercibleArrayCharString :: Coercible (Array Char) String where
  coerce = fromCharArray

instance coercibleStringArrayChar :: Coercible String (Array Char) where
  coerce = toCharArray

-- Does this have too high a chance of running into overlapping instances?
{-- instance coercibleShow :: Show a => Coercible a String where --}
{--   coerce = show --}

instance coercibleFunctor :: (Functor f, Coercible a b) => Coercible (f a) (f b) where
  coerce = map coerce

-- Is this even useful?
{-- instance coercibleApply :: Apply f => Coercible (f (a -> b)) (f a -> f b) where --}
{--   coerce = apply --}

instance coercibleApplicative :: Applicative f => Coercible a (f a) where
  coerce = pure

-- Defining it for any Foldable runs into overlapping instances with Array Char,
-- so it's best not to keep it general
instance coercableListCharString :: Coercible (List Char) String where
  coerce = foldMap toString

instance coercibleStringListChar :: Coercible String (List Char) where
  coerce = reverse <<< coerce' Nil where
    coerce' acc str =
      case uncons str of
           Just { head, tail } -> coerce' (head : acc) str
           _ -> acc
