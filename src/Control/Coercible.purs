module Control.Coercible
  ( class Coercible
  , coerce ) where

import Prelude

import Control.Comonad (class Comonad, extract)
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.List (List(..), (:), reverse)
import Data.Maybe (Maybe(..))
import Data.String (fromCharArray, toCharArray, uncons)

class Coercible a b where
  coerce :: a -> b

instance coercibleId :: Coercible a a where
  coerce = id

instance coercibleUnit :: Coercible a Unit where
  coerce _ = unit

instance coercibleVoid :: Coercible Void a where
  coerce = absurd

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

instance coercibleBind :: Bind m => Coercible (m (m a)) (m a) where
  coerce = join

instance coercibleComonad :: Comonad m => Coercible (m a) a where
  coerce = extract

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
