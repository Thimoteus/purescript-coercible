module Control.Coercible
  ( class Coercible
  , coerce ) where

import Prelude

import Control.Comonad (class Comonad, extract)
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.List (List(..), (:), reverse)
import Data.Maybe (Maybe(..))
import Data.String (CodePoint, codePointFromChar, fromCodePointArray, toCodePointArray, uncons)

class Coercible a b where
  coerce :: a -> b

instance coercibleIntNumber :: Coercible Int Number where
  coerce = toNumber
else instance coercibleCodePointString :: Coercible CodePoint String where
  coerce = fromCodePoint
else instance coercibleCharString :: Coercible Char String where
  coerce = fromCodePoint <<< codePointFromChar
else instance coercibleArrayCodePointString :: Coercible (Array CodePoint) String where
  coerce = fromCodePointArray
else instance coercibleArrayCharString :: Coercible (Array Char) String where
  coerce = fromCodePointArray <<< map codePointFromChar
else instance coercibleStringArrayCodePoint :: Coercible String (Array CodePoint) where
  coerce = toCodePointArray
else instance coercibleFunctor :: (Functor f, Coercible a b) => Coercible (f a) (f b) where
  coerce = map coerce
else instance coercibleApplicative :: Applicative f => Coercible a (f a) where
  coerce = pure
else instance coercibleBind :: Bind m => Coercible (m (m a)) (m a) where
  coerce = join
else instance coercibleComonad :: Comonad m => Coercible (m a) a where
  coerce = extract
else instance coercableListCodePointString :: Coercible (List CodePoint) String where
  coerce = foldMap fromCodePoint
else instance coercableListCharString :: Coercible (List Char) String where
  coerce = foldMap (fromCodePoint <<< codePointFromChar)
else instance coercibleStringListCodePoint :: Coercible String (List CodePoint) where
  coerce = reverse <<< coerce' Nil where
    coerce' acc str =
      case uncons str of
           Just { head, tail } -> coerce' (head : acc) str
           _ -> acc
else instance coercibleUnit :: Coercible a Unit where
  coerce _ = unit
else instance coercibleVoid :: Coercible Void a where
  coerce = absurd
else instance coercibleImage :: Coercible a (b -> a) where
  coerce = const

fromCodePoint :: CodePoint -> String
fromCodePoint c = fromCodePointArray [c]
