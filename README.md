# purescript-coercible [![Build Status](https://travis-ci.org/Thimoteus/purescript-coercible.svg?branch=master)](https://travis-ci.org/Thimoteus/purescript-coercible) [![purescript-coercible on Pursuit](https://pursuit.purescript.org/packages/purescript-coercible)](https://pursuit.purescript.org/packages/purescript-coercible/badge)

*Fun with multi-parameter type classes!*

This library provides a way to *safely* coerce values of some type into another
type, when it makes sense.

Some sane default instances are provided, particularly for the primitive types.

You may wish to use this for newtype wrappers:

```purescript
newtype Address = Address String

instance coercibleAddress :: Coercible Address String where
  coerce (Address s) = s

instance coercibleString :: Coercible String Address where
  coerce = Address

newtype Filepath = Filepath String

instance coercibleFilepathAddress :: Coercible Filepath Address where
  coerce (Filepath p) = Address p
```

# install
`bower i purescript-coercible`
