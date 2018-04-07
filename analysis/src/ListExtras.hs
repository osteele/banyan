{-# LANGUAGE NoImplicitPrelude #-}

module ListExtras
  ( invariant
  , shortest
  , shortest2
  , conjugateWithSentinels
  ) where

import           Prelude   as UnsafePartial (tail)
import           Protolude

{-| `invariant f` is the convergence of the fixed-point iteraton f, i.e. the
element x s.t. f x == x.

The result is undefined if the iteration doesn't converge. This can happen
even if there is a fixed point, if it doesn't attract the initial value.

Cf. `Data.function.fix`, which returns the least-defined *domain-theory* fixed
point.
-}
invariant :: Eq a => (a -> a) -> a -> a
invariant f x =
  if x == x'
    then x
    else invariant f x'
  where x' = f x

-- | Apply each function to a value; return the shortest result.
shortest :: Foldable f => [a -> f b] -> a -> f b
shortest fns x =
  minimumBy (compare `on` length) $ fmap ($ x) fns
  -- minimumBy (compare `on` length) $ funcs <*> [x]

-- | Apply each curried binary function to a value; return the shortest result.
shortest2 :: Foldable f => [a -> b -> f c] -> a -> b -> f c
shortest2 f =
  curry $ shortest $ fmap uncurry f

{-| Append a sentinel to each end of a list, apply the function, and remove the
first and last element of the result.

(This is the left conjugation of `f` by `[s] ++ _ ++ [s]`.)

`conjugateWithSentinels` is a partial function. It's undefined if the provided
doesn't return a list with at least two elements.

    conjugateWithSentinels 'a' f "bcd" == head . init . f "abcda"
    conjugateWithSentinels s id == id
-}
conjugateWithSentinels :: a -> ([a] -> [b]) -> [a] -> [b]
conjugateWithSentinels s f =
  eachEnd UnsafePartial.tail . f . eachEnd (s :)
    where eachEnd g = g . reverse . g . reverse
