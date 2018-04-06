module ListExtras
  ( invariant
  , shortest
  , withSentinel
  ) where

import Data.Function (on)
import Data.List (minimumBy)

{-| `invariant f` is the *mathematical fixed point of f, i.e. the element x s.t.
f x == x.

Cf. `Data.function.fix`, which returns the *least-defined* fixed point.
-}
invariant
  :: Eq a
  => (a -> a) -> a -> a
invariant fn a =
  let b = fn a
  in if a == b
       then a
       else invariant fn b

-- | Apply each function to a value; return the shortest result
shortest
  :: Foldable f
  => [a -> f b] -> a -> f b
shortest funcs x = minimumBy (compare `on` length) $ funcs <*> [x]

{-| Append a to each end of a list, applies the function, and removes the
first and last element of the result.

    withSentinel 'a' f "bcd" == head . init . f "abcda"
    withSentinel s id == id
-}
withSentinel :: a -> ([a] -> [b]) -> [a] -> [b]
withSentinel s func =
  let eachEnd f = f . reverse . f . reverse
  in eachEnd tail . func . eachEnd (s :)
