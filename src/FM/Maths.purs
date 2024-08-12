module FM.Maths
  ( weightedMean
  , mean
  , weightedStdev
  , stdev
  , Normal
  , zscore
  , erf
  , zscorePercentile
  ) where

import Prelude

import Data.Array.NonEmpty (fromFoldable1)
import Data.Foldable (length, sum)
import Data.Int (toNumber)
import Data.Number (exp, pi, pow, sqrt, sqrt2)
import Data.Semigroup.Foldable (class Foldable1, foldl1)
import Data.Tuple (Tuple(..))

type Normal = { mean :: Number, stdev :: Number }

weightedMean :: forall f. Foldable1 f => f (Tuple Number Number) -> Number
weightedMean =
  foldl1
    (\(Tuple s1 w1) (Tuple s2 w2) -> Tuple (s1 + s2) (w1 + w2))
    >>> (\(Tuple s w) -> s / w)

mean :: forall f. Foldable1 f => f Number -> Number
mean xs = sum xs / length xs

weightedStdev :: forall f. Foldable1 f => f (Tuple Number Number) -> Normal
weightedStdev xs =
  { mean: m
  , stdev: sd
  }
  where
  m = weightedMean xs
  sd =
    foldl1
      ( \(Tuple s1 w1) (Tuple s2 w2) -> Tuple (s1 + pow (s2 - m) 2.0) (w1 + w2)
      )
      xs
      # (\(Tuple s w) -> s / w)
      # sqrt

stdev :: forall f. Foldable1 f => f Number -> Normal
stdev = fromFoldable1
  >>> map (\s -> Tuple s 1.0)
  >>> weightedStdev

zscore :: Normal -> Number -> Number
zscore { mean: m, stdev: s } x = (x - m) / s

erf :: Number -> Number
erf z
  | z == 0.0 = 0.0
  | otherwise = (2.0 / sqrt pi) * go 0
      where
      segCount = 20
      segLength = z / toNumber segCount

      -- integration
      go :: Int -> Number
      go i
        | i >= segCount = 0.0
        | otherwise =
            let
              x = toNumber i * segLength + segLength / 2.0
              y = exp (-pow x 2.0)
            in
              y * segLength + go (i + 1)

zscorePercentile :: Number -> Number
zscorePercentile z = (1.0 + erf (z / sqrt2)) / 2.0
