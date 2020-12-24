module SpaceUtil where

import Approx
import Data.Foldable (toList)
import SDL (V2)

instance Approx a => Approx (V2 a) where
  isApproxEqual = isApproxEqual' toList
