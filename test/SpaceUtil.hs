module SpaceUtil where

import           Approx
import           SDL                            ( V2 )
import           Data.Foldable                  ( toList )

instance Approx a => Approx (V2 a) where
    isApproxEqual = isApproxEqual' toList
