module VisualUtil where

import           Visual
import           Approx
import           Graphics.Rasterific     hiding ( V2(..) )
import qualified Graphics.Rasterific           as R
                                                ( V2(..) )
import           Data.Foldable

instance Eq VectorImage where
    (VectorImage s1 c1 _) == (VectorImage s2 c2 _) = s1 == s2 && c1 == c2

instance Approx CubicBezier where
    isApproxEqual = isApproxEqual' (\(CubicBezier a b c d) -> [a, b, c, d])

instance Approx a => Approx (R.V2 a) where
    isApproxEqual = isApproxEqual' toList

instance Approx Line where
    isApproxEqual = isApproxEqual' (\(Line a b) -> [a, b])
