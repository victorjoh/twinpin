module VisualUtil where

import           Visual

instance Eq VectorImage where
    (VectorImage s1 c1 _) == (VectorImage s2 c2 _) = s1 == s2 && c1 == c2
