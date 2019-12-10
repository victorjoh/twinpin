module CircleUtil where

import           Circle
import           Space

getCirclePosition :: Circle -> Position2D
getCirclePosition (Circle position _) = position
