module ShotUtil where

import           Shot
import           Space
import           CircleUtil

getShotPosition :: Shot -> Position2D
getShotPosition shot = getCirclePosition $ shotToCircle shot
