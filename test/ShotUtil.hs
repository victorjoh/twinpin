module ShotUtil where

import           Shot
import           Space
import           CircleUtil

getShotPosition :: Shot -> Position2D
getShotPosition shot = getCirclePosition $ shotToCircle shot

getShotState :: Shot -> ShotState
getShotState (Shot _ _ state _) = state

-- returns how much time is needed for a shot to travel a certain distance
getShotMovementTime :: Vector1D -> Time
getShotMovementTime distance = round $ distance / shotSpeed
