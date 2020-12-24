module BulletUtil where

import Bullet
import CircleUtil
import Space

getBulletPosition :: Bullet -> Position2D
getBulletPosition bullet = getCirclePosition $ bulletToCircle bullet

getBulletState :: Bullet -> BulletState
getBulletState (Bullet _ _ state _) = state

-- returns how much time is needed for a bullet to travel a certain distance
getBulletMovementTime :: Vector1D -> Time
getBulletMovementTime distance = round $ distance / bulletSpeed
