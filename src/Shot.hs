module Shot
    ( Shot
    , shotSpeed
    , shotTextureFiles
    , createShot
    , toDrawableShot
    , updateShot
    , isShotWithinBounds
    , shotToCircle
    , setShotHit
    )
where

import           Space
import           Circle
import           Foreign.C.Types
import           SDL

data Shot = Shot Circle Velocity2D FilePath deriving (Show, Eq)

shotSpeed :: Speed
shotSpeed = 0.7

defaultTexture = "gen/shot.bmp"
hitTexture = "gen/shot-hit.bmp"

shotTextureFiles :: [FilePath]
shotTextureFiles = [defaultTexture, hitTexture]

side :: Size1D
side = 32 / 3

size :: Size2D
size = V2 side side

createShot :: Position2D -> Angle2D -> Shot
createShot position angle = Shot (Circle position (side / 2))
                                 (toVelocity angle shotSpeed)
                                 defaultTexture

toDrawableShot :: Shot -> (FilePath, Maybe (Rectangle CInt), CDouble)
toDrawableShot (Shot cicle _ texture) = toDrawableCircle cicle 0 texture

updateShot :: DeltaTime -> Shot -> Shot
updateShot dt (Shot circle velocity texture) =
    Shot (updateCirclePosition velocity dt circle) velocity texture

isShotWithinBounds :: Bounds2D -> Shot -> Bool
isShotWithinBounds bounds (Shot circle _ _) =
    isCircleWithinBounds bounds circle

shotToCircle :: Shot -> Circle
shotToCircle (Shot circle _ _) = circle

setShotHit :: Shot -> Shot
setShotHit (Shot circle velocity texture) = Shot circle velocity hitTexture
