module Shot
    ( Shot
    , shotSpeed
    , shotTextureFile
    , createShot
    , toDrawableShot
    , updateShot
    , isShotWithinBounds
    )
where

import           Space
import           Shape
import           Foreign.C.Types
import           SDL

newtype Shot = Shot Shape deriving (Show, Eq)

shotSpeed :: Speed
shotSpeed = 0.7

shotTextureFile :: FilePath
shotTextureFile = "gen/shot.bmp"

side :: Size1D
side = 32 / 3

size :: Size2D
size = V2 side side

createShot :: Position2D -> Angle2D -> Shot
createShot position angle = Shot $ Shape position (toVelocity angle shotSpeed)

toDrawableShot :: Shot -> (FilePath, Maybe (Rectangle CInt), CDouble)
toDrawableShot (Shot shape) = toDrawableShape shape 0 size shotTextureFile

updateShot :: DeltaTime -> Shot -> Shot
updateShot dt (Shot (Shape position velocity)) =
    Shot $ Shape (updatePosition2D position velocity dt) velocity

isShotWithinBounds :: Shot -> Bounds2D -> Bool
isShotWithinBounds (Shot (Shape position _)) bounds =
    isWithinBounds2D position $ increaseBounds2D bounds size
