module Shot
    ( Shot(..)
    , ShotState(..)
    , shotSpeed
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
import           Graphics.Rasterific     hiding ( V2(..) )
import qualified Graphics.Rasterific           as Rasterific
                                                ( V2(..) )
import           Graphics.Rasterific.Texture
import           Codec.Picture.Types

data Shot = Shot Circle Velocity2D ShotState deriving (Show, Eq)
data ShotState = HasHitPlayer | HasNotHitPlayer deriving (Show, Eq)

shotSpeed :: Speed
shotSpeed = 0.7

side :: Size1D
side = 32 / 3

size :: Size2D
size = V2 side side

createShot :: Position2D -> Angle2D -> Shot
createShot position angle = Shot (Circle position (side / 2))
                                 (toVelocity angle shotSpeed)
                                 HasNotHitPlayer

toDrawableShot :: Shot -> (Rectangle CInt, Image PixelRGBA8)
toDrawableShot (Shot circle _ state) =
    toSolidCircleTexture (shotColor state) circle

shotColor :: ShotState -> PixelRGBA8
shotColor HasHitPlayer    = PixelRGBA8 0xd3 0x5f 0x5f 255
shotColor HasNotHitPlayer = PixelRGBA8 0xff 0xe6 0x80 255

updateShot :: DeltaTime -> Shot -> Shot
updateShot dt (Shot circle velocity state) =
    Shot (updateCirclePosition velocity dt circle) velocity state

isShotWithinBounds :: Bounds2D -> Shot -> Bool
isShotWithinBounds bounds (Shot circle _ _) =
    isCircleWithinBounds bounds circle

shotToCircle :: Shot -> Circle
shotToCircle (Shot circle _ _) = circle

setShotHit :: Shot -> Shot
setShotHit (Shot circle velocity _) = Shot circle velocity HasHitPlayer
