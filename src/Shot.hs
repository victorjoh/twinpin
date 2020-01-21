module Shot
    ( Shot(..)
    , ShotState(..)
    , staticShotImages
    , shotSpeed
    , shotRadius
    , createShot
    , drawShot
    , updateShot
    , isShotWithinBounds
    , shotToCircle
    , setShotHit
    )
where

import           Space
import           Circle
import           Visual
import           SDL
import           Codec.Picture.Types

data Shot = Shot Circle Velocity2D ShotState deriving (Show, Eq)
data ShotState = HasHitPlayer | HasNotHitPlayer deriving (Show, Eq)

shotSpeed :: Speed
shotSpeed = 0.7

shotRadius :: Radius
shotRadius = 16 / 3

hasNotHitPlayerImageId = "hasNotHitPlayerShot"
hasHitPlayerImageId = "hasHitPlayerShot"

createShot :: Position2D -> Angle2D -> Shot
createShot position direction = Shot (Circle position shotRadius)
                                     (toVelocity direction shotSpeed)
                                     HasNotHitPlayer

staticShotImages :: [(ImageId, VectorImage)]
staticShotImages =
    [ (imageId, toSolidCircleImage color shotRadius)
    | (imageId, color) <-
        [ (hasNotHitPlayerImageId, PixelRGBA8 0xff 0xe6 0x80 255)
        , (hasHitPlayerImageId   , PixelRGBA8 0xd3 0x5f 0x5f 255)
        ]
    ]

drawShot :: Shot -> (Rectangle Float, Either VectorImage ImageId)
drawShot (Shot circle _ state) =
    ( toTextureArea circle
    , case state of
        HasNotHitPlayer -> Right hasNotHitPlayerImageId
        HasHitPlayer    -> Right hasHitPlayerImageId
    )

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
