module Shot
    ( Shot(..)
    , ShotState(..)
    , ShotId
    , Health
    , shotDamage
    , staticShotImages
    , shotSpeed
    , shotRadius
    , createShot
    , drawShot
    , updateShot
    , isShotWithinBounds
    , shotToCircle
    , setShotHit
    , getShotId
    )
where

import           Space
import           Circle
import           Visual
import           SDL
import           Codec.Picture.Types

type ShotId = Int
data Shot = Shot Circle Velocity2D ShotState ShotId deriving (Show, Eq)
data ShotState = HasHitPlayer | HasNotHitPlayer deriving (Show, Eq)

type Health = Float
shotDamage :: Health
shotDamage = 0.15

shotSpeed :: Speed
shotSpeed = 1.26

shotRadius :: Radius
shotRadius = 10

hasNotHitPlayerImageId = "hasNotHitPlayerShot"
hasHitPlayerImageId = "hasHitPlayerShot"

createShot :: Position2D -> Angle2D -> ShotId -> Shot
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
drawShot (Shot circle _ state _) =
    ( toTextureArea circle
    , case state of
        HasNotHitPlayer -> Right hasNotHitPlayerImageId
        HasHitPlayer    -> Right hasHitPlayerImageId
    )

updateShot :: DeltaTime -> Shot -> Shot
updateShot dt (Shot circle velocity state shotId) =
    Shot (updateCirclePosition velocity dt circle) velocity state shotId

isShotWithinBounds :: Bounds2D -> Shot -> Bool
isShotWithinBounds bounds (Shot circle _ _ _) =
    isCircleWithinBounds bounds circle

shotToCircle :: Shot -> Circle
shotToCircle (Shot circle _ _ _) = circle

setShotHit :: Shot -> Shot
setShotHit (Shot circle velocity _ shotId) =
    Shot circle velocity HasHitPlayer shotId

getShotId :: Shot -> ShotId
getShotId (Shot _ _ _ shotId) = shotId
