module Bullet
    ( Bullet(..)
    , BulletState(..)
    , BulletId
    , Health
    , bulletDamage
    , staticBulletImages
    , bulletSpeed
    , bulletRadius
    , createBullet
    , drawBullet
    , moveBullet
    , isBulletWithinBounds
    , bulletToCircle
    , setBulletHit
    , getBulletId
    )
where

import           Space
import           Circle
import           Visual
import           SDL
import           Codec.Picture.Types

type BulletId = Int
data Bullet = Bullet Circle Velocity2D BulletState BulletId deriving (Show, Eq)
data BulletState = HasHitPlayer | HasNotHitPlayer deriving (Show, Eq)

type Health = Float
bulletDamage :: Health
bulletDamage = 0.15

bulletSpeed :: Speed
bulletSpeed = 1.26

bulletRadius :: Radius
bulletRadius = 10

hasNotHitPlayerImageId = "hasNotHitPlayerBullet"
hasHitPlayerImageId = "hasHitPlayerBullet"

createBullet :: Position2D -> Angle2D -> BulletId -> Bullet
createBullet position direction = Bullet (Circle position bulletRadius)
                                         (toVelocity direction bulletSpeed)
                                         HasNotHitPlayer

staticBulletImages :: [(ImageId, VectorImage)]
staticBulletImages =
    [ (imageId, toSolidCircleImage color bulletRadius)
    | (imageId, color) <-
        [ (hasNotHitPlayerImageId, PixelRGBA8 0xff 0xe6 0x80 255)
        , (hasHitPlayerImageId   , PixelRGBA8 0xd3 0x5f 0x5f 255)
        ]
    ]

drawBullet :: Bullet -> (Rectangle Float, Either VectorImage ImageId)
drawBullet (Bullet circle _ state _) =
    ( toTextureArea circle
    , case state of
        HasNotHitPlayer -> Right hasNotHitPlayerImageId
        HasHitPlayer    -> Right hasHitPlayerImageId
    )

moveBullet :: DeltaTime -> Bullet -> Bullet
moveBullet dt (Bullet circle velocity state bulletId) =
    Bullet (moveCircle velocity dt circle) velocity state bulletId

isBulletWithinBounds :: Bounds2D -> Bullet -> Bool
isBulletWithinBounds bounds (Bullet circle _ _ _) =
    isCircleWithinBounds bounds circle

bulletToCircle :: Bullet -> Circle
bulletToCircle (Bullet circle _ _ _) = circle

setBulletHit :: Bullet -> Bullet
setBulletHit (Bullet circle velocity _ bulletId) =
    Bullet circle velocity HasHitPlayer bulletId

getBulletId :: Bullet -> BulletId
getBulletId (Bullet _ _ _ bulletId) = bulletId
