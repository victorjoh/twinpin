module Shot
    ( Shot
    , shotTextureFile
    , initShot
    , drawShot
    , updateShot
    , shotOutsideBounds
    )
where

import Time
import Space
import           Foreign.C.Types
import           SDL
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromJust )

data Shot = Shot Position2D Velocity2D Texture

shotSpeed = 0.7

side :: Size1D
side = 32 / 3

shotTextureFile :: FilePath
shotTextureFile = "gen/shot.bmp"

initShot :: Position2D -> Rotation2D -> Map.Map FilePath Texture -> Shot
initShot position angle textureMap = Shot
    position
    (toVelocity angle shotSpeed)
    (fromJust (Map.lookup shotTextureFile textureMap))

drawShot :: Shot -> (Texture, Maybe (Rectangle CInt), CDouble)
drawShot (Shot (Position2D x y) _ texture) =
    ( texture
    , Just (Rectangle
            (P (V2 (round (x - side/2)) (round (y - side/2))))
            (V2 (round side) (round side)))
    , 0
    )

updateShot :: Shot -> DeltaTime -> Shot
updateShot (Shot (Position2D xp yp) (Velocity2D xv yv) texture) td = Shot
    (Position2D (xp + xv * (fromIntegral td)) (yp + yv * (fromIntegral td)))
    (Velocity2D xv yv)
    texture

shotOutsideBounds :: Shot -> Bounds2D -> Bool
shotOutsideBounds (Shot (Position2D xp yp) _ _) (Bounds2D xb yb) =
    shotOutsideBounds1D xp xb || shotOutsideBounds1D yp yb

shotOutsideBounds1D :: Position1D -> Bounds1D -> Bool
shotOutsideBounds1D pos (lower, upper) =
    pos + side/2 < lower || pos - side/2 > upper