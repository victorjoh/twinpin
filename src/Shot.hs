module Shot
    ( Shot
    , shotTextureFile
    , initShot
    , drawShot
    , updateShot
    , isShotWithinBounds
    )
where

import           Time
import           Space
import           Shape
import           Foreign.C.Types
import           SDL
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromJust )

newtype Shot = Shot Shape

shotSpeed = 0.7

side :: Size1D
side = 32 / 3

size :: Size2D
size = V2 side side

shotTextureFile :: FilePath
shotTextureFile = "gen/shot.bmp"

initShot :: Position2D -> Angle2D -> Map.Map FilePath Texture -> Shot
initShot position angle textureMap = Shot $ Shape
    position
    (toVelocity angle shotSpeed)
    (fromJust (Map.lookup shotTextureFile textureMap))

drawShot :: Shot -> (Texture, Maybe (Rectangle CInt), CDouble)
drawShot (Shot shape) = drawShape shape 0 size

updateShot :: Shot -> DeltaTime -> Shot
updateShot (Shot (Shape position velocity texture)) dt =
    Shot $ Shape (updatePosition2D position velocity dt) velocity texture

isShotWithinBounds :: Shot -> Bounds2D -> Bool
isShotWithinBounds (Shot (Shape position _ _)) bounds =
    isWithinBounds2D position $ increaseBounds2D bounds size