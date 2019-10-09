module Circle where

import           Space
import           Foreign.C.Types
import           SDL.Video.Renderer             ( Rectangle(..) )
import           SDL.Vect

type Radius = Float
data Circle = Circle Position2D Radius deriving (Show, Eq)

-- Converts to something that is easily drawable by SDL. Circle has coordinates 
-- on the middle of the texture, whereas the SDL representation has the
-- coordinates specified on the top left corner.
toDrawableCircle
    :: Circle
    -> Angle2D -- texture rotation in clockwise degrees
    -> FilePath
    -> (FilePath, Maybe (Rectangle CInt), CDouble)
toDrawableCircle (Circle position radius) angle textureFile =
    ( textureFile
    , Just
        (Rectangle (toPixelPoint (position - radius'))
                   (toPixelSize (radius' * 2))
        )
    , toPixelAngle angle
    )
    where radius' = (V2 radius radius)

areIntersecting :: Circle -> Circle -> Bool
areIntersecting (Circle (V2 x1 y1) r1) (Circle (V2 x2 y2) r2) =
    (r1 + r2) ^ 2 > (x2 - x1) ^ 2 + (y2 - y1) ^ 2
