module Shape where

import           Space
import           Foreign.C.Types
import           SDL.Video.Renderer             ( Rectangle(..) )

data Shape = Shape Position2D Velocity2D

-- Converts to something that is easily drawable by SDL. Shape has coordinates 
-- on the middle of the texture, whereas the SDL representation has the
-- coordinates specified on the top left corner.
toDrawableShape
    :: Shape
    -> Angle2D -- texture rotation in clockwise degrees
    -> Size2D -- texture size in pixels
    -> FilePath
    -> (FilePath, Maybe (Rectangle CInt), CDouble)
toDrawableShape (Shape position _) angle size textureFile =
    ( textureFile
    , Just (Rectangle (toPixelPoint (position - size / 2)) (toPixelSize size))
    , toPixelAngle angle
    )
