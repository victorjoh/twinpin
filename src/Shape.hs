module Shape where

import           Space
import           Foreign.C.Types
import           SDL.Video.Renderer             ( Texture
                                                , Rectangle(..)
                                                )

data Shape = Shape Position2D Velocity2D Texture

drawShape
    :: Shape
    -> Angle2D -- texture rotation in clockwise degrees
    -> Size2D -- texture size in pixels
    -> (Texture, Maybe (Rectangle CInt), CDouble)
drawShape (Shape position _ texture) angle size =
    ( texture
    , Just (Rectangle
            (toPixelPoint (position - size / 2))
            (toPixelSize size))
    , toPixelAngle angle
    )
