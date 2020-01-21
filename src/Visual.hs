module Visual
    ( ImageId
    , VectorImage(..)
    , ScaleRatio
    , transparent
    , backgroundColorAlpha
    , backgroundColorRasterific
    , backgroundColorSDL
    , renderScaledVectorImage
    , getScaleRatioAndOffset
    , moveRectangle
    , scaleRectangle
    )
where

import           Space
import           Codec.Picture.Types
import           Graphics.Rasterific     hiding ( V2(..) )
import           Graphics.Rasterific.Transformations
import           SDL                            ( V2(..)
                                                , Point(..)
                                                , V4(..)
                                                )
import           SDL.Video.Renderer             ( Rectangle(..) )
import           Foreign.C.Types                ( CInt )
import           Data.Word                      ( Word8 )

type ImageId = String
data VectorImage = VectorImage Size2D PixelRGBA8 (Drawing PixelRGBA8 ())
type ScaleRatio = Float

transparent :: PixelRGBA8
transparent = PixelRGBA8 255 255 255 0

createBackgroundColor :: (Word8 -> Word8 -> Word8 -> Word8 -> a) -> Word8 -> a
createBackgroundColor typeConstructor = typeConstructor 34 11 21

backgroundColorAlpha :: Pixel8 -> PixelRGBA8
backgroundColorAlpha = createBackgroundColor PixelRGBA8

backgroundColorSDL :: V4 Word8
backgroundColorSDL = createBackgroundColor V4 255

backgroundColorRasterific :: PixelRGBA8
backgroundColorRasterific = backgroundColorAlpha 255

renderScaledVectorImage :: ScaleRatio -> VectorImage -> Image PixelRGBA8
renderScaledVectorImage ratio (VectorImage nominalSize background drawing) =
    let (V2 scaledWidth scaledHeight) = round <$> ratio @* nominalSize
    in  renderDrawing scaledWidth scaledHeight background
            $ withTransformation (scale ratio ratio) drawing

-- Get the ratio for conversion from the visual drawing area to the pixel area.
-- The aspect ratio of the visual drawing area is kept the same as before. The
-- offset is the top left corner of the centered visual drawing area when scaled
-- to fit the pixel area. It will either be on the form V2 x 0 or V2 0 x.
getScaleRatioAndOffset :: Size2D -> V2 CInt -> (ScaleRatio, V2 Float)
getScaleRatioAndOffset (V2 drawingAreaWidth drawingAreaHeight) pixelArea =
    let V2 pixelAreaWidth pixelAreaHeight = fromIntegral <$> pixelArea
        pixelAspectRatio                  = pixelAreaWidth / pixelAreaHeight
        drawingAspectRatio                = drawingAreaWidth / drawingAreaHeight
        horizontalAxes                    = (pixelAreaWidth, drawingAreaWidth)
        verticalAxes                      = (pixelAreaHeight, drawingAreaHeight)
    in  if pixelAspectRatio > drawingAspectRatio
            then getScaleRatioAndOffset' verticalAxes horizontalAxes (flip V2 0)
            else getScaleRatioAndOffset' horizontalAxes verticalAxes (V2 0)

getScaleRatioAndOffset'
    :: (Float, Size1D)
    -> (Float, Size1D)
    -> (Float -> V2 Float)
    -> (ScaleRatio, V2 Float)
getScaleRatioAndOffset' ratioAxes offsetAxes toVector =
    let (pixelRatioAxis, drawingRatioAxis) = ratioAxes
        ratio                              = pixelRatioAxis / drawingRatioAxis
        (pixelOffsetAxis, drawingOffsetAxis) = offsetAxes
        offset = (pixelOffsetAxis - drawingOffsetAxis * ratio) / 2
    in  (ratio, toVector offset)

moveRectangle :: Num a => V2 a -> Rectangle a -> Rectangle a
moveRectangle distance (Rectangle (P pos) size) =
    Rectangle (P $ distance + pos) size

scaleRectangle :: Num a => a -> Rectangle a -> Rectangle a
scaleRectangle factor (Rectangle (P pos) size) =
    let factor2D = V2 factor factor
    in  Rectangle (P $ factor2D * pos) (factor2D * size)
