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
    , circleSector
    , scaleAndOffset
    , breakCubicBezierAt
    , breakLineAt
    , bezierCircle
    , circleQuadrant1
    , circleQuadrant2
    , circleQuadrant3
    , circleQuadrant4
    )
where

import           Space
import           Codec.Picture.Types
import           Graphics.Rasterific     hiding ( V2(..) )
import qualified Graphics.Rasterific           as R
                                                ( V2(..) )
import           Graphics.Rasterific.Linear     ( (^+^)
                                                , (^*)
                                                , lerp
                                                )
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

instance Show VectorImage where
    show (VectorImage size color _) =
        "VectorImage (" ++ show size ++ ") (" ++ show color ++ ") _"

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

scaleAndOffset :: Transformable a => Size1D -> a -> a
scaleAndOffset s = transform ((^+^ R.V2 s s) . (^* s))

--         , - ~ ~ ~ - ,
--     , '       |       ' ,
--   ,           |           ,
--  ,     2      |      1     ,
-- ,             |             ,
-- ,-------------+-------------,
-- ,             |             ,
--  ,     3      |      4     ,
--   ,           |           ,
--     ,         |        , '
--       ' - , _ _ _ ,  '
circleQuadrant1, circleQuadrant2, circleQuadrant3, circleQuadrant4
    :: CubicBezier
circleQuadrant1 = CubicBezier (R.V2 1 0)
                              (R.V2 1 (-bezierCircleConstant))
                              (R.V2 bezierCircleConstant (-1))
                              (R.V2 0 (-1))
circleQuadrant2 = CubicBezier (R.V2 0 (-1))
                              (R.V2 (-bezierCircleConstant) (-1))
                              (R.V2 (-1) (-bezierCircleConstant))
                              (R.V2 (-1) 0)
circleQuadrant3 = CubicBezier (R.V2 (-1) 0)
                              (R.V2 (-1) bezierCircleConstant)
                              (R.V2 (-bezierCircleConstant) 1)
                              (R.V2 0 1)
circleQuadrant4 = CubicBezier (R.V2 0 1)
                              (R.V2 bezierCircleConstant 1)
                              (R.V2 1 bezierCircleConstant)
                              (R.V2 1 0)
bezierCircleConstant = 0.551915024494

bezierCircle :: [CubicBezier]
bezierCircle =
    [circleQuadrant4, circleQuadrant3, circleQuadrant2, circleQuadrant1]

-- copied from Graphics.Rasterific.CubicBezier since it is in an unexposed
-- module
-- https://github.com/Twinside/Rasterific/blob/d607a5916a840c173c4a6c60f52c7e1a1533544e/src/Graphics/Rasterific/CubicBezier.hs#L260
breakCubicBezierAt :: CubicBezier -> Float -> (CubicBezier, CubicBezier)
breakCubicBezierAt (CubicBezier a b c d) val =
    (CubicBezier a ab abbc abbcbccd, CubicBezier abbcbccd bccd cd d)
  where
    ab       = lerp val b a
    bc       = lerp val c b
    cd       = lerp val d c

    abbc     = lerp val bc ab
    bccd     = lerp val cd bc
    abbcbccd = lerp val bccd abbc

breakLineAt :: Line -> Float -> (Line, Line)
breakLineAt (Line a b) t = (Line a ab, Line ab b) where ab = lerp t b a

circleSector :: Angle2D -> [Either CubicBezier Line]
circleSector a
    | a <= 0
    = []
    | a > 0 && a < 2 * pi
    = let nbrOfQuadrants = ceiling $ a * 2 / pi
          quadrants      = drop (4 - nbrOfQuadrants) bezierCircle
          cutQuadrant    = fst $ breakCubicBezierAt
              (head quadrants)
              ((a - fromIntegral (nbrOfQuadrants - 1) * pi / 2) * 2 / pi)
          orig = R.V2 0 0
      in  Right (Line orig (R.V2 1 0))
              : Right (Line (_cBezierX3 cutQuadrant) orig)
              : map Left (cutQuadrant : tail quadrants)
    | otherwise
    = map Left bezierCircle
