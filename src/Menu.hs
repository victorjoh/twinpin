module Menu where

import           Space
import           Shot
import           Visual
import           SDL
import           Graphics.Text.TrueType         ( Font )
import           Codec.Picture.Types
import           Graphics.Rasterific     hiding ( V2(..) )
import qualified Graphics.Rasterific           as Rasterific
                                                ( V2(..) )
import           Graphics.Rasterific.Texture
import           Data.List                      ( foldl' )

data Menu = Resume | Quit deriving (Show, Eq)

menuSize :: Size2D
menuSize = V2 540 414

menuImageId = "menu"

getStaticMenuImage :: Font -> (ImageId, VectorImage)
getStaticMenuImage font =
    let menuLine x y = printTextAt font (PointSize 54) (Rasterific.V2 x y)
    in  ( menuImageId
        , VectorImage menuSize (backgroundColorAlpha 120)
            $ withTexture (uniformTexture $ PixelRGBA8 0xE6 0xE6 0xE6 255)
            $ do
                  menuLine 90  135 "paused"
                  menuLine 144 225 "resume"
                  menuLine 144 315 "quit"
        )

drawMenu :: Menu -> [(Rectangle Float, Either VectorImage ImageId)]
drawMenu menu =
    let selectionPosition = if menu == Resume then 540 else 630
    in  [ (Rectangle (P $ V2 690 333) menuSize, Right menuImageId)
        , drawShot $ createShot (V2 807 selectionPosition) 0 (-1)
        ]

updateMenu :: [Event] -> Menu -> Menu
updateMenu events menu = foldl' eventToMenu menu events

eventToMenu :: Menu -> Event -> Menu
eventToMenu fallback (Event _ (JoyAxisEvent axisEventData)) =
    let JoyAxisEventData _ axisId axisPos = axisEventData
        noiseThreshold                    = 5000
    in  if axisId == 1
            then if axisPos < -noiseThreshold
                then Resume
                else if axisPos > noiseThreshold then Quit else fallback
            else fallback
eventToMenu fallback (Event _ (JoyHatEvent (JoyHatEventData _ _ hatPosition)))
    = case hatPosition of
        HatUp   -> Resume
        HatDown -> Quit
        _       -> fallback
eventToMenu fallback (Event _ (KeyboardEvent eventData)) =
    let KeyboardEventData _ motion _ (Keysym (Scancode code) _ _) = eventData
    in  if motion == Pressed
            then case code of
                82 -> Resume
                81 -> Quit
                _  -> fallback
            else fallback
eventToMenu fallback _ = fallback
