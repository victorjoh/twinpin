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
menuSize = V2 300 230

menuImageId = "menu"

getStaticMenuImage :: Font -> (ImageId, VectorImage)
getStaticMenuImage font =
    let menuLine x y = printTextAt font (PointSize 30) (Rasterific.V2 x y)
    in  ( menuImageId
        , VectorImage menuSize (backgroundColorAlpha 120)
            $ withTexture (uniformTexture $ PixelRGBA8 0xE6 0xE6 0xE6 255)
            $ do
                  menuLine 50 75  "paused"
                  menuLine 80 125 "resume"
                  menuLine 80 175 "quit"
        )

drawMenu :: Menu -> [(Rectangle Float, Either VectorImage ImageId)]
drawMenu menu =
    let selectionPosition = if menu == Resume then 300 else 350
    in  [ (Rectangle (P $ V2 250 185) menuSize, Right menuImageId)
        , drawShot $ createShot (V2 315 selectionPosition) 0
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