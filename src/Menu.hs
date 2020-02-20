module Menu where

import           Space
import           Bullet
import           Visual
import           SDL
import           Graphics.Text.TrueType         ( Font )
import           Codec.Picture.Types
import           Graphics.Rasterific     hiding ( V2(..) )
import qualified Graphics.Rasterific           as Rasterific
                                                ( V2(..) )
import           Graphics.Rasterific.Texture
import           Data.List                      ( foldl' )

type Header = [(String, PixelRGBA8)]
type ContinueName = String
data Selection = Continue | Quit deriving (Show, Eq)
data Menu = Menu Header ContinueName Selection deriving (Show, Eq)

menuSize :: Size2D
menuSize = V2 540 414

defaultTextColor = PixelRGBA8 0xE6 0xE6 0xE6 255

getMenuImageId :: Header -> ImageId
getMenuImageId header = "menu-" ++ concatMap fst header

getStaticMenuImage :: Font -> Menu -> (ImageId, VectorImage)
getStaticMenuImage font (Menu header continueName _) =
    let
        textSize = PointSize 54
        menuLine x y = printTextAt font textSize (Rasterific.V2 x y)
    in
        ( getMenuImageId header
        , VectorImage menuSize (backgroundColorAlpha 120)
        $ withTexture (uniformTexture defaultTextColor)
        $ do
              printTextRanges (Rasterific.V2 90 135) $ map
                  (\(text, color) -> TextRange font
                                               textSize
                                               text
                                               (Just $ uniformTexture color)
                  )
                  header
              menuLine 144 225 continueName
              menuLine 144 315 "quit"
        )

drawMenu :: Menu -> [(Rectangle Float, Either VectorImage ImageId)]
drawMenu (Menu header _ selection) =
    let selectionPosition = if selection == Continue then 540 else 630
    in  [ (Rectangle (P $ V2 690 333) menuSize, Right $ getMenuImageId header)
        , drawBullet $ createBullet (V2 807 selectionPosition) 0 (-1)
        ]

updateSelection :: [Event] -> Menu -> Menu
updateSelection events (Menu header continueName previous) =
    Menu header continueName $ foldl' eventToSelection previous events

eventToSelection :: Selection -> Event -> Selection
eventToSelection fallback (Event _ (JoyAxisEvent axisEventData)) =
    let JoyAxisEventData _ axisId axisPos = axisEventData
        noiseThreshold                    = 5000
    in  if axisId == 1
            then if axisPos < -noiseThreshold
                then Continue
                else if axisPos > noiseThreshold then Quit else fallback
            else fallback
eventToSelection fallback (Event _ (JoyHatEvent hatEventData)) =
    let JoyHatEventData _ _ hatPosition = hatEventData
    in  case hatPosition of
            HatUp   -> Continue
            HatDown -> Quit
            _       -> fallback
eventToSelection fallback (Event _ (KeyboardEvent eventData)) =
    let KeyboardEventData _ motion _ (Keysym (Scancode code) _ _) = eventData
    in  if motion == Pressed
            then case code of
                82 -> Continue
                81 -> Quit
                _  -> fallback
            else fallback
eventToSelection fallback _ = fallback
