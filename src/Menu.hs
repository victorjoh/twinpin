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
import           Control.Monad                  ( zipWithM_ )

type Header = [(String, PixelRGBA8)]
-- type ContinueName = String
-- data Selection = Continue | Quit deriving (Show, Eq)
data Menu a = Menu Header [a] Int deriving (Show, Eq)

menuSize :: Size2D
menuSize = V2 540 414

defaultTextColor = PixelRGBA8 0xE6 0xE6 0xE6 255

getMenuImageId :: Header -> ImageId
getMenuImageId header = "menu-" ++ concatMap fst header

textSize :: PointSize
textSize = PointSize 54

getStaticMenuImage :: Show a => Font -> Menu a -> (ImageId, VectorImage)
getStaticMenuImage font (Menu header choices _) =
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
              menuRows font choices
    )

menuRows :: Show a => Font -> [a] -> Drawing PixelRGBA8 ()
menuRows font choices = zipWithM_ (menuRow font) choices $ iterate (+ 90) 225

menuRow :: Show a => Font -> a -> Position1D -> Drawing PixelRGBA8 ()
menuRow font row y = printTextAt font textSize (Rasterific.V2 144 y) $ show row

drawMenu :: Menu a -> [(Rectangle Float, Either VectorImage ImageId)]
drawMenu (Menu header _ selection) =
    let selectionPosition = fromIntegral $ 540 + selection * 90
    in  [ (Rectangle (P $ V2 690 333) menuSize, Right $ getMenuImageId header)
        , drawBullet $ createBullet (V2 807 selectionPosition) 0 (-1)
        ]

getSelection :: Menu a -> a
getSelection (Menu _ choices selection) = choices !! selection

updateSelection :: [Event] -> Menu a -> Menu a
updateSelection events (Menu header choices previous) =
    Menu header choices
        $ foldl' (eventToSelection $ length choices) previous events

eventToSelection :: Int -> Int -> Event -> Int
eventToSelection nbrOfChoices current event =
    limit (0, nbrOfChoices - 1) $ current + eventToChange event

eventToChange :: Event -> Int
eventToChange (Event _ (JoyAxisEvent axisEventData)) =
    let JoyAxisEventData _ axisId axisPos = axisEventData
        noiseThreshold                    = 5000
    in  if axisId == 1
            then if axisPos < -noiseThreshold
                then -1
                else if axisPos > noiseThreshold then 1 else 0
            else 0
eventToChange (Event _ (JoyHatEvent hatEventData)) =
    let JoyHatEventData _ _ hatPosition = hatEventData
    in  case hatPosition of
            HatUp   -> -1
            HatDown -> 1
            _       -> 0
eventToChange (Event _ (KeyboardEvent eventData)) =
    let KeyboardEventData _ motion _ (Keysym (Scancode code) _ _) = eventData
    in  if motion == Pressed
            then case code of
                82 -> -1
                81 -> 1
                _  -> 0
            else 0
eventToChange _ = 0

