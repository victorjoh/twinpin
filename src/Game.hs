module Game
    ( Game(..)
    , GameState(..)
    , Menu(..)
    , createGame
    , updateGame
    , drawGame
    , isFinished
    )
where

import           Match
import           Space
import           Shot
import           SDL                     hiding ( Paused )
import           Foreign.C.Types
import           Graphics.Text.TrueType         ( Font )
import           Graphics.Rasterific     hiding ( V2(..) )
import qualified Graphics.Rasterific           as Rasterific
                                                ( V2(..) )
import           Graphics.Rasterific.Texture
import           Codec.Picture.Types
import           Data.Tuple.Extra               ( second )
import           Data.List                      ( foldl' )

data Menu = Resume | Quit deriving (Show, Eq)
-- collect the events when paused to get the correct initial state when
-- unpausing
data GameState = Running Match | Paused Match Menu [Event] | Finished
                 deriving (Show, Eq)
data Game = Game Time GameState deriving (Show, Eq)

optionsButtonId = 9
psButtonId = 10
cancelButtonId = 1
acceptButtonId = 0

createGame :: V2 CInt -> Game
createGame boundsCInt =
    Game 0 $ Running (createMatch (fromIntegral <$> boundsCInt))

drawGame :: Game -> [(Rectangle CInt, Font -> Image PixelRGBA8)]
drawGame (Game _ (Running match)) = map (second const) $ drawMatch match
drawGame (Game _ (Paused match menu _)) =
    let
        selectionPosition = if menu == Resume then 115 else 165
        menuWidth         = 300
        menuHeight        = 230
        textSize          = PointSize 30
    in
        map (second const) (drawMatch match)
            ++ [ ( Rectangle (P $ V2 250 185) (V2 menuWidth menuHeight)
                 , \font ->
                     renderDrawing (fromIntegral menuWidth)
                                   (fromIntegral menuHeight)
                                   (PixelRGBA8 34 11 21 120)
                         $ do
                               withTexture
                                       (uniformTexture
                                           (PixelRGBA8 0xE6 0xE6 0xE6 255)
                                       )
                                   $ do
                                         printTextAt font
                                                     textSize
                                                     (Rasterific.V2 50 75)
                                                     "paused"
                                         printTextAt font
                                                     textSize
                                                     (Rasterific.V2 80 125)
                                                     "resume"
                                         printTextAt font
                                                     textSize
                                                     (Rasterific.V2 80 175)
                                                     "quit"
                               withTexture
                                       (uniformTexture
                                           (shotColor HasNotHitPlayer)
                                       )
                                   $ fill
                                   $ circle
                                         (Rasterific.V2 65 selectionPosition)
                                         shotRadius
                 )
               ]
drawGame (Game _ Finished) = []

updateGame :: [Event] -> Time -> Game -> Game
updateGame events newTime oldGame =
    let Game oldTime oldState = oldGame
        passedTime            = newTime - oldTime
        newGameState          = switchGameState events
            $ updateGameState events passedTime oldState
    in  Game newTime newGameState

updateGameState :: [Event] -> DeltaTime -> GameState -> GameState
updateGameState events passedTime (Running match) =
    Running $ updateMatch events passedTime match
updateGameState events passedTime (Paused match menu previousEvents) =
    Paused match (updateMenu events menu) (previousEvents ++ events)
updateGameState _ _ state = state

updateMenu :: [Event] -> Menu -> Menu
updateMenu events menu = foldl' eventToMenu menu events

eventToMenu :: Menu -> Event -> Menu
eventToMenu fallback (Event _ (JoyAxisEvent axisEventData)) =
    let JoyAxisEventData _ axisId axisPosition = axisEventData
    in  if axisId == 1
            then if axisPosition < -5000
                then Resume
                else if axisPosition > 5000 then Quit else fallback
            else fallback
eventToMenu fallback (Event _ (JoyHatEvent (JoyHatEventData _ _ hatPosition)))
    = case hatPosition of
        HatUp   -> Resume
        HatDown -> Quit
        _       -> fallback
eventToMenu fallback _ = fallback

switchGameState :: [Event] -> GameState -> GameState
switchGameState events (Paused match menu previousEvents) =
    foldl' pausedEventToGameState (Paused match menu previousEvents) events
switchGameState events (Running match) =
    foldl' runningEventToGameState (Running match) events
switchGameState _ Finished = Finished

pausedEventToGameState :: GameState -> Event -> GameState
pausedEventToGameState gameState (Event _ (JoyButtonEvent buttonData)) =
    let JoyButtonEventData _ buttonId buttonState = buttonData
    in  case gameState of
            Paused match menu previousEvents ->
                if buttonId == acceptButtonId && buttonState == JoyButtonPressed
                    then case menu of
                        Resume -> Running (updateMatch previousEvents 0 match)
                        Quit   -> Finished
                    else Paused match menu previousEvents
            _ -> gameState
pausedEventToGameState _         (Event _ (WindowClosedEvent _)) = Finished
pausedEventToGameState gameState _                               = gameState

runningEventToGameState :: GameState -> Event -> GameState
runningEventToGameState (Running match) (Event _ (JoyButtonEvent buttonData)) =
    let JoyButtonEventData _ buttonId buttonState = buttonData
    in  if (buttonId == optionsButtonId || buttonId == psButtonId)
               && buttonState
               == JoyButtonPressed
            then Paused match Resume []
            else Running match
runningEventToGameState _ (Event _ (WindowClosedEvent _)) = Finished
runningEventToGameState Finished _ = Finished
runningEventToGameState previousState _ = previousState

isFinished :: Game -> Bool
isFinished (Game _ Finished) = True
isFinished _                 = False
