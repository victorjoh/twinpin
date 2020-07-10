module Game
    ( Game(..)
    , GameState(..)
    , Menu(..)
    , Action(..)
    , pauseMenu
    , redWinMenu
    , blueWinMenu
    , tieMenu
    , createGame
    , getStaticImages
    , updateGame
    , drawGame
    , isFinished
    , assignJoysticks
    )
where

import           Match
import           Space
import           Visual
import           Menu
import           Player
import           SDL.Raw.Types                  ( JoystickID )
import           SDL
import           Foreign.C.Types
import           Graphics.Text.TrueType         ( Font )
import           Codec.Picture.Types
import           Data.Tuple.Extra               ( second )
import           Data.List                      ( foldl' )
import           Data.Bifunctor                 ( first
                                                , bimap
                                                )
import           Graphics.Rasterific     hiding ( V2(..) )
import qualified Graphics.Rasterific           as Rasterific
                                                ( V2(..) )
import           Graphics.Rasterific.Texture

-- collect the events when interrupted to get the correct initial state when
-- continuing
data GameState = Running Match
               | Interrupted Match (Menu Action) [Event]
               | Finished
                 deriving (Show, Eq)
data Game = Game Time GameState deriving (Show, Eq)

data Action = Resume | Restart | Quit deriving Eq

optionsButtonId = 9
psButtonId = 10
acceptButtonId = 0

boundsImageId = "bounds"

instance Show Action where
    show Resume  = "resume"
    show Restart = "restart"
    show Quit    = "quit"

pauseMenu, redWinMenu, blueWinMenu, tieMenu :: Menu Action
pauseMenu = Menu [("paused", defaultTextColor)] [Resume, Quit] 0
redWinMenu = winMenu ("red", aimColor Red)
blueWinMenu = winMenu ("blue", aimColor Blue)
tieMenu = gameOverMenu [("it's a draw!", defaultTextColor)]

winMenu playerText = gameOverMenu [playerText, (" wins!", defaultTextColor)]
gameOverMenu header = Menu header [Restart, Quit] 0

createGame :: Game
createGame = Game 0 $ Running createMatch

getStaticImages :: Font -> V2 CInt -> [(ImageId, Image PixelRGBA8)]
getStaticImages font winSize =
    let (ratio, offset) = getScaleRatioAndOffset matchSize winSize
        matchRect       = Rectangle (P $ V2 0 0) matchSize
        scaledRectangle = moveRectangle offset $ scaleRectangle ratio matchRect
    in  drawBounds winSize scaledRectangle ratio : map
            (second $ renderScaledVectorImage ratio)
            (  map (getStaticMenuImage font)
                   [pauseMenu, redWinMenu, blueWinMenu, tieMenu]
            ++ staticMatchImages font
            )

drawBounds
    :: V2 CInt -> Rectangle Float -> ScaleRatio -> (ImageId, Image PixelRGBA8)
drawBounds winSize matchRectangle ratio =
    let
        V2 windowWidthCInt windowHeightCInt = fromIntegral <$> winSize
        V2 windowWidth     windowHeight     = fromIntegral <$> winSize
        Rectangle (P (V2 matchX matchY)) (V2 matchWidth matchHeight) =
            matchRectangle
        frameWidth = ratio * 3
    in
        ( boundsImageId
        , renderDrawing windowWidthCInt windowHeightCInt transparent $ do
            withTexture (uniformTexture backgroundColorRasterific) $ do
                fill $ rectangle (Rasterific.V2 0 0) matchX windowHeight
                fill $ rectangle (Rasterific.V2 0 0) windowWidth matchY
                fill $ rectangle (Rasterific.V2 0 (matchY + matchHeight))
                                 windowWidth
                                 (windowHeight - matchY - matchHeight)
                fill $ rectangle (Rasterific.V2 (matchX + matchWidth) 0)
                                 (windowWidth - matchX - matchWidth)
                                 windowHeight
            withTexture (uniformTexture pillarColor)
                $ stroke frameWidth JoinRound (CapRound, CapRound)
                $ rectangle
                      (Rasterific.V2 (matchX - frameWidth / 2)
                                     (matchY - frameWidth / 2)
                      )
                      (matchWidth + frameWidth)
                      (matchHeight + frameWidth)
        )

transformToWindowArea
    :: V2 CInt
    -> (Rectangle Float, Either VectorImage ImageId)
    -> (Rectangle CInt, Either (Image PixelRGBA8) ImageId)
transformToWindowArea winSize =
    let (ratio, offset) = getScaleRatioAndOffset matchSize winSize
    in  bimap (fmap round . moveRectangle offset . scaleRectangle ratio)
              (first $ renderScaledVectorImage ratio)

drawGame
    :: V2 CInt -> Game -> [(Rectangle CInt, Either (Image PixelRGBA8) ImageId)]
drawGame winSize (Game _ state) =
    map (transformToWindowArea winSize) (drawGameState state)
        ++ [(Rectangle (P $ V2 0 0) winSize, Right boundsImageId)]

drawGameState :: GameState -> [(Rectangle Float, Either VectorImage ImageId)]
drawGameState (Running match           ) = drawMatch match
drawGameState (Interrupted match menu _) = drawMatch match ++ drawMenu menu
drawGameState Finished                   = []

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
updateGameState events _ (Interrupted match menu previousEvents) =
    Interrupted match (updateSelection events menu) (previousEvents ++ events)
updateGameState _ _ state = state

switchGameState :: [Event] -> GameState -> GameState
switchGameState events (Interrupted match menu previousEvents) =
    foldl' pausedEventToGameState (Interrupted match menu previousEvents) events
switchGameState events (Running match) =
    let winners = getWinners match
    in  if not $ null winners
            then Interrupted
                match
                (case winners of
                    [Red ] -> redWinMenu
                    [Blue] -> blueWinMenu
                    _      -> tieMenu
                )
                []
            else foldl' runningEventToGameState (Running match) events
switchGameState _ Finished = Finished

pausedEventToGameState :: GameState -> Event -> GameState
pausedEventToGameState gameState (Event _ (JoyButtonEvent buttonData)) =
    let JoyButtonEventData _ btnId btnState = buttonData
        xPressed = btnId == acceptButtonId && btnState == JoyButtonPressed
    in  chooseSelectedMenuEntryIf gameState xPressed
pausedEventToGameState gameState (Event _ (KeyboardEvent eventData)) =
    let KeyboardEventData _ motion _ (Keysym (Scancode code) _ _) = eventData
        enterPressed = motion == Pressed && code == 40
    in  chooseSelectedMenuEntryIf gameState enterPressed
pausedEventToGameState _         (Event _ (WindowClosedEvent _)) = Finished
pausedEventToGameState gameState _                               = gameState

chooseSelectedMenuEntryIf :: GameState -> Bool -> GameState
chooseSelectedMenuEntryIf (Interrupted match menu previousEvents) True =
    case getSelection menu of
        -- update so changes in input when paused will
        -- affect the match when unpaused
        Resume  -> Running $ updateMatch previousEvents 0 match
        -- set player ids to not loose joystick mappings
        Restart -> Running $ setPlayerIds (getPlayerIds match) createMatch
        Quit    -> Finished
chooseSelectedMenuEntryIf gameState True  = gameState
chooseSelectedMenuEntryIf gameState False = gameState

runningEventToGameState :: GameState -> Event -> GameState
runningEventToGameState (Running match) (Event _ (JoyButtonEvent buttonData)) =
    let JoyButtonEventData _ buttonId buttonState = buttonData
    in  if (buttonId == optionsButtonId || buttonId == psButtonId)
               && buttonState
               == JoyButtonPressed
            then Interrupted match pauseMenu []
            else Running match
runningEventToGameState (Running match) (Event _ (KeyboardEvent eventData)) =
    let KeyboardEventData _ motion _ (Keysym (Scancode code) _ _) = eventData
    in  if motion == Pressed && code == 41
            then Interrupted match pauseMenu []
            else Running match
runningEventToGameState _ (Event _ (WindowClosedEvent _)) = Finished
runningEventToGameState Finished _ = Finished
runningEventToGameState previousState _ = previousState

assignJoysticks :: [JoystickID] -> Game -> Game
assignJoysticks newJoysticks (Game lastUpdatedTime state) =
    Game lastUpdatedTime $ assignJoysticksToGameState newJoysticks state

assignJoysticksToGameState :: [JoystickID] -> GameState -> GameState
assignJoysticksToGameState newJoysticks (Running match) =
    Running $ assignJoysticksToMatch newJoysticks match
assignJoysticksToGameState newJoysticks (Interrupted match menu events) =
    Interrupted (assignJoysticksToMatch newJoysticks match) menu events
assignJoysticksToGameState _ Finished = Finished

isFinished :: Game -> Bool
isFinished (Game _ Finished) = True
isFinished _                 = False
