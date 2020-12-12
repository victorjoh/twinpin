module Game
    ( Game(..)
    , GameState(..)
    , Menu(..)
    , MainAction(..)
    , InterruptedAction(..)
    , mainMenu
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
import           Data.List                      ( foldl'
                                                , delete
                                                )
import           Data.Bifunctor                 ( first
                                                , bimap
                                                )
import           Graphics.Rasterific     hiding ( V2(..) )
import qualified Graphics.Rasterific           as Rasterific
                                                ( V2(..) )
import           Graphics.Rasterific.Texture

-- collect the events when interrupted to get the correct initial state when
-- continuing
--data GameState = Start MainMenu
data GameState = MainMenu (Menu MainAction) [JoystickID]
               | Running Match
               | Interrupted Match (Menu InterruptedAction) [Event]
               | Finished
                 deriving (Show, Eq)
data Game = Game Time GameState deriving (Show, Eq)

data MainAction = NewMatch | MainQuit deriving Eq
data InterruptedAction = Resume | Restart | PauseQuit deriving Eq

--data MainMenu = MainMenu [JoystickID]
--
--data Menu a = Menu a [a]
--data MainNode = NewGame [MainNode] | Quit [MainNode] 
--              | NumberOfPlayers Int | Start | Back | Yes | No
--
--mainMenu' :: Root No [
--        NewGame [
--            NumberOfPlayers 2,
--            Start,
--            Back
--        ],
--        Quit [No, Yes]
--    ]

optionsButtonId = 9
psButtonId = 10
acceptButtonId = 0
enterKeyCode = 40
escapeKeyCode = 41

boundsImageId = "bounds"

instance Show MainAction where
    show NewMatch = "new game"
    show MainQuit = "quit"

instance Show InterruptedAction where
    show Resume    = "resume"
    show Restart   = "restart"
    show PauseQuit = "quit"

mainMenu :: Menu MainAction
mainMenu = uncoloredMenu "Twinpin" [NewMatch, MainQuit]

pauseMenu, redWinMenu, blueWinMenu, tieMenu :: Menu InterruptedAction
pauseMenu = uncoloredMenu "paused" [Resume, PauseQuit]
redWinMenu = winMenu ("red", aimColor Red)
blueWinMenu = winMenu ("blue", aimColor Blue)
tieMenu = uncoloredMenu "it's a draw!" [Restart, PauseQuit]

uncoloredMenu headerText choices =
    Menu [(headerText, defaultTextColor)] choices (head choices)
winMenu playerText =
    Menu [playerText, (" wins!", defaultTextColor)] [Restart, PauseQuit] Restart

createGame :: Game
createGame = Game 0 $ MainMenu mainMenu []

getStaticImages :: Font -> V2 CInt -> [(ImageId, Image PixelRGBA8)]
getStaticImages font winSize =
    let (ratio, offset) = getScaleRatioAndOffset matchSize winSize
        matchRect       = Rectangle (P $ V2 0 0) matchSize
        scaledRectangle = moveRectangle offset $ scaleRectangle ratio matchRect
    in  drawBounds winSize scaledRectangle ratio : map
            (second $ renderScaledVectorImage ratio)
            (  map (getStaticMenuImage font)
                   [pauseMenu, redWinMenu, blueWinMenu, tieMenu]
            ++ [getStaticMenuImage font mainMenu]
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
drawGameState (MainMenu menu _         ) = drawMenu menu
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
updateGameState events _ (MainMenu menu joystickIds) = MainMenu
    (updateSelection events menu)
    (removeDisconnectedJoysticks events joystickIds)
updateGameState events passedTime (Running match) =
    Running $ updateMatch events passedTime match
updateGameState events _ (Interrupted match menu previousEvents) =
    Interrupted match (updateSelection events menu) (previousEvents ++ events)
updateGameState _ _ state = state

removeDisconnectedJoysticks :: [Event] -> [JoystickID] -> [JoystickID]
removeDisconnectedJoysticks events joystickIds =
    foldl' removeDisconnectedJoystick joystickIds events

removeDisconnectedJoystick :: [JoystickID] -> Event -> [JoystickID]
removeDisconnectedJoystick joystickIds event = case event of
    Event _ (JoyDeviceEvent (JoyDeviceEventData JoyDeviceRemoved joystickId))
        -> delete joystickId joystickIds
    _ -> joystickIds 

switchGameState :: [Event] -> GameState -> GameState
switchGameState events state@MainMenu{} =
    foldl' menuEventToGameState state events
switchGameState events state@Interrupted{} =
    foldl' menuEventToGameState state events
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

menuEventToGameState :: GameState -> Event -> GameState
menuEventToGameState state event
    | isAcceptPressed event = chooseSelectedMenuEntry state
    | isWindowClosed event  = Finished
    | otherwise             = state

isAcceptPressed :: Event -> Bool
isAcceptPressed (Event _ (JoyButtonEvent eventData)) =
    let JoyButtonEventData _ btnId btnState = eventData
    in  btnId == acceptButtonId && btnState == JoyButtonPressed
isAcceptPressed (Event _ (KeyboardEvent eventData)) =
    let KeyboardEventData _ motion _ (Keysym (Scancode code) _ _) = eventData
    in  code == enterKeyCode && motion == Pressed
isAcceptPressed _ = False

isWindowClosed :: Event -> Bool
isWindowClosed (Event _ (WindowClosedEvent _)) = True
isWindowClosed _                               = False

chooseSelectedMenuEntry :: GameState -> GameState
chooseSelectedMenuEntry (MainMenu menu joystickIds) = case getSelection menu of
    NewMatch -> Running $ assignJoysticksToMatch joystickIds createMatch
    MainQuit -> Finished
chooseSelectedMenuEntry (Interrupted match menu previousEvents) =
    case getSelection menu of
        -- update so changes in input when paused will
        -- affect the match when unpaused
        Resume    -> Running $ updateMatch previousEvents 0 match
        -- set player ids to not loose joystick mappings
        Restart   -> Running $ setPlayerIds (getPlayerIds match) createMatch
        PauseQuit -> Finished
chooseSelectedMenuEntry state = state

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
    in  if motion == Pressed && code == escapeKeyCode
            then Interrupted match pauseMenu []
            else Running match
runningEventToGameState _ (Event _ (WindowClosedEvent _)) = Finished
runningEventToGameState previousState _ = previousState

assignJoysticks :: [JoystickID] -> Game -> Game
assignJoysticks newJoysticks (Game lastUpdatedTime state) =
    Game lastUpdatedTime $ assignJoysticksToGameState newJoysticks state

assignJoysticksToGameState :: [JoystickID] -> GameState -> GameState
assignJoysticksToGameState newJoysticks (MainMenu menu joystickIds) =
    MainMenu menu $ joystickIds ++ newJoysticks
assignJoysticksToGameState newJoysticks (Running match) =
    Running $ assignJoysticksToMatch newJoysticks match
assignJoysticksToGameState newJoysticks (Interrupted match menu events) =
    Interrupted (assignJoysticksToMatch newJoysticks match) menu events
assignJoysticksToGameState _ Finished = Finished

isFinished :: Game -> Bool
isFinished (Game _ Finished) = True
isFinished _                 = False
