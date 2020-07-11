{-# LANGUAGE ImplicitParams #-}
module GameSpec where

import           Test.Hspec
import           Game
import           Match
import           Circle
import           Space
import           Bullet
import           Player
import           Menu
import           PlayerUtil
import           MenuUtil
import           BulletUtil
import           Visual
import           SDL
import           SDL.Internal.Types             ( Window(..) )
import           Foreign.Ptr                    ( nullPtr )
import           MatchUtil
import           Approx
import           SpaceUtil                      ( )
import           Data.Maybe                     ( fromJust )
import           Data.List                      ( elemIndex )

setSelection :: Eq a => a -> Menu a -> Menu a
setSelection newSelection (Menu header choices _) =
    Menu header choices $ fromJust $ elemIndex newSelection choices

spec :: Spec
spec = do
    let emptyMatch =
            Match (Movables 0 [] []) (Obstacles (createBounds 1920 1080) [])

    describe "createGame"
        $          it "creates a game in the main menu state"
        $          createGame
        `shouldBe` Game 0 (MainMenu mainMenu [])

    describe "drawGame" $ do
        it "draws a match when running"
            $ let in map
                      fst
                      (drawGame (V2 1920 1080) (Game 0 (Running emptyMatch)))
                  `shouldBe` [Rectangle (P (V2 0 0)) (V2 1920 1080)]
        it "draws the match and the menu on top when paused"
            $ let game = Game 0 (Interrupted emptyMatch pauseMenu [])
              in  map fst (drawGame (V2 1920 1080) game)
                  `shouldBe` map (fmap round . fst) (drawMenu pauseMenu)
                  ++         [Rectangle (P (V2 0 0)) (V2 1920 1080)]
        it "only draws the bounds when the game is shut down"
            $          map fst (drawGame (V2 1920 1080) (Game 0 Finished))
            `shouldBe` [Rectangle (P $ V2 0 0) (V2 1920 1080)]
        it
                ("offsets the x position of the menu if the screen has a wider "
                ++ "aspect ratio than the match"
                )
            $ let game = Game 0 (Interrupted emptyMatch pauseMenu [])
              in  map fst (drawGame (V2 3840 1080) game)
                  `shouldBe` map
                                 (fmap round . moveRectangle (V2 960 0) . fst)
                                 (drawMenu pauseMenu)
                  ++         [Rectangle (P (V2 0 0)) (V2 3840 1080)]
        it
                (  "offsets the y position of the menu if the screen has a "
                ++ "narrower aspect ratio than the match"
                )
            $ let game = Game 0 (Interrupted emptyMatch pauseMenu [])
              in  map fst (drawGame (V2 1920 2160) game)
                  `shouldBe` map
                                 (fmap round . moveRectangle (V2 0 540) . fst)
                                 (drawMenu pauseMenu)
                  ++         [Rectangle (P (V2 0 0)) (V2 1920 2160)]
        it "scales the menu if the window is higher resolution than the match"
            $ let game = Game 0 (Interrupted emptyMatch pauseMenu [])
              in  map fst (drawGame (V2 3840 2160) game)
                  `shouldBe` map (fmap round . scaleRectangle 2 . fst)
                                 (drawMenu pauseMenu)
                  ++         [Rectangle (P (V2 0 0)) (V2 3840 2160)]

    describe "updateGame" $ do
        let ?epsilon = 0.01
        it "is not finished when the user has not closed the window"
            $               updateGame [] 50 createGame
            `shouldSatisfy` (not . isFinished)
        it "is finished when the user closes the window"
            $ let closedEvent = Event
                      0
                      (WindowClosedEvent
                          (WindowClosedEventData (Window nullPtr))
                      )
              in  updateGame [closedEvent] 1 (assignJoysticks [0] createGame)
                      `shouldSatisfy` isFinished
        it "updates the match with how much time has passed since last update"
            $ let bullet = createBullet (V2 100 300) 0 0
                  bulletToMatch s = Match
                      (Movables 1 [s] [])
                      (Obstacles (createBounds 1920 1080) [])
                  game = Game 100 $ Running $ bulletToMatch bullet
              in  updateGame [] 150 game
                      `shouldBe` Game
                                     150
                                     (Running $ bulletToMatch $ moveBullet
                                         50
                                         bullet
                                     )
        it "opens the pause menu when the options button is pressed"
            $ let optionsPressed = createButtonPressedEvent 0 9
                  game           = Game 0 $ Running emptyMatch
              in  updateGame [optionsPressed] 1 game
                      `shouldBe` Game 1 (Interrupted emptyMatch pauseMenu [])
        it "opens the pause menu when the ps button is pressed"
            $ let psPressed = createButtonPressedEvent 0 10
                  game      = Game 0 $ Running emptyMatch
              in  updateGame [psPressed] 1 game
                      `shouldBe` Game 1 (Interrupted emptyMatch pauseMenu [])
        it "opens the pause menu when escape is pressed"
            $ let escPressed = toEvent $ KeyboardEventData
                      Nothing
                      Pressed
                      False
                      (Keysym (Scancode 41) (Keycode 27) noKeyModifier)
                  game = Game 0 $ Running emptyMatch
              in  updateGame [escPressed] 1 game
                      `shouldBe` Game 1 (Interrupted emptyMatch pauseMenu [])
        it "opens the win screen for red when red wins"
            $ let
                  red = createPlayer (V2 200 200) 0 Red Nothing
                  blue =
                      setHealth (bulletDamage / 2)
                          $ setDeaths (playerLives - 1)
                          $ createPlayer (V2 500 500) 0 Blue Nothing
                  bullet = createBullet (V2 400 500) 0 0
                  td     = getBulletMovementTime 100
                  match  = Match
                      ( Movables 1 [bullet]
                      $ map (IntersectedPlayer []) [red, blue]
                      )
                      (Obstacles (createBounds 1920 1080) [])
                  game = Game 0 $ Running match
              in
                  updateGame [] td game `shouldBe` Game
                      td
                      (Interrupted
                          (Match
                              (Movables
                                  1
                                  [setBulletHit $ moveBullet td bullet]
                                  [ IntersectedPlayer [] red
                                  , IntersectedPlayer [0]
                                  $ setHealth playerMaxHealth
                                  $ setDeaths playerLives blue
                                  ]
                              )
                              (Obstacles (createBounds 1920 1080) [])
                          )
                          redWinMenu
                          []
                      )
        it "opens the win screen for blue when blue wins"
            $ let red =
                      setHealth (bulletDamage / 2)
                          $ setDeaths (playerLives - 1)
                          $ createPlayer (V2 500 500) 0 Red Nothing
                  blue   = createPlayer (V2 200 200) 0 Blue Nothing
                  bullet = createBullet (V2 400 500) 0 0
                  td     = getBulletMovementTime 100
                  match  = Match
                      ( Movables 1 [bullet]
                      $ map (IntersectedPlayer []) [red, blue]
                      )
                      (Obstacles (createBounds 1920 1080) [])
                  game = Game 0 $ Running match
              in  updateGame [] td game `shouldBe` Game
                      td
                      (Interrupted
                          (Match
                              (Movables
                                  1
                                  [setBulletHit $ moveBullet td bullet]
                                  [ IntersectedPlayer [0]
                                  $ setHealth playerMaxHealth
                                  $ setDeaths playerLives red
                                  , IntersectedPlayer [] blue
                                  ]
                              )
                              (Obstacles (createBounds 1920 1080) [])
                          )
                          blueWinMenu
                          []
                      )
        it "opens the tie screen when it is a draw"
            $ let red   = createPlayer (V2 200 200) 0 Red Nothing
                  b1    = createBullet (V2 100 200) 0 0
                  blue  = createPlayer (V2 500 500) 0 Blue Nothing
                  b2    = createBullet (V2 400 500) 0 1
                  td    = getBulletMovementTime 100
                  match = Match
                      (Movables 2 [b1, b2] $ map
                          ( IntersectedPlayer []
                          . setHealth (bulletDamage / 2)
                          . setDeaths (playerLives - 1)
                          )
                          [red, blue]
                      )
                      (Obstacles (createBounds 1920 1080) [])
                  game = Game 0 $ Running match
              in  updateGame [] td game `shouldBe` Game
                      td
                      (Interrupted
                          (Match
                              (Movables
                                  2
                                  (map (setBulletHit . moveBullet td) [b1, b2])
                                  ( zipWith
                                          (\bulletId ->
                                              IntersectedPlayer [bulletId]
                                          )
                                          [0, 1]
                                  $ map (setDeaths playerLives) [red, blue]
                                  )
                              )
                              (Obstacles (createBounds 1920 1080) [])
                          )
                          tieMenu
                          []
                      )
        it
                (  "resumes the match when the x button is pressed when resume "
                ++ "is selected in the pause menu"
                )
            $ let xPressed = createButtonPressedEvent 0 0
                  game     = Game 0 $ Interrupted emptyMatch pauseMenu []
              in  updateGame [xPressed] 1 game
                      `shouldBe` Game 1 (Running emptyMatch)
        it
                (  "restarts the match when the x button is pressed when "
                ++ "restart is selected in the game over menu"
                )
            $ let match =
                      updateMatch [createTriggerEvent 5 JoyButtonPressed] 100
                          $ assignJoysticksToMatch [5, 7] createMatch
                  xPressed = createButtonPressedEvent 0 0
                  game     = Game 0 $ Interrupted match redWinMenu []
              in  updateGame [xPressed] 1 game
                      `shouldBe` Game
                                     1
                                     (Running $ assignJoysticksToMatch
                                         [5, 7]
                                         createMatch
                                     )
        it
                (  "starts a new match when the x button is pressed when new "
                ++ "game is selected in the main menu"
                )
            $ let xPressed = createButtonPressedEvent 0 0
                  game     = Game 0 $ MainMenu mainMenu []
              in  updateGame [xPressed] 1 game
                      `shouldBe` Game 1 (Running createMatch)
        it
                (  "quits the game when the x button is pressed when quit is "
                ++ "selected in the pause menu"
                )
            $ let xPressed = createButtonPressedEvent 0 0
                  menu     = setSelection PauseQuit pauseMenu
                  game     = Game 0 $ Interrupted emptyMatch menu []
              in  updateGame [xPressed] 1 game `shouldBe` Game 1 Finished
        it
                (  "quits the game when the x button is pressed when quit is "
                ++ "selected in the main menu"
                )
            $ let xPressed = createButtonPressedEvent 0 0
                  menu     = setSelection MainQuit mainMenu
                  game     = Game 0 $ MainMenu menu []
              in  updateGame [xPressed] 1 game `shouldBe` Game 1 Finished
        it
                (  "quits the game when enter is pressed when quit is selected "
                ++ "in the menu"
                )
            $ let enterPressed = toEvent $ KeyboardEventData
                      Nothing
                      Pressed
                      False
                      (Keysym (Scancode 40) (Keycode 13) noKeyModifier)
                  menu = setSelection PauseQuit pauseMenu
                  game = Game 0 $ Interrupted emptyMatch menu []
              in  updateGame [enterPressed] 1 game `shouldBe` Game 1 Finished
        it
                (  "moves the selection down when the left thumbstick is moved "
                ++ "down in interrupted mode"
                )
            $ let
                  stickDown    = toEvent $ JoyAxisEventData 0 1 30000
                  game         = Game 0 $ Interrupted emptyMatch pauseMenu []
                  expectedMenu = setSelection PauseQuit pauseMenu
                  expectedState =
                      Interrupted emptyMatch expectedMenu [stickDown]
              in
                  updateGame [stickDown] 1 game `shouldBe` Game 1 expectedState
        it
                (  "moves the selection down when the left thumbstick is moved "
                ++ "down in main menu mode"
                )
            $ let stickDown     = toEvent $ JoyAxisEventData 0 1 30000
                  game          = Game 0 $ MainMenu mainMenu []
                  expectedMenu  = setSelection MainQuit mainMenu
                  expectedState = MainMenu expectedMenu []
              in  updateGame [stickDown] 1 game `shouldBe` Game 1 expectedState
        it "does not update the match while the game is paused"
            $ let
                  bullet = createBullet (V2 100 300) 0 0
                  match  = Match (Movables 1 [bullet] [])
                                 (Obstacles (createBounds 1920 1080) [])
                  game = Game 0 $ Interrupted match pauseMenu []
              in
                  updateGame [] 50 game
                      `shouldBe` Game 50 (Interrupted match pauseMenu [])
        it
                ("keeps track of thumbstick movements while the game is paused "
                ++ "so the players are given the correct initial velocity when "
                ++ "resuming the match"
                )
            $ let
                  player = createPlayer (V2 100 300) 0 Red (Just 0)
                  match  = Match
                      (Movables 0 [] [IntersectedPlayer [] player])
                      (Obstacles (createBounds 1920 1080) [])
                  game              = Game 0 $ Interrupted match pauseMenu []
                  moveRight         = createMoveRightEvent 0 50 200
                  pausedGame        = updateGame [moveRight] 999 game
                  xPressed          = createButtonPressedEvent 0 0
                  switchedToRunning = updateGame [xPressed] 1000 pausedGame
                  Game _ (Running runningMatch) =
                      updateGame [] 1200 switchedToRunning
                  newPosition = getPlayerPosition $ getFirstPlayer runningMatch
              in
                  newPosition `shouldApproxBe` V2 150 300
        it
                (  "keeps track of trigger changes while the game is paused "
                ++ "so the players are given the correct initial firing state "
                ++ "when resuming the match"
                )
            $ let
                  player = createPlayer (V2 100 300) 0 Red (Just 0)
                  bulletsToMatch bullets = Match
                      (Movables 0 bullets [IntersectedPlayer [] player])
                      (Obstacles (createBounds 1920 1080) [])
                  match             = bulletsToMatch []
                  game              = Game 0 $ Interrupted match pauseMenu []
                  triggerPressed    = createTriggerEvent 0 JoyButtonPressed
                  pausedGame        = updateGame [triggerPressed] 999 game
                  xPressed          = createButtonPressedEvent 0 0
                  switchedToRunning = updateGame [xPressed] 1000 pausedGame
                  Game _ (Running runningMatch) =
                      updateGame [] 1200 switchedToRunning
              in
                  getBullets runningMatch
                      `shouldBe` [ moveBullet 200
                                       $ createBullet (V2 100 300) 0 0
                                 ]
        it
                (  "only keeps track of the latest change for a button while "
                ++ "the game is paused"
                )
            $ let
                  player = createPlayer (V2 100 300) 0 Red (Just 0)
                  match  = Match
                      (Movables 0 [] [IntersectedPlayer [] player])
                      (Obstacles (createBounds 1920 1080) [])
                  game              = Game 0 $ Interrupted match pauseMenu []
                  triggerPressed    = createTriggerEvent 0 JoyButtonPressed
                  firstPressed      = updateGame [triggerPressed] 500 game
                  triggerReleased   = createTriggerEvent 0 JoyButtonReleased
                  thenReleased = updateGame [triggerReleased] 999 firstPressed
                  xPressed          = createButtonPressedEvent 0 0
                  switchedToRunning = updateGame [xPressed] 1000 thenReleased
                  Game _ (Running runningMatch) =
                      updateGame [] 1200 switchedToRunning
              in
                  runningMatch `shouldBe` match
        it
                ("respects the sequential order of the supplied events when in "
                ++ "the menu"
                )
            $ let game     = Game 0 $ Interrupted emptyMatch pauseMenu []
                  moveDown = toEvent $ JoyHatEventData 0 0 HatDown
                  moveUp   = toEvent $ JoyHatEventData 0 0 HatUp
              in  updateGame [moveDown, moveUp] 1 game
                      `shouldBe` Game
                                     1
                                     (Interrupted
                                         emptyMatch
                                         pauseMenu
                                         [moveDown, moveUp]
                                     )
        it "removes joysticks that are disconnected when in main menu"
            $ let
                  game = Game 0 $ MainMenu mainMenu [8]
                  joystickRemoved =
                      toEvent $ JoyDeviceEventData JoyDeviceRemoved 8
                  Game _ (MainMenu _ joystickIds) =
                      updateGame [joystickRemoved] 1 game
              in
                  joystickIds `shouldBe` []
        it
                ("creates the game with the joysticks that were available in the "
                ++ "main menu"
                )
            $ let game                   = Game 0 $ MainMenu mainMenu [8]
                  xPressed               = createButtonPressedEvent 8 0
                  Game _ (Running match) = updateGame [xPressed] 1 game
              in  match `shouldBe` assignJoysticksToMatch [8] createMatch

    describe "assignJoysticks" $ do
        let player = IntersectedPlayer [] (createPlayer (V2 0 0) 0 Red Nothing)
            match =
                Match (Movables 0 [] [player]) (Obstacles (createBounds 0 0) [])
        it "assigns joysticks to the match when running"
            $ let game                      = Game 0 $ Running match
                  Game _ (Running newMatch) = assignJoysticks [1] game
              in  getJoystickId (getFirstPlayer newMatch) `shouldBe` Just 1
        it "assigns joysticks to the match when paused"
            $ let game = Game 0 $ Interrupted match pauseMenu []
                  Game _ (Interrupted newMatch _ _) = assignJoysticks [1] game
              in  getJoystickId (getFirstPlayer newMatch) `shouldBe` Just 1
        it "keeps track of available joysticks when in the main menu"
            $ let game = Game 0 $ MainMenu mainMenu [3]
                  Game _ (MainMenu _ joystickIds) = assignJoysticks [1] game
              in  joystickIds `shouldBe` [3, 1]
        it "does nothing when the game is already finished"
            $          assignJoysticks [1] (Game 0 Finished)
            `shouldBe` Game 0 Finished
