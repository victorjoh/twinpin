module GameSpec where

import           Test.Hspec
import           Game
import           Match
import           Circle
import           Space
import           Shot
import           Player
import           PlayerUtil
import           SDL                     hiding ( Paused )
import           SDL.Internal.Types             ( Window(..) )
import           Foreign.Ptr                    ( nullPtr )
import           MatchUtil

spec :: Spec
spec = do
    describe "createGame"
        $ it "creates a game in a running state"
        $ let size = V2 800 600
              game = createGame size
          in  game `shouldBe` Game
                  0
                  (Running $ createMatch $ fromIntegral <$> size)

    describe "drawGame" $ do
        let pillar       = Circle (V2 60 70) 48
            windowWidth  = 800
            windowHeight = 600
            windowBounds = createBounds windowWidth windowHeight
            match = Match (Movables [] []) (Obstacles windowBounds [pillar])
        it "draws a match when running"
            $          map fst (drawGame (Game 0 (Running match)))
            `shouldBe` [toTextureArea pillar]
        it "draws the match and the menu on top when paused"
            $ let menuSize   = V2 300 230
                  windowSize = V2 windowWidth windowHeight
                  menuPos    = windowSize / 2 - (fromIntegral <$> menuSize) / 2
              in  map fst (drawGame (Game 0 (Paused match Resume [])))
                      `shouldBe` [ toTextureArea pillar
                                 , Rectangle (P (round <$> menuPos)) menuSize
                                 ]
        it "draws nothing when the game is shut down"
            $          map fst (drawGame (Game 0 Finished))
            `shouldBe` []

    describe "updateGame" $ do
        let emptyMatch =
                Match (Movables [] []) (Obstacles (createBounds 800 600) [])
        it "is not finished when the user has not closed the window"
            $               updateGame [] 50 (createGame $ V2 800 600)
            `shouldSatisfy` (not . isFinished)
        it "is finished when the user closes the window"
            $ let closedEvent = Event
                      0
                      (WindowClosedEvent
                          (WindowClosedEventData (Window nullPtr))
                      )
              in  updateGame [closedEvent] 1 (createGame $ V2 5 5)
                      `shouldSatisfy` isFinished
        it "updates the match with how much time has passed since last update"
            $ let
                  shot = createShot (V2 100 300) 0
                  shotToMatch s = Match
                      (Movables [s] [])
                      (Obstacles (createBounds 800 600) [])
                  game = Game 100 $ Running $ shotToMatch shot
              in
                  updateGame [] 150 game
                      `shouldBe` Game
                                     150
                                     (Running $ shotToMatch $ updateShot 50 shot
                                     )
        it "opens the pause menu when the options button is pressed"
            $ let optionsPressed = createButtonPressedEvent 0 9
                  game           = Game 0 $ Running emptyMatch
              in  updateGame [optionsPressed] 1 game
                      `shouldBe` Game 1 (Paused emptyMatch Resume [])
        it "opens the pause menu when the ps button is pressed"
            $ let psPressed = createButtonPressedEvent 0 10
                  game      = Game 0 $ Running emptyMatch
              in  updateGame [psPressed] 1 game
                      `shouldBe` Game 1 (Paused emptyMatch Resume [])
        it
                (  "resumes the match when the x button is pressed when resume "
                ++ "is selected in the menu"
                )
            $ let xPressed = createButtonPressedEvent 0 0
                  game     = Game 0 $ Paused emptyMatch Resume []
              in  updateGame [xPressed] 1 game
                      `shouldBe` Game 1 (Running emptyMatch)
        it
                (  "quits the game when the x button is pressed when quit is "
                ++ "selected in the menu"
                )
            $ let xPressed = createButtonPressedEvent 0 0
                  game     = Game 0 $ Paused emptyMatch Quit []
              in  updateGame [xPressed] 1 game `shouldBe` Game 1 Finished
        it "moves the selection down when the left thumbstick is moved down"
            $ let stickDown = toEvent $ JoyAxisEventData 0 1 30000
                  game      = Game 0 $ Paused emptyMatch Resume []
              in  updateGame [stickDown] 1 game
                      `shouldBe` Game 1 (Paused emptyMatch Quit [stickDown])
        it "moves the selection up when the left thumbstick is moved up"
            $ let stickUp = toEvent $ JoyAxisEventData 0 1 $ -30000
                  game    = Game 0 $ Paused emptyMatch Quit []
              in  updateGame [stickUp] 1 game
                      `shouldBe` Game 1 (Paused emptyMatch Resume [stickUp])
        it
                (  "moves the selection down when down is pressed on the "
                ++ "directional buttons"
                )
            $ let dirDown = toEvent $ JoyHatEventData 0 0 HatDown
                  game    = Game 0 $ Paused emptyMatch Resume []
              in  updateGame [dirDown] 1 game
                      `shouldBe` Game 1 (Paused emptyMatch Quit [dirDown])
        it
                ("moves the selection up when up is pressed on the directional "
                ++ "buttons"
                )
            $ let dirUp = toEvent $ JoyHatEventData 0 0 HatUp
                  game  = Game 0 $ Paused emptyMatch Quit []
              in  updateGame [dirUp] 1 game
                      `shouldBe` Game 1 (Paused emptyMatch Resume [dirUp])
        it "does not update the match while the game is paused"
            $ let
                  shot  = createShot (V2 100 300) 0
                  match = Match (Movables [shot] [])
                                (Obstacles (createBounds 800 600) [])
                  game = Game 0 $ Paused match Resume []
              in
                  updateGame [] 50 game
                      `shouldBe` Game 50 (Paused match Resume [])
        it
                ("keeps track of thumbstick movements while the game is paused "
                ++ "so the players are given the correct initial velocity when "
                ++ "resuming the match"
                )
            $ let
                  player = createPlayer (V2 100 300) 0 0
                  match  = Match (Movables [] [PlayerWithBarrel player []])
                                 (Obstacles (createBounds 800 600) [])
                  game              = Game 0 $ Paused match Resume []
                  moveRight         = createMoveRightEvent 0 50 200
                  pausedGame        = updateGame [moveRight] 999 game
                  xPressed          = createButtonPressedEvent 0 0
                  switchedToRunning = updateGame [xPressed] 1000 pausedGame
                  Game _ (Running runningMatch) =
                      updateGame [] 1200 switchedToRunning
                  newPosition = getPlayerPosition $ getFirstPlayer runningMatch
              in
                  newPosition `shouldBe` V2 150 300
        it
                (  "keeps track of trigger changes while the game is paused "
                ++ "so the players are given the correct initial firing state "
                ++ "when resuming the match"
                )
            $ let
                  player = createPlayer (V2 100 300) 0 0
                  shotsToMatch shots = Match
                      (Movables shots [PlayerWithBarrel player []])
                      (Obstacles (createBounds 800 600) [])
                  match             = shotsToMatch []
                  game              = Game 0 $ Paused match Resume []
                  triggerPressed    = createTriggerEvent 0 JoyButtonPressed
                  pausedGame        = updateGame [triggerPressed] 999 game
                  xPressed          = createButtonPressedEvent 0 0
                  switchedToRunning = updateGame [xPressed] 1000 pausedGame
                  Game _ (Running runningMatch) =
                      updateGame [] 1200 switchedToRunning
              in
                  getShots runningMatch
                      `shouldBe` [updateShot 200 $ createShot (V2 100 300) 0]
        it
                (  "only keeps track of the latest change for a button while "
                ++ "the game is paused"
                )
            $ let
                  player = createPlayer (V2 100 300) 0 0
                  match  = Match (Movables [] [PlayerWithBarrel player []])
                                 (Obstacles (createBounds 800 600) [])
                  game              = Game 0 $ Paused match Resume []
                  triggerPressed    = createTriggerEvent 0 JoyButtonPressed
                  firstPressed      = updateGame [triggerPressed] 500 game
                  triggerReleased   = createTriggerEvent 0 JoyButtonReleased
                  thenReleased      = updateGame [triggerReleased] 999 game
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
            $ let game     = Game 0 $ Paused emptyMatch Resume []
                  moveDown = toEvent $ JoyHatEventData 0 0 HatDown
                  moveUp   = toEvent $ JoyHatEventData 0 0 HatUp
              in  updateGame [moveDown, moveUp] 1 game
                      `shouldBe` Game
                                     1
                                     (Paused
                                         emptyMatch
                                         Resume
                                         [moveDown, moveUp]
                                     )
