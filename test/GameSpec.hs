{-# LANGUAGE ImplicitParams #-}
module GameSpec where

import           Test.Hspec
import           Game
import           Match
import           Circle
import           Space
import           Shot
import           Player
import           Menu
import           PlayerUtil
import           MenuUtil
import           Visual
import           SDL                     hiding ( Paused )
import           SDL.Internal.Types             ( Window(..) )
import           Foreign.Ptr                    ( nullPtr )
import           MatchUtil
import           Test.HUnit.Approx

spec :: Spec
spec = do
    let emptyMatch =
            Match (Movables [] []) (Obstacles (createBounds 1920 1080) [])

    describe "createGame"
        $          it "creates a game in a running state"
        $          createGame
        `shouldBe` Game 0 (Running createMatch)

    describe "drawGame" $ do
        it "draws a match when running"
            $ let in map
                      fst
                      (drawGame (V2 1920 1080) (Game 0 (Running emptyMatch)))
                  `shouldBe` [Rectangle (P (V2 0 0)) (V2 1920 1080)]
        it "draws the match and the menu on top when paused"
            $ let game = Game 0 (Paused emptyMatch Resume [])
              in  map fst (drawGame (V2 1920 1080) game)
                  `shouldBe` map (fmap round . fst) (drawMenu Resume)
                  ++         [Rectangle (P (V2 0 0)) (V2 1920 1080)]
        it "only draws the bounds when the game is shut down"
            $          map fst (drawGame (V2 1920 1080) (Game 0 Finished))
            `shouldBe` [Rectangle (P $ V2 0 0) (V2 1920 1080)]
        it
                ("offsets the x position of the menu if the screen has a wider "
                ++ "aspect ratio than the match"
                )
            $ let game = Game 0 (Paused emptyMatch Resume [])
              in  map fst (drawGame (V2 3840 1080) game)
                  `shouldBe` map
                                 (fmap round . moveRectangle (V2 960 0) . fst)
                                 (drawMenu Resume)
                  ++         [Rectangle (P (V2 0 0)) (V2 3840 1080)]
        it
                (  "offsets the y position of the menu if the screen has a "
                ++ "narrower aspect ratio than the match"
                )
            $ let game = Game 0 (Paused emptyMatch Resume [])
              in  map fst (drawGame (V2 1920 2160) game)
                  `shouldBe` map
                                 (fmap round . moveRectangle (V2 0 540) . fst)
                                 (drawMenu Resume)
                  ++         [Rectangle (P (V2 0 0)) (V2 1920 2160)]
        it "scales the menu if the window is higher resolution than the match"
            $ let game = Game 0 (Paused emptyMatch Resume [])
              in  map fst (drawGame (V2 3840 2160) game)
                  `shouldBe` map (fmap round . scaleRectangle 2 . fst)
                                 (drawMenu Resume)
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
              in  updateGame [closedEvent] 1 createGame
                      `shouldSatisfy` isFinished
        it "updates the match with how much time has passed since last update"
            $ let
                  shot = createShot (V2 100 300) 0
                  shotToMatch s = Match
                      (Movables [s] [])
                      (Obstacles (createBounds 1920 1080) [])
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
        it "opens the pause menu when escape is pressed"
            $ let escPressed = toEvent $ KeyboardEventData
                      Nothing
                      Pressed
                      False
                      (Keysym (Scancode 41) (Keycode 27) noKeyModifier)
                  game = Game 0 $ Running emptyMatch
              in  updateGame [escPressed] 1 game
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
        it
                (  "quits the game when enter is pressed when quit is selected "
                ++ "in the menu"
                )
            $ let enterPressed = toEvent $ KeyboardEventData
                      Nothing
                      Pressed
                      False
                      (Keysym (Scancode 40) (Keycode 13) noKeyModifier)
                  game = Game 0 $ Paused emptyMatch Quit []
              in  updateGame [enterPressed] 1 game `shouldBe` Game 1 Finished
        it "moves the selection down when the left thumbstick is moved down"
            $ let stickDown = toEvent $ JoyAxisEventData 0 1 30000
                  game      = Game 0 $ Paused emptyMatch Resume []
              in  updateGame [stickDown] 1 game
                      `shouldBe` Game 1 (Paused emptyMatch Quit [stickDown])
        it "does not update the match while the game is paused"
            $ let
                  shot  = createShot (V2 100 300) 0
                  match = Match (Movables [shot] [])
                                (Obstacles (createBounds 1920 1080) [])
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
                                 (Obstacles (createBounds 1920 1080) [])
                  game              = Game 0 $ Paused match Resume []
                  moveRight         = createMoveRightEvent 0 50 200
                  pausedGame        = updateGame [moveRight] 999 game
                  xPressed          = createButtonPressedEvent 0 0
                  switchedToRunning = updateGame [xPressed] 1000 pausedGame
                  Game _ (Running runningMatch) =
                      updateGame [] 1200 switchedToRunning
                  newPosition = getPlayerPosition $ getFirstPlayer runningMatch
              in
                  newPosition @?~ V2 150 300
        it
                (  "keeps track of trigger changes while the game is paused "
                ++ "so the players are given the correct initial firing state "
                ++ "when resuming the match"
                )
            $ let
                  player = createPlayer (V2 100 300) 0 0
                  shotsToMatch shots = Match
                      (Movables shots [PlayerWithBarrel player []])
                      (Obstacles (createBounds 1920 1080) [])
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
                                 (Obstacles (createBounds 1920 1080) [])
                  game              = Game 0 $ Paused match Resume []
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
