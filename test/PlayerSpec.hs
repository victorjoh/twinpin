module PlayerSpec where

import           Test.Hspec
import           Player
import           PlayerUtil
import           Space
import           Circle
import           Shot                           ( createShot )
import           SDL.Vect
import           SDL.Event
import           SDL.Input.Joystick             ( JoyButtonState(..) )
import           SDL.Video.Renderer             ( Rectangle(..) )
import           Codec.Picture.Types
import           Data.Maybe

spec :: Spec
spec = do
    describe "toDrawablePlayer"
        $ it
              (  "transforms the position and size of the player to something"
              ++ " that SDL is familiar with"
              )
        $ let player           = createPlayer (V2 40 50) pi 0
              (destination, _) = toDrawablePlayer player
          in  destination `shouldBe` toTextureArea (playerToCircle player)

    describe "updatePlayer" $ do
        it "moves the player according to the changed left stick position"
            $ let old        = createPlayer (V2 40 50) 0 0
                  stickXPos  = JoyAxisEventData 0 0 10000
                  stickYPos  = JoyAxisEventData 0 1 20000
                  passedTime = 50
                  new = updatePlayer (toEvents [stickXPos, stickYPos])
                                     passedTime
                                     (Obstacles (createBounds 800 600) [])
                                     old
              in  getPlayerPosition new `shouldBe` V2 45 60
        it
                (  "does not move the player if the left stick position is very"
                ++ " close to the default position"
                )
            $ let oldPos = V2 40 50
                  old =
                      setPlayerVelocity (V2 0.1 0.2) $ createPlayer oldPos 0 0
                  stickXPos = JoyAxisEventData 0 0 4000
                  stickYPos = JoyAxisEventData 0 1 4000
                  new       = updatePlayer (toEvents [stickXPos, stickYPos])
                                           50
                                           (Obstacles (createBounds 800 600) [])
                                           old
              in  getPlayerPosition new `shouldBe` oldPos
        it "remembers the velocity changed from previous updates"
            $ let
                  old = setPlayerVelocity (V2 0.1 0.2)
                      $ createPlayer (V2 40 50) 0 0
                  new = updatePlayer []
                                     50
                                     (Obstacles (createBounds 800 600) [])
                                     old
              in
                  getPlayerPosition new `shouldBe` V2 45 60
        it "limits the player position by the bounds given"
            $ let bounds = createBounds 100 600
                  old =
                      setPlayerVelocity (V2 0.1 0) $ createPlayer (V2 40 50) 0 0
                  new = updatePlayer [] 1000 (Obstacles bounds []) old
              in  getPlayerPosition new `shouldBe` V2 (100 - playerRadius) 50
        it "changes the player's aim depending on the right stick position"
            $ let stickXPos = JoyAxisEventData 0 3 10000
                  stickYPos = JoyAxisEventData 0 4 (-10000)
                  old       = createPlayer (V2 50 50) 0 0
                  new       = updatePlayer (toEvents [stickXPos, stickYPos])
                                           1
                                           (Obstacles (createBounds 800 600) [])
                                           old
              in  getPlayerAngle new `shouldBe` -pi / 4
        it
                (  "does not change the player's aim if the right stick is very"
                ++ " close to the default position"
                )
            $ let oldAngle  = 30
                  stickXPos = JoyAxisEventData 0 3 4000
                  stickYPos = JoyAxisEventData 0 4 (-4000)
                  old       = createPlayer (V2 50 50) oldAngle 0
                  new       = updatePlayer (toEvents [stickXPos, stickYPos])
                                           1
                                           (Obstacles (createBounds 800 600) [])
                                           old
              in  getPlayerAngle new `shouldBe` oldAngle
        it "ignores unused joystick buttons (left trigger)"
            $ let unused = JoyAxisEventData 0 2 10000
                  old    = createPlayer (V2 40 50) 0 0
                  new    = updatePlayer (toEvents [unused])
                                        50
                                        (Obstacles (createBounds 800 600) [])
                                        old
              in  new `shouldBe` old
        it "ignores events from different gamepads"
            $ let old       = createPlayer (V2 40 50) 0 0
                  stickYPos = JoyAxisEventData 1 1 20000
                  new       = updatePlayer (toEvents [stickYPos])
                                           50
                                           (Obstacles (createBounds 800 600) [])
                                           old
              in  new `shouldBe` old
        it "reduces the reload time according to the passed time"
            $ let
                  oldReloadTime = 150
                  old =
                      setReloadTime oldReloadTime $ createPlayer (V2 40 50) 0 0
                  new = updatePlayer []
                                     50
                                     (Obstacles (createBounds 800 600) [])
                                     old
              in
                  getReloadTime new `shouldBe` 100
        it "reduces the reload time no less than 0"
            $ let
                  oldReloadTime = 30
                  old =
                      setReloadTime oldReloadTime $ createPlayer (V2 40 50) 0 0
                  new = updatePlayer []
                                     50
                                     (Obstacles (createBounds 800 600) [])
                                     old
              in
                  getReloadTime new `shouldBe` 0

    describe "triggerShot" $ do
        it "triggers a shot if the right bumper button is pressed"
            $ let playerPos      = V2 40 50
                  playerAngle    = 30
                  player         = createPlayer playerPos playerAngle 0
                  (Gun aim _ _)  = getGun player
                  triggerPressed = JoyButtonEventData 0 5 JoyButtonPressed
              in  triggerShot [toEvent triggerPressed] player
                      `shouldBe` ( setGun (Gun aim minShotInterval Firing)
                                          player
                                 , Just (createShot playerPos playerAngle)
                                 )
        it
                (  "does not trigger a shot if some other button is pressed"
                ++ " (x button)"
                )
            $ let unusedPressed = JoyButtonEventData 0 0 JoyButtonPressed
              in  isNothing $ snd $ triggerShot
                      (toEvents [unusedPressed])
                      (createPlayer (V2 40 50) 30 0)
        it "ignores events from different gamepads"
            $ let playerId = 1
              in  isNothing $ snd $ triggerShot
                      [createTriggerEvent 0]
                      (createPlayer (V2 40 50) 30 playerId)
        it "does not trigger a shot if the player is reloading"
            $ let player = setReloadTime 10 $ createPlayer (V2 0 0) 0 0
              in  isNothing $ snd $ triggerShot [createTriggerEvent 0] player
        it "keeps shooting while the trigger is pressed"
            $ let old          = createPlayer (V2 0 0) 0 0
                  (between, _) = triggerShot [createTriggerEvent 0] old
              in  isJust $ snd $ triggerShot [] $ setReloadTime 0 between
        it "stops shooting when the trigger is released"
            $ let
                  old             = createPlayer (V2 0 0) 0 0
                  (between, _)    = triggerShot [createTriggerEvent 0] old
                  triggerReleased = JoyButtonEventData 0 5 JoyButtonReleased
                  (_, maybeShot) =
                      triggerShot (toEvents [triggerReleased]) between
              in
                  isNothing maybeShot
        it "triggers a shot if the right trigger button is pressed"
            $ let triggerPressed = JoyAxisEventData 0 5 (triggerMinFireValue + 1)
                  player         = createPlayer (V2 0 0) 0 0
              in  isJust $ snd $ triggerShot [toEvent triggerPressed] player
        it
                (  "does not trigger a shot if the right trigger button is not"
                ++ " pressed far enough"
                )
            $ let triggerPressed = JoyAxisEventData 0 5 (triggerMinFireValue - 1)
                  player         = createPlayer (V2 0 0) 0 0
              in  isNothing $ snd $ triggerShot [toEvent triggerPressed] player
