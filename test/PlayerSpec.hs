module PlayerSpec where

import           Test.Hspec
import           Player
import           PlayerUtil
import           Space
import           Circle
import           Shot                           ( createShot )
import           SDL
import           SDL.Input.Joystick             ( JoyButtonState(..) )
import           Data.Maybe
import           Data.List                      ( replicate )

spec :: Spec
spec = do
    describe "drawPlayer"
        $ it
              (  "transforms the position and size of the player to something"
              ++ " that SDL is familiar with"
              )
        $ let player    = createPlayer (V2 40 50) pi Red Nothing
              positions = map fst $ drawPlayer player
          in  positions
                  `shouldBe` replicate 2 (toTextureArea $ playerToCircle player)

    describe "updatePlayer" $ do
        it "moves the player according to the changed left stick position"
            $ let old        = createPlayer (V2 40 50) 0 Red (Just 3)
                  stickXPos  = JoyAxisEventData 3 0 10000
                  stickYPos  = JoyAxisEventData 3 1 20000
                  passedTime = 50
                  new = updatePlayer (toEvents [stickXPos, stickYPos])
                                     passedTime
                                     (Obstacles (createBounds 800 600) [])
                                     old
              in  getPlayerPosition new `shouldBe` V2 49 68
        it
                (  "does not move the player if the left stick position is very"
                ++ " close to the default position"
                )
            $ let
                  oldPos = V2 40 50
                  old    = setPlayerVelocity (V2 0.1 0.2)
                      $ createPlayer oldPos 0 Red (Just 0)
                  stickXPos = JoyAxisEventData 0 0 4000
                  stickYPos = JoyAxisEventData 0 1 4000
                  new       = updatePlayer (toEvents [stickXPos, stickYPos])
                                           50
                                           (Obstacles (createBounds 800 600) [])
                                           old
              in
                  getPlayerPosition new `shouldBe` oldPos
        it "remembers the velocity changed from previous updates"
            $ let
                  old = setPlayerVelocity (V2 0.1 0.2)
                      $ createPlayer (V2 40 50) 0 Red Nothing
                  new = updatePlayer []
                                     50
                                     (Obstacles (createBounds 800 600) [])
                                     old
              in
                  getPlayerPosition new `shouldBe` V2 45 60
        it "limits the player position by the bounds given"
            $ let
                  bounds = createBounds 100 600
                  old    = setPlayerVelocity (V2 0.1 0)
                      $ createPlayer (V2 40 50) 0 Red Nothing
                  new = updatePlayer [] 1000 (Obstacles bounds []) old
              in
                  getPlayerPosition new `shouldBe` V2 (100 - playerRadius) 50
        it "changes the player's aim depending on the right stick position"
            $ let stickXPos = JoyAxisEventData 0 3 10000
                  stickYPos = JoyAxisEventData 0 4 (-10000)
                  old       = createPlayer (V2 50 50) 0 Red (Just 0)
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
                  old       = createPlayer (V2 50 50) oldAngle Red (Just 0)
                  new       = updatePlayer (toEvents [stickXPos, stickYPos])
                                           1
                                           (Obstacles (createBounds 800 600) [])
                                           old
              in  getPlayerAngle new `shouldBe` oldAngle
        it "ignores unused joystick buttons (left trigger)"
            $ let unused = JoyAxisEventData 0 2 10000
                  old    = createPlayer (V2 40 50) 0 Red (Just 0)
                  new    = updatePlayer (toEvents [unused])
                                        50
                                        (Obstacles (createBounds 800 600) [])
                                        old
              in  new `shouldBe` old
        it "ignores events from different gamepads"
            $ let old       = createPlayer (V2 40 50) 0 Red (Just 0)
                  stickYPos = JoyAxisEventData 1 1 20000
                  new       = updatePlayer (toEvents [stickYPos])
                                           50
                                           (Obstacles (createBounds 800 600) [])
                                           old
              in  new `shouldBe` old
        it "reduces the reload time according to the passed time"
            $ let
                  oldReloadTime = 150
                  old           = setReloadTime oldReloadTime
                      $ createPlayer (V2 40 50) 0 Red Nothing
                  new = updatePlayer []
                                     50
                                     (Obstacles (createBounds 800 600) [])
                                     old
              in
                  getReloadTime new `shouldBe` 100
        it "reduces the reload time no less than 0"
            $ let
                  oldReloadTime = 30
                  old           = setReloadTime oldReloadTime
                      $ createPlayer (V2 40 50) 0 Red Nothing
                  new = updatePlayer []
                                     50
                                     (Obstacles (createBounds 800 600) [])
                                     old
              in
                  getReloadTime new `shouldBe` 0
        it "removes the joystick if it is disconnected"
            $ let
                  joystickId = 5
                  old        = createPlayer (V2 100 100) 0 Red (Just joystickId)
                  joystickRemoved =
                      toEvent $ JoyDeviceEventData JoyDeviceRemoved joystickId
                  new = updatePlayer [joystickRemoved]
                                     1
                                     (Obstacles (createBounds 800 600) [])
                                     old
              in
                  new `shouldNotSatisfy` hasJoystick
        it "does not remove the joystick if some other joystick is disconnected"
            $ let
                  playerId = 5
                  eventId  = 9
                  old      = createPlayer (V2 100 100) 0 Red (Just playerId)
                  otherJoystickRemoved =
                      toEvent $ JoyDeviceEventData JoyDeviceRemoved eventId
                  new = updatePlayer [otherJoystickRemoved]
                                     1
                                     (Obstacles (createBounds 800 600) [])
                                     old
              in
                  new `shouldSatisfy` hasJoystick

    describe "triggerShot" $ do
        it "triggers a shot if the right bumper button is pressed"
            $ let playerPos      = V2 40 50
                  playerAngle    = 30
                  player = createPlayer playerPos playerAngle Red (Just 0)
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
                  maybeShot     = snd $ triggerShot
                      (toEvents [unusedPressed])
                      (createPlayer (V2 40 50) 30 Red (Just 0))
              in  maybeShot `shouldSatisfy` isNothing
        it "ignores events from different gamepads"
            $ let joystickId = 1
                  maybeShot  = snd $ triggerShot
                      [createTriggerEvent 0 JoyButtonPressed]
                      (createPlayer (V2 40 50) 30 Red (Just joystickId))
              in  maybeShot `shouldSatisfy` isNothing
        it "does not trigger a shot if the player is reloading"
            $ let
                  player =
                      setReloadTime 10 $ createPlayer (V2 0 0) 0 Red (Just 0)
                  maybeShot = snd $ triggerShot
                      [createTriggerEvent 0 JoyButtonPressed]
                      player
              in
                  maybeShot `shouldSatisfy` isNothing
        it "keeps shooting while the trigger is pressed"
            $ let
                  old     = createPlayer (V2 0 0) 0 Red (Just 0)
                  between = fst $ triggerShot
                      [createTriggerEvent 0 JoyButtonPressed]
                      old
                  maybeShot = snd $ triggerShot [] $ setReloadTime 0 between
              in
                  maybeShot `shouldSatisfy` isJust
        it "stops shooting when the trigger is released"
            $ let
                  old = createPlayer (V2 0 0) 0 Red (Just 0)
                  (between, _) =
                      triggerShot [createTriggerEvent 0 JoyButtonPressed] old
                  reloadedBetween = setReloadTime 0 between
                  triggerReleased = createTriggerEvent 0 JoyButtonReleased
                  maybeShot =
                      snd $ triggerShot [triggerReleased] reloadedBetween
              in
                  maybeShot `shouldSatisfy` isNothing
        it "triggers a shot if the right trigger button is pressed"
            $ let triggerPressed =
                      JoyAxisEventData 0 5 (triggerMinFireValue + 1)
                  player    = createPlayer (V2 0 0) 0 Red (Just 0)
                  maybeShot = snd $ triggerShot [toEvent triggerPressed] player
              in  maybeShot `shouldSatisfy` isJust
        it
                (  "does not trigger a shot if the right trigger button is not"
                ++ " pressed far enough"
                )
            $ let triggerPressed =
                      JoyAxisEventData 0 5 (triggerMinFireValue - 1)
                  player    = createPlayer (V2 0 0) 0 Red (Just 0)
                  maybeShot = snd $ triggerShot [toEvent triggerPressed] player
              in  maybeShot `shouldSatisfy` isNothing
        it "respects the sequential order of the supplied events"
            $ let old             = createPlayer (V2 0 0) 0 Red (Just 0)
                  triggerPressed  = createTriggerEvent 0 JoyButtonPressed
                  triggerReleased = createTriggerEvent 0 JoyButtonReleased
                  new = fst $ triggerShot [triggerPressed, triggerReleased] old
              in  getGunState new `shouldBe` Idle
