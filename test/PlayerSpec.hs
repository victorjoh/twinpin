{-# LANGUAGE ImplicitParams #-}
module PlayerSpec where

import           Test.Hspec
import           Player
import           PlayerUtil
import           Space
import           Circle
import           Bullet                         ( createBullet )
import           SDL
import           SDL.Input.Joystick             ( JoyButtonState(..) )
import           Data.Maybe
import           Data.List                      ( replicate )
import qualified Graphics.Rasterific           as R
                                                ( V2(..) )
import           Test.HUnit.Base                ( Assertion )
import           Control.Monad                  ( when )
import           Test.HUnit.Lang                ( HUnitFailure(..)
                                                , FailureReason(..)
                                                )
import           Control.Exception              ( throwIO )
import           Data.CallStack

location :: HasCallStack => Maybe SrcLoc
location = case reverse callStack of
    (_, loc) : _ -> Just loc
    []           -> Nothing

shouldApproxBe
    :: ( HasCallStack
       , Ord a
       , Num (f a)
       , Show (f a)
       , Foldable f
       , Functor f
       , Eq (f a)
       , Show a
       , ?epsilon::a
       )
    => [f a]
    -> [f a]
    -> Assertion
shouldApproxBe expected actual
    | length expected /= length actual
    = expected `shouldBe` actual
    | otherwise
    = when (any (or . fmap (> ?epsilon) . abs) $ zipWith (-) actual expected)
        $ throwIO
              (HUnitFailure location $ ExpectedButGot
                  (Just $ "maximum margin of error: " ++ show ?epsilon)
                  (show expected)
                  (show actual)
              )

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

    describe "fireBullet" $ do
        it "fires a bullet if the right bumper button is pressed"
            $ let playerPos      = V2 40 50
                  playerAngle    = 30
                  player = createPlayer playerPos playerAngle Red (Just 0)
                  (Gun aim _ _)  = getGun player
                  triggerPressed = JoyButtonEventData 0 5 JoyButtonPressed
              in  fireBullet [toEvent triggerPressed] 8 player
                      `shouldBe` ( setGun (Gun aim minFiringInterval Firing)
                                          player
                                 , Just (createBullet playerPos playerAngle 8)
                                 )
        it
                (  "does not fire a bullet if some other button is pressed"
                ++ " (x button)"
                )
            $ let unusedPressed = JoyButtonEventData 0 0 JoyButtonPressed
                  maybeBullet   = snd $ fireBullet
                      (toEvents [unusedPressed])
                      8
                      (createPlayer (V2 40 50) 30 Red (Just 0))
              in  maybeBullet `shouldSatisfy` isNothing
        it "ignores events from different gamepads"
            $ let joystickId  = 1
                  maybeBullet = snd $ fireBullet
                      [createTriggerEvent 0 JoyButtonPressed]
                      8
                      (createPlayer (V2 40 50) 30 Red (Just joystickId))
              in  maybeBullet `shouldSatisfy` isNothing
        it "does not fire a bullet if the player is reloading"
            $ let
                  player =
                      setReloadTime 10 $ createPlayer (V2 0 0) 0 Red (Just 0)
                  maybeBullet = snd $ fireBullet
                      [createTriggerEvent 0 JoyButtonPressed]
                      8
                      player
              in
                  maybeBullet `shouldSatisfy` isNothing
        it "keeps firing while the trigger is pressed"
            $ let
                  old     = createPlayer (V2 0 0) 0 Red (Just 0)
                  between = fst $ fireBullet
                      [createTriggerEvent 0 JoyButtonPressed]
                      8
                      old
                  maybeBullet = snd $ fireBullet [] 8 $ setReloadTime 0 between
              in
                  maybeBullet `shouldSatisfy` isJust
        it "stops firing when the trigger is released"
            $ let
                  old = createPlayer (V2 0 0) 0 Red (Just 0)
                  (between, _) =
                      fireBullet [createTriggerEvent 0 JoyButtonPressed] 8 old
                  reloadedBetween = setReloadTime 0 between
                  triggerReleased = createTriggerEvent 0 JoyButtonReleased
                  maybeBullet =
                      snd $ fireBullet [triggerReleased] 9 reloadedBetween
              in
                  maybeBullet `shouldSatisfy` isNothing
        it "triggers a bullet if the right trigger button is pressed"
            $ let
                  triggerPressed =
                      JoyAxisEventData 0 5 (triggerMinFireValue + 1)
                  player = createPlayer (V2 0 0) 0 Red (Just 0)
                  maybeBullet =
                      snd $ fireBullet [toEvent triggerPressed] 8 player
              in
                  maybeBullet `shouldSatisfy` isJust
        it
                ("does not trigger a bullet if the right trigger button is not"
                ++ " pressed far enough"
                )
            $ let
                  triggerPressed =
                      JoyAxisEventData 0 5 (triggerMinFireValue - 1)
                  player = createPlayer (V2 0 0) 0 Red (Just 0)
                  maybeBullet =
                      snd $ fireBullet [toEvent triggerPressed] 8 player
              in
                  maybeBullet `shouldSatisfy` isNothing
        it "respects the sequential order of the supplied events"
            $ let
                  old             = createPlayer (V2 0 0) 0 Red (Just 0)
                  triggerPressed  = createTriggerEvent 0 JoyButtonPressed
                  triggerReleased = createTriggerEvent 0 JoyButtonReleased
                  new =
                      fst $ fireBullet [triggerPressed, triggerReleased] 8 old
              in
                  getGunState new `shouldBe` Idle

    describe "squareSector" $ do
        let ?epsilon = 0.00001
        let tan30    = tan $ pi / 6
        it "gives no points if angle is 0 deg" $ squareSector 0 `shouldBe` []
        it "gives no points if angle is less than 0 deg"
            $          squareSector (-pi / 4)
            `shouldBe` []
        it "gives a square sector for 30 deg"
            $                squareSector (pi / 6)
            `shouldApproxBe` [R.V2 1 (-tan30), R.V2 1 0, R.V2 0 0]
        it "gives a square sector for 45 deg"
            $                squareSector (pi / 4)
            `shouldApproxBe` [R.V2 1 (-1), R.V2 1 0, R.V2 0 0]
        it "gives a square sector for 60 deg"
            $                squareSector (pi / 3)
            `shouldApproxBe` [R.V2 tan30 (-1), R.V2 1 (-1), R.V2 1 0, R.V2 0 0]
        it "gives a square sector for 90 deg"
            $                squareSector (pi / 2)
            `shouldApproxBe` [R.V2 0 (-1), R.V2 1 (-1), R.V2 1 0, R.V2 0 0]
        it "gives a square sector for 120 deg"
            $                squareSector (pi * 2 / 3)
            `shouldApproxBe` [ R.V2 (-tan30) (-1)
                             , R.V2 1 (-1)
                             , R.V2 1 0
                             , R.V2 0 0
                             ]
        it "gives a square sector for 135 deg"
            $                squareSector (pi * 3 / 4)
            `shouldApproxBe` [R.V2 (-1) (-1), R.V2 1 (-1), R.V2 1 0, R.V2 0 0]
        it "gives a square sector for 150 deg"
            $                squareSector (pi * 5 / 6)
            `shouldApproxBe` [ R.V2 (-1) (-tan30)
                             , R.V2 (-1) (-1)
                             , R.V2 1 (-1)
                             , R.V2 1 0
                             , R.V2 0 0
                             ]
        it "gives a square sector for 180 deg"
            $                squareSector pi
            `shouldApproxBe` [ R.V2 (-1) 0
                             , R.V2 (-1) (-1)
                             , R.V2 1 (-1)
                             , R.V2 1 0
                             , R.V2 0 0
                             ]
        it "gives a square sector for 210 deg"
            $                squareSector (pi * 7 / 6)
            `shouldApproxBe` [ R.V2 (-1) tan30
                             , R.V2 (-1) (-1)
                             , R.V2 1 (-1)
                             , R.V2 1 0
                             , R.V2 0 0
                             ]
        it "gives a square sector for 225 deg"
            $                squareSector (pi * 5 / 4)
            `shouldApproxBe` [ R.V2 (-1) 1
                             , R.V2 (-1) (-1)
                             , R.V2 1 (-1)
                             , R.V2 1 0
                             , R.V2 0 0
                             ]
        it "gives a square sector for 240 deg"
            $                squareSector (pi * 4 / 3)
            `shouldApproxBe` [ R.V2 (-tan30) 1
                             , R.V2 (-1) 1
                             , R.V2 (-1) (-1)
                             , R.V2 1 (-1)
                             , R.V2 1 0
                             , R.V2 0 0
                             ]
        it "gives a square sector for 270 deg"
            $                squareSector (pi * 3 / 2)
            `shouldApproxBe` [ R.V2 0 1
                             , R.V2 (-1) 1
                             , R.V2 (-1) (-1)
                             , R.V2 1 (-1)
                             , R.V2 1 0
                             , R.V2 0 0
                             ]
        it "gives a square sector for 300 deg"
            $                squareSector (pi * 5 / 3)
            `shouldApproxBe` [ R.V2 tan30 1
                             , R.V2 (-1) 1
                             , R.V2 (-1) (-1)
                             , R.V2 1 (-1)
                             , R.V2 1 0
                             , R.V2 0 0
                             ]
        it "gives a square sector for 315 deg"
            $                squareSector (pi * 7 / 4)
            `shouldApproxBe` [ R.V2 1 1
                             , R.V2 (-1) 1
                             , R.V2 (-1) (-1)
                             , R.V2 1 (-1)
                             , R.V2 1 0
                             , R.V2 0 0
                             ]
        it "gives a square sector for 330 deg"
            $                squareSector (pi * 11 / 6)
            `shouldApproxBe` [ R.V2 1 tan30
                             , R.V2 1 1
                             , R.V2 (-1) 1
                             , R.V2 (-1) (-1)
                             , R.V2 1 (-1)
                             , R.V2 1 0
                             , R.V2 0 0
                             ]
        it "gives a square for 360 deg"
            $          squareSector (2 * pi)
            `shouldBe` [R.V2 1 1, R.V2 (-1) 1, R.V2 (-1) (-1), R.V2 1 (-1)]
        it "gives a square for angles above 360 deg"
            $          squareSector (pi * 9 / 4)
            `shouldBe` [R.V2 1 1, R.V2 (-1) 1, R.V2 (-1) (-1), R.V2 1 (-1)]

    describe "scaleAndOffset"
        $ it "scales and offsets the points with a value"
        $ scaleAndOffset 2 [R.V2 1 (-1), R.V2 (-1) (-1), R.V2 (-1) 1, R.V2 1 1]
        `shouldBe` [R.V2 4 0, R.V2 0 0, R.V2 0 4, R.V2 4 4]

    describe "inflictDamage" $ do
        it "reduces the health of the player"
            $ let old = createPlayer (V2 0 0) 0 Red Nothing
              in  getHealth (inflictDamage 0.15 old) `shouldBe` 0.85
        it "resets the players health to max if health is reduced below zero"
            $ let old = setHealth 0.05 $ createPlayer (V2 0 0) 0 Red Nothing
              in  getHealth (inflictDamage 0.15 old) `shouldBe` playerMaxHealth

