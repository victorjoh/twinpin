{-# LANGUAGE ImplicitParams #-}

module PlayerSpec where

import           Test.Hspec
import           Player
import           PlayerUtil
import           Space
import           Circle
import           Bullet                         ( createBullet )
import           SDL                     hiding ( Epsilon )
import           SDL.Input.Joystick             ( JoyButtonState(..) )
import           Data.Maybe
import           Data.List                      ( replicate )
import           Graphics.Rasterific     hiding ( V2(..) )
import qualified Graphics.Rasterific           as R
                                                ( V2(..) )
import           Test.HUnit.Base                ( Assertion )
import           Control.Monad                  ( unless )
import           Test.HUnit.Lang                ( HUnitFailure(..)
                                                , FailureReason(..)
                                                )
import           Control.Exception              ( throwIO )
import           Data.CallStack
import           Data.Foldable                  ( toList )

location :: HasCallStack => Maybe SrcLoc
location = case reverse callStack of
    (_, loc) : _ -> Just loc
    []           -> Nothing

type Epsilon = Float

class Show a => Approx a where
    isApproxEqual :: Epsilon -> a -> a -> Bool

instance Approx a => Approx [a] where
    isApproxEqual e v1 v2 =
        length v1 == length v2 && and (zipWith (isApproxEqual e) v1 v2)

instance (Approx a, Approx b) => Approx (Either a b) where
    isApproxEqual e v1 v2 = case (v1, v2) of
        (Left  _ , Right _ ) -> False
        (Right _ , Left _  ) -> False
        (Left  l1, Left l2 ) -> isApproxEqual e l1 l2
        (Right r1, Right r2) -> isApproxEqual e r1 r2

instance Approx CubicBezier where
    isApproxEqual = isApproxEqual' (\(CubicBezier a b c d) -> [a, b, c, d])

instance Approx a => Approx (R.V2 a) where
    isApproxEqual = isApproxEqual' toList

instance Approx Float where
    isApproxEqual e v1 v2 = e >= abs (v1 - v2)

instance Approx Line where
    isApproxEqual = isApproxEqual' (\(Line a b) -> [a, b])

isApproxEqual' :: Approx a => (c -> [a]) -> Epsilon -> c -> c -> Bool
isApproxEqual' toList' e v1 v2 = isApproxEqual e (toList' v1) (toList' v2)

shouldApproxBe
    :: (HasCallStack, Approx a, ?epsilon::Epsilon) => a -> a -> Assertion
shouldApproxBe actual expected =
    unless (isApproxEqual ?epsilon actual expected) $ throwIO
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
        it "fires a bullet if the right trigger button is pressed"
            $ let
                  triggerPressed =
                      JoyAxisEventData 0 5 (triggerMinFireValue + 1)
                  player = createPlayer (V2 0 0) 0 Red (Just 0)
                  maybeBullet =
                      snd $ fireBullet [toEvent triggerPressed] 8 player
              in
                  maybeBullet `shouldSatisfy` isJust
        it
                (  "does not fire a bullet if the right trigger button is not"
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

    describe "inflictDamage" $ do
        it "reduces the health of the player"
            $ let old = createPlayer (V2 0 0) 0 Red Nothing
              in  getHealth (inflictDamage 0.15 old) `shouldBe` 0.85
        it "resets the players health to max if health is reduced below zero"
            $ let old = setHealth 0.05 $ createPlayer (V2 0 0) 0 Red Nothing
              in  getHealth (inflictDamage 0.15 old) `shouldBe` playerMaxHealth
        it
                (  "increases the player's number of deaths by 1 when their "
                ++ "health is depleted"
                )
            $ let old = setHealth 0.05 $ createPlayer (V2 0 0) 0 Red Nothing
              in  getDeaths (inflictDamage 0.15 old) `shouldBe` 1

    describe "aimShadowShape" $ do
        let ?epsilon = 0.01
        it "is an empty shape if length is zero"
            $          aimShadowShape 0
            `shouldBe` []
        it "should be a circle segment if less than 1/3"
            $                aimShadowShape (1 / 6)
            `shouldApproxBe` [ Left $ CubicBezier
                                 (R.V2 (-1 / 6) (-1 / sqrt 12))
                                 (R.V2 (-0.26695037) (-0.22991335))
                                 (R.V2 (-1 / 3) (-0.12264779))
                                 (R.V2 (-1 / 3) 0)
                             , Left $ CubicBezier
                                 (R.V2 (-1 / 3) 0)
                                 (R.V2 (-1 / 3) 0.12264779)
                                 (R.V2 (-0.26695037) 0.22991335)
                                 (R.V2 (-1 / 6) (1 / sqrt 12))
                             , Right $ Line (R.V2 (-1 / 6) (1 / sqrt 12))
                                            (R.V2 (-1 / 6) (-1 / sqrt 12))
                             ]
        it
                (  "should be half a circle and a rectangle if between 0 and "
                ++ "~4/3"
                )
            $                aimShadowShape (5 / 6)
            `shouldApproxBe` [ Left topLeftCurve
                             , Left bottomLeftCurve
                             , Right $ Line (R.V2 0 (1 / 3)) (R.V2 0.5 (1 / 3))
                             , Right
                                 $ Line (R.V2 0.5 (1 / 3)) (R.V2 0.5 (-1 / 3))
                             , Right
                                 $ Line (R.V2 0.5 (-1 / 3)) (R.V2 0 (-1 / 3))
                             ]
        it
                (  "should be half a circle, a rectangle and another split "
                ++ "circle segment when between ~4/3 and 4/3"
                )
            $                aimShadowShape (4 / 3 - 1e-2)
            `shouldApproxBe` [ Left topLeftCurve
                             , Left bottomLeftCurve
                             , Right bottomLine
                             , Left $ CubicBezier
                                 (R.V2 0.9383697 0.34615636)
                                 (R.V2 0.9717603 0.25577885)
                                 (R.V2 0.99245834 0.15927286)
                                 (R.V2 0.9982961 5.880624e-2)
                             , Right $ Line (R.V2 0.9982961 5.880624e-2)
                                            (R.V2 0.9982961 (-5.880624e-2))
                             , Left $ CubicBezier
                                 (R.V2 0.9982961 (-5.880624e-2))
                                 (R.V2 0.99245834 (-0.15927286))
                                 (R.V2 0.9717603 (-0.25577885))
                                 (R.V2 0.9383697 (-0.34615636))
                             , Right topLine
                             ]
        it
                (  "should be half a circle, a rectangle and a circle segment "
                ++ "when 4/3"
                )
            $          aimShadowShape (4 / 3)
            `shouldBe` aimShape
