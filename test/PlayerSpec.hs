{-# LANGUAGE ImplicitParams #-}

module PlayerSpec where

import Approx
import Bullet (createBullet)
import Circle
import Data.Maybe
import Graphics.Rasterific hiding (V2 (..))
import qualified Graphics.Rasterific as R
  ( V2 (..),
  )
import Player
import PlayerUtil
import SDL hiding (Epsilon)
import Space
import SpaceUtil ()
import Test.Hspec
import VisualUtil ()

spec :: Spec
spec = do
  describe "drawPlayer" $
    it
      ( "transforms the position and size of the player to something"
          ++ " that SDL is familiar with"
      )
      $ let player = createPlayer (V2 40 50) pi Red Nothing
            positions = map fst $ drawPlayer player
         in positions
              `shouldBe` replicate 2 (toTextureArea $ playerToCircle player)

  describe "updatePlayer" $ do
    let ?epsilon = 0.0001
    it "moves the player according to the changed left stick position" $
      let old = createPlayer (V2 40 50) 0 Red (Just 3)
          stickXPos = JoyAxisEventData 3 0 10000
          stickYPos = JoyAxisEventData 3 1 20000
          passedTime = 50
          new =
            updatePlayer
              (toEvents [stickXPos, stickYPos])
              passedTime
              (Obstacles (createBounds 800 600) [])
              old
       in getPlayerPosition new `shouldBe` V2 49 68
    it
      ( "does not move the player if the left stick position is very"
          ++ " close to the default position"
      )
      $ let oldPos = V2 40 50
            old =
              setPlayerVelocity (V2 0.1 0.2) $
                createPlayer oldPos 0 Red (Just 0)
            stickXPos = JoyAxisEventData 0 0 4000
            stickYPos = JoyAxisEventData 0 1 4000
            new =
              updatePlayer
                (toEvents [stickXPos, stickYPos])
                50
                (Obstacles (createBounds 800 600) [])
                old
         in getPlayerPosition new `shouldBe` oldPos
    it "remembers the velocity changed from previous updates" $
      let old =
            setPlayerVelocity (V2 0.1 0.2) $
              createPlayer (V2 40 50) 0 Red Nothing
          new =
            updatePlayer
              []
              50
              (Obstacles (createBounds 800 600) [])
              old
       in getPlayerPosition new `shouldBe` V2 45 60
    it "limits the player position by the bounds given" $
      let bounds = createBounds 100 600
          old =
            setPlayerVelocity (V2 0.1 0) $
              createPlayer (V2 40 50) 0 Red Nothing
          new = updatePlayer [] 1000 (Obstacles bounds []) old
       in getPlayerPosition new `shouldBe` V2 (100 - playerRadius) 50
    it "changes the player's aim depending on the right stick position" $
      let stickXPos = JoyAxisEventData 0 3 10000
          stickYPos = JoyAxisEventData 0 4 (-10000)
          old = createPlayer (V2 50 50) 0 Red (Just 0)
          new =
            updatePlayer
              (toEvents [stickXPos, stickYPos])
              1
              (Obstacles (createBounds 800 600) [])
              old
       in getPlayerAngle new `shouldBe` - pi / 4
    it
      ( "does not change the player's aim if the right stick is very"
          ++ " close to the default position"
      )
      $ let oldAngle = 30
            stickXPos = JoyAxisEventData 0 3 4000
            stickYPos = JoyAxisEventData 0 4 (-4000)
            old = createPlayer (V2 50 50) oldAngle Red (Just 0)
            new =
              updatePlayer
                (toEvents [stickXPos, stickYPos])
                1
                (Obstacles (createBounds 800 600) [])
                old
         in getPlayerAngle new `shouldBe` oldAngle
    it "ignores unused joystick buttons (x button)" $
      let unused = JoyButtonEventData 0 0 JoyButtonPressed
          old = createPlayer (V2 40 50) 0 Red (Just 0)
          new =
            updatePlayer
              (toEvents [unused])
              50
              (Obstacles (createBounds 800 600) [])
              old
       in new `shouldBe` old
    it "ignores events from different gamepads" $
      let old = createPlayer (V2 40 50) 0 Red (Just 0)
          stickYPos = JoyAxisEventData 1 1 20000
          new =
            updatePlayer
              (toEvents [stickYPos])
              50
              (Obstacles (createBounds 800 600) [])
              old
       in new `shouldBe` old
    it "reduces the reload time according to the passed time" $
      let oldReloadTime = 150
          old =
            setReloadTime oldReloadTime $
              createPlayer (V2 40 50) 0 Red Nothing
          new =
            updatePlayer
              []
              50
              (Obstacles (createBounds 800 600) [])
              old
       in getReloadTime new `shouldBe` 100
    it "reduces the reload time no less than 0" $
      let oldReloadTime = 30
          old =
            setReloadTime oldReloadTime $
              createPlayer (V2 40 50) 0 Red Nothing
          new =
            updatePlayer
              []
              50
              (Obstacles (createBounds 800 600) [])
              old
       in getReloadTime new `shouldBe` 0
    it "removes the joystick if it is disconnected" $
      let joystickId = 5
          old = createPlayer (V2 100 100) 0 Red (Just joystickId)
          joystickRemoved =
            toEvent $ JoyDeviceEventData JoyDeviceRemoved joystickId
          new =
            updatePlayer
              [joystickRemoved]
              1
              (Obstacles (createBounds 800 600) [])
              old
       in new `shouldNotSatisfy` hasJoystick
    it "does not remove the joystick if some other joystick is disconnected" $
      let playerId = 5
          eventId = 9
          old = createPlayer (V2 100 100) 0 Red (Just playerId)
          otherJoystickRemoved =
            toEvent $ JoyDeviceEventData JoyDeviceRemoved eventId
          new =
            updatePlayer
              [otherJoystickRemoved]
              1
              (Obstacles (createBounds 800 600) [])
              old
       in new `shouldSatisfy` hasJoystick

    let boostIsActivatedWhen event =
          let old = createPlayer (V2 0 0) 0 Red (Just 0)
              obstacles = Obstacles (createBounds 800 600) []
              new = updatePlayer [event] 1 obstacles old
           in getBoostTime new `shouldBe` fullBoostCycle
    it "activates the boost when left bumper button is pressed" $
      let leftBumperPressed =
            toEvent $ JoyButtonEventData 0 4 JoyButtonPressed
       in boostIsActivatedWhen leftBumperPressed
    it "activates the boost when left trigger is pressed" $
      let leftTiggerPressed =
            toEvent $ JoyAxisEventData 0 2 (triggerMinFireValue + 1)
       in boostIsActivatedWhen leftTiggerPressed

    let boostIsNotActivatedWhen event =
          let old = createPlayer (V2 0 0) 0 Red (Just 0)
              obstacles = Obstacles (createBounds 800 600) []
              new = updatePlayer [event] 1 obstacles old
           in getBoostTime new `shouldBe` 0
    it
      ( "only activates the boost for the player who pressed the "
          ++ "left bumper button"
      )
      $ let otherPlayerPressedLeftBumper =
              toEvent $ JoyButtonEventData 7 4 JoyButtonPressed
         in boostIsNotActivatedWhen otherPlayerPressedLeftBumper
    it
      ( "only activates the boost for the player who pressed the "
          ++ "left trigger button"
      )
      $ let otherPlayerPressedLeftTrigger =
              toEvent $ JoyAxisEventData 7 2 (triggerMinFireValue + 1)
         in boostIsNotActivatedWhen otherPlayerPressedLeftTrigger
    it "only activates the boost if the left trigger is pressed far enough" $
      let leftTiggerIsNotPressedFarEnough =
            toEvent $ JoyAxisEventData 0 2 (triggerMinFireValue - 1)
       in boostIsNotActivatedWhen leftTiggerIsNotPressedFarEnough
    context "when boost button is pressed and player is stationary" $
      it "it boosts the player in the aim direction" $
        let old = createPlayer (V2 100 100) (pi * 3 / 2) Red (Just 0)
            obstacles = Obstacles (createBounds 800 600) []
            leftBumperPressed =
              toEvent $ JoyButtonEventData 0 4 JoyButtonPressed
            new = updatePlayer [leftBumperPressed] 10 obstacles old
         in getPlayerPosition new
              `shouldBe` V2 100 (100 - 10 * boostSpeed)
    context
      ( "when the player does not have a joystick assigned in boost "
          ++ "mode"
      )
      $ it "still uses up the boost" $
        let old =
              setPlayerBoostTime fullBoostCycle $
                createPlayer (V2 100 100) 0 Red Nothing
            obstacles = Obstacles (createBounds 800 600) []
            new = updatePlayer [] 10 obstacles old
         in getBoostTime new `shouldBe` fullBoostCycle - 10
    context "when boost is active" $ do
      let leftBumperPressed = JoyButtonEventData 0 4 JoyButtonPressed
          old =
            updatePlayer [toEvent leftBumperPressed] 0 obstacles $
              setPlayerVelocity (V2 0.5 0) $
                createPlayer (V2 50 300) 0 Red (Just 0)
          obstacles = Obstacles (createBounds 800 600) []
          expectedPositionAfter100ms = V2 (50 + 100 * boostSpeed) 300
      it "moves the player fast" $
        let new = updatePlayer [] 100 obstacles old
         in getPlayerPosition new
              `shouldBe` expectedPositionAfter100ms
      it "locks the speed between two updates" $
        let new =
              updatePlayer [] 50 obstacles $
                updatePlayer [] 50 obstacles old
         in getPlayerPosition new
              `shouldBe` expectedPositionAfter100ms
      it "leaves the movement unaffected by left thumbstick changes" $
        let leftStickMovement = toEvent $ JoyAxisEventData 0 1 20000
            new = updatePlayer [leftStickMovement] 100 obstacles old
         in getPlayerPosition new
              `shouldBe` expectedPositionAfter100ms
    context "when boost is deactivated" $
      it "moves the player in the direction of the left thumbstick" $
        let boostMovement = Movement (V2 1 0) (V2 1 0) fullBoostCycle
            old =
              setMovement boostMovement $
                createPlayer (V2 700 300) 0 Red (Just 0)
            obstacles = Obstacles (createBounds 800 600) []
            new =
              updatePlayer [] boostDuration obstacles $
                updatePlayer
                  [createMoveDownEvent 0 10 50]
                  50
                  obstacles
                  old
         in getPlayerPosition new
              `shouldApproxBe` V2 (800 - playerRadius) 310

  describe "fireBullet" $ do
    it "fires a bullet if the right bumper button is pressed" $
      let playerPos = V2 40 50
          playerAngle = 30
          player = createPlayer playerPos playerAngle Red (Just 0)
          (Gun aim _ _) = getGun player
          triggerPressed = JoyButtonEventData 0 5 JoyButtonPressed
       in fireBullet [toEvent triggerPressed] 8 player
            `shouldBe` ( setGun
                           (Gun aim minFiringInterval Firing)
                           player,
                         Just (createBullet playerPos playerAngle 8)
                       )
    it
      ( "does not fire a bullet if some other button is pressed"
          ++ " (x button)"
      )
      $ let unusedPressed = JoyButtonEventData 0 0 JoyButtonPressed
            maybeBullet =
              snd $
                fireBullet
                  (toEvents [unusedPressed])
                  8
                  (createPlayer (V2 40 50) 30 Red (Just 0))
         in maybeBullet `shouldSatisfy` isNothing
    it "ignores events from different gamepads" $
      let joystickId = 1
          maybeBullet =
            snd $
              fireBullet
                [createTriggerEvent 0 JoyButtonPressed]
                8
                (createPlayer (V2 40 50) 30 Red (Just joystickId))
       in maybeBullet `shouldSatisfy` isNothing
    it "does not fire a bullet if the player is reloading" $
      let player =
            setReloadTime 10 $ createPlayer (V2 0 0) 0 Red (Just 0)
          maybeBullet =
            snd $
              fireBullet
                [createTriggerEvent 0 JoyButtonPressed]
                8
                player
       in maybeBullet `shouldSatisfy` isNothing
    it "keeps firing while the trigger is pressed" $
      let old = createPlayer (V2 0 0) 0 Red (Just 0)
          between =
            fst $
              fireBullet
                [createTriggerEvent 0 JoyButtonPressed]
                8
                old
          maybeBullet = snd $ fireBullet [] 8 $ setReloadTime 0 between
       in maybeBullet `shouldSatisfy` isJust
    it "stops firing when the trigger is released" $
      let old = createPlayer (V2 0 0) 0 Red (Just 0)
          (between, _) =
            fireBullet [createTriggerEvent 0 JoyButtonPressed] 8 old
          reloadedBetween = setReloadTime 0 between
          triggerReleased = createTriggerEvent 0 JoyButtonReleased
          maybeBullet =
            snd $ fireBullet [triggerReleased] 9 reloadedBetween
       in maybeBullet `shouldSatisfy` isNothing
    it "fires a bullet if the right trigger button is pressed" $
      let triggerPressed =
            JoyAxisEventData 0 5 (triggerMinFireValue + 1)
          player = createPlayer (V2 0 0) 0 Red (Just 0)
          maybeBullet =
            snd $ fireBullet [toEvent triggerPressed] 8 player
       in maybeBullet `shouldSatisfy` isJust
    it
      ( "does not fire a bullet if the right trigger button is not"
          ++ " pressed far enough"
      )
      $ let triggerPressed =
              JoyAxisEventData 0 5 (triggerMinFireValue - 1)
            player = createPlayer (V2 0 0) 0 Red (Just 0)
            maybeBullet =
              snd $ fireBullet [toEvent triggerPressed] 8 player
         in maybeBullet `shouldSatisfy` isNothing
    it "respects the sequential order of the supplied events" $
      let old = createPlayer (V2 0 0) 0 Red (Just 0)
          triggerPressed = createTriggerEvent 0 JoyButtonPressed
          triggerReleased = createTriggerEvent 0 JoyButtonReleased
          new =
            fst $ fireBullet [triggerPressed, triggerReleased] 8 old
       in getGunState new `shouldBe` Idle

  describe "inflictDamage" $ do
    it "reduces the health of the player" $
      let old = createPlayer (V2 0 0) 0 Red Nothing
       in getHealth (inflictDamage 0.15 old) `shouldBe` 0.85
    it "resets the players health to max if health is reduced below zero" $
      let old = setHealth 0.05 $ createPlayer (V2 0 0) 0 Red Nothing
       in getHealth (inflictDamage 0.15 old) `shouldBe` playerMaxHealth
    it
      ( "increases the player's number of deaths by 1 when their "
          ++ "health is depleted"
      )
      $ let old = setHealth 0.05 $ createPlayer (V2 0 0) 0 Red Nothing
         in getDeaths (inflictDamage 0.15 old) `shouldBe` 1

  describe "aimShadowShape" $ do
    let ?epsilon = 0.01
    it "is an empty shape if length is zero" $
      aimShadowShape 0
        `shouldBe` []
    it "should be a circle segment if less than 1/3" $
      aimShadowShape (1 / 6)
        `shouldApproxBe` [ Left $
                             CubicBezier
                               (R.V2 (-0.23565605) (-0.23565605))
                               (R.V2 (-0.2959929) (-0.17531918))
                               (R.V2 (-1 / 3) (-9.1985844e-2))
                               (R.V2 (-1 / 3) 0),
                           Left $
                             CubicBezier
                               (R.V2 (-1 / 3) 0)
                               (R.V2 (-1 / 3) 9.1985844e-2)
                               (R.V2 (-0.2959929) 0.17531918)
                               (R.V2 (-0.23565605) 0.23565605),
                           Right $
                             Line
                               (R.V2 (-0.23565605) 0.23565605)
                               (R.V2 (-0.23565605) (-0.23565605))
                         ]
    it
      ( "should be half a circle and a rectangle if between 0 and "
          ++ "~4/3"
      )
      $ aimShadowShape (5 / 6)
        `shouldApproxBe` [ Left topLeftCurve,
                           Left bottomLeftCurve,
                           Right $ Line (R.V2 0 (1 / 3)) (R.V2 0.5 (1 / 3)),
                           Right $
                             Line (R.V2 0.5 (1 / 3)) (R.V2 0.5 (-1 / 3)),
                           Right $
                             Line (R.V2 0.5 (-1 / 3)) (R.V2 0 (-1 / 3))
                         ]
    it
      ( "should be half a circle, a rectangle and another split "
          ++ "circle segment when between ~4/3 and 4/3"
      )
      $ aimShadowShape (4 / 3 - 1e-2)
        `shouldApproxBe` [ Left topLeftCurve,
                           Left bottomLeftCurve,
                           Right bottomLine,
                           Left $
                             CubicBezier
                               (R.V2 0.9383697 0.34615636)
                               (R.V2 0.9717603 0.25577885)
                               (R.V2 0.99245834 0.15927286)
                               (R.V2 0.9982961 5.880624e-2),
                           Right $
                             Line
                               (R.V2 0.9982961 5.880624e-2)
                               (R.V2 0.9982961 (-5.880624e-2)),
                           Left $
                             CubicBezier
                               (R.V2 0.9982961 (-5.880624e-2))
                               (R.V2 0.99245834 (-0.15927286))
                               (R.V2 0.9717603 (-0.25577885))
                               (R.V2 0.9383697 (-0.34615636)),
                           Right topLine
                         ]
    it
      ( "should be half a circle, a rectangle and a circle segment "
          ++ "when 4/3"
      )
      $ aimShadowShape (4 / 3)
        `shouldBe` aimShape
