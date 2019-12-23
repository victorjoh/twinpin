module PlayerSpec where

import           Test.Hspec
import           Player
import           PlayerUtil
import           Space
import           Circle
import           Shot                           ( createShot )
import           SDL.Vect
import           SDL.Event
import           SDL.Input.Joystick             ( JoyButtonState
                                                    ( JoyButtonPressed
                                                    )
                                                )
import           SDL.Video.Renderer             ( Rectangle(..) )
import           Codec.Picture.Types

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

    describe "triggerShot" $ do
        it "triggers a shot if right bumper button is pressed"
            $ let playerPos      = V2 40 50
                  playerAngle    = 30
                  triggerPressed = JoyButtonEventData 0 5 JoyButtonPressed
              in  triggerShot (toEvents [triggerPressed])
                              (createPlayer playerPos playerAngle 0)
                      `shouldBe` Just (createShot playerPos playerAngle)
        it
                (  "does not trigger a shot if some other button is pressed"
                ++ " (x button)"
                )
            $ let unusedPressed = JoyButtonEventData 0 0 JoyButtonPressed
              in  triggerShot (toEvents [unusedPressed])
                              (createPlayer (V2 40 50) 30 0)
                      `shouldBe` Nothing
        it "ignores events from different gamepads"
            $ let playerId       = 1
                  triggerPressed = JoyButtonEventData 0 5 JoyButtonPressed
              in  triggerShot (toEvents [triggerPressed])
                              (createPlayer (V2 40 50) 30 playerId)
                      `shouldBe` Nothing
