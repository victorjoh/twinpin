module PlayerSpec where

import           Test.Hspec
import           Player
import           Space
import           Shot                           ( createShot )
import           SDL.Vect
import           SDL.Event
import           SDL.Input.Joystick             ( JoyButtonState
                                                    ( JoyButtonPressed
                                                    )
                                                )
import           SDL.Video.Renderer             ( Rectangle(..) )

spec :: Spec
spec = do
    describe "toDrawablePlayer" $ do
        it "converts from player to something that can be drawn by SDL" $ do
            toDrawablePlayer (createPlayer (V2 40 50) 30 0)
                `shouldBe` ( "gen/player.bmp"
                           , Just (Rectangle (P (V2 24 34)) (V2 32 32))
                           , 30
                           )

    describe "updatePlayer" $ do
        it "moves the player according to the changed left stick position" $ do
            toDrawablePlayer
                    (updatePlayer
                        [ Event 0 (JoyAxisEvent (JoyAxisEventData 0 0 10000))
                        , Event 0 (JoyAxisEvent (JoyAxisEventData 0 1 20000))
                        ]
                        50
                        (Bounds2D (0, 250) (0, 200))
                        (createPlayer (V2 40 50) 0 0)
                    )
                `shouldBe` toDrawablePlayer (createPlayer (V2 45 60) 0 0)
        it
                (  "does not move the player if the left stick position is very"
                ++ " close to the default position"
                )
            $ do
                  toDrawablePlayer
                          (updatePlayer
                              [ Event
                                  0
                                  (JoyAxisEvent (JoyAxisEventData 0 0 4000))
                              , Event
                                  0
                                  (JoyAxisEvent (JoyAxisEventData 0 1 4000))
                              ]
                              50
                              (Bounds2D (0, 250) (0, 200))
                              (updatePlayer
                                  -- first give the player some velocity to see
                                  -- that the velocity is set to (0, 0) later on
                                  [ Event
                                      0
                                      (JoyAxisEvent (JoyAxisEventData 0 0 10000)
                                      )
                                  , Event
                                      0
                                      (JoyAxisEvent (JoyAxisEventData 0 1 20000)
                                      )
                                  ]
                                  50
                                  (Bounds2D (0, 250) (0, 200))
                                  (createPlayer (V2 40 50) 0 0)
                              )
                          )
                      `shouldBe` toDrawablePlayer (createPlayer (V2 45 60) 0 0)
        it "remembers the velocity changed from previous updates" $ do
            toDrawablePlayer
                    (updatePlayer
                        []
                        25
                        (Bounds2D (0, 250) (0, 200))
                        (updatePlayer
                            [ Event
                                0
                                (JoyAxisEvent (JoyAxisEventData 0 0 10000))
                            , Event
                                0
                                (JoyAxisEvent (JoyAxisEventData 0 1 20000))
                            ]
                            25
                            (Bounds2D (0, 250) (0, 200))
                            (createPlayer (V2 40 50) 0 0)
                        )
                    )
                `shouldBe` toDrawablePlayer (createPlayer (V2 45 60) 0 0)
        it "limits the player position by the bounds given" $ do
            toDrawablePlayer
                    (updatePlayer
                        [ Event 0 (JoyAxisEvent (JoyAxisEventData 0 0 10000))
                        , Event 0 (JoyAxisEvent (JoyAxisEventData 0 1 20000))
                        ]
                        50
                        (Bounds2D (0, 60) (0, 66))
                        (createPlayer (V2 40 50) 0 0)
                    )
                `shouldBe` toDrawablePlayer (createPlayer (V2 44 50) 0 0)
        it "changes the player's aim depending on the right stick position" $ do
            toDrawablePlayer
                    (updatePlayer
                        [ Event 0 (JoyAxisEvent (JoyAxisEventData 0 3 10000))
                        , Event 0 (JoyAxisEvent (JoyAxisEventData 0 4 (-10000)))
                        ]
                        500
                        (Bounds2D (0, 250) (0, 200))
                        (createPlayer (V2 40 50) 0 0)
                    )
                `shouldBe` toDrawablePlayer (createPlayer (V2 40 50) (-45) 0)
        it
                (  "does not change the player's aim if the right stick is very"
                ++ " close to the default position"
                )
            $          do
                           toDrawablePlayer
                               (updatePlayer
                                   [ Event 0 (JoyAxisEvent (JoyAxisEventData 0 3 4000))
                                   , Event
                                       0
                                       (JoyAxisEvent (JoyAxisEventData 0 4 (-4000)))
                                   ]
                                   500
                                   (Bounds2D (0, 250) (0, 200))
                                   (createPlayer (V2 40 50) 0 0)
                               )
            `shouldBe` toDrawablePlayer (createPlayer (V2 40 50) 0 0)
        it "ignores unused joystick buttons (left trigger)" $ do
            toDrawablePlayer
                    (updatePlayer
                        [Event 0 (JoyAxisEvent (JoyAxisEventData 0 2 10000))]
                        50
                        (Bounds2D (0, 250) (0, 200))
                        (createPlayer (V2 40 50) 0 0)
                    )
                `shouldBe` toDrawablePlayer (createPlayer (V2 40 50) 0 0)
        it "ignores events from different gamepads" $ do
            toDrawablePlayer
                    (updatePlayer
                        [Event 0 (JoyAxisEvent (JoyAxisEventData 0 0 20000))]
                        50
                        (Bounds2D (0, 250) (0, 200))
                        (createPlayer (V2 40 50) 0 1)
                    )
                `shouldBe` toDrawablePlayer (createPlayer (V2 40 50) 0 0)

    describe "triggerShot" $ do
        it "triggers a shot if right bumper button is pressed" $ do
            triggerShot
                    [ Event
                          0
                          (JoyButtonEvent
                              (JoyButtonEventData 0 5 JoyButtonPressed)
                          )
                    ]
                    (createPlayer (V2 40 50) 30 0)
                `shouldBe` Just (createShot (V2 40 50) 30)
        it
                (  "does not trigger a shot if some other button is pressed"
                ++ " (x button)"
                )
            $          do
                           triggerShot
                               [ Event
                                     0
                                     (JoyButtonEvent
                                         (JoyButtonEventData 0 0 JoyButtonPressed)
                                     )
                               ]
                               (createPlayer (V2 40 50) 30 0)
            `shouldBe` Nothing
        it "ignores events from different gamepads" $ do
            triggerShot
                    [ Event
                          0
                          (JoyButtonEvent
                              (JoyButtonEventData 0 5 JoyButtonPressed)
                          )
                    ]
                    (createPlayer (V2 40 50) 30 1)
                `shouldBe` Nothing
