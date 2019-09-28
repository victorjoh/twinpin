module GameSpec where

import           Test.Hspec
import           Game
import           SDL.Vect
import           SDL.Event
import           SDL.Input.Joystick             ( JoyButtonState
                                                    ( JoyButtonPressed
                                                    )
                                                )
import           SDL.Internal.Types             ( Window(..) )
import           Foreign.Ptr                    ( nullPtr )
import           Foreign.C.Types
import           SDL.Video.Renderer             ( Rectangle(..) )

spec :: Spec
spec = do
    describe "gameTextureFiles" $ do
        it "retreives all the paths to texture files in the game" $ do
            gameTextureFiles
                `shouldMatchList` ["gen/shot.bmp", "gen/player.bmp"]

    describe "toDrawableGame" $ do
        it "converts from game to something that can be drawn by SDL"
            $          do
                           toDrawableGame createGame
            `shouldBe` [ ( "gen/player.bmp"
                         , Just (Rectangle (P (V2 0 0)) (V2 32 32))
                         , 0
                         )
                       ]

    describe "updateGame" $ do
        it "updates the player given the right event" $ do
            toDrawableGame
                    (updateGame
                        createGame
                        [ Event 0 (JoyAxisEvent (JoyAxisEventData 0 0 10000))
                        , Event 0 (JoyAxisEvent (JoyAxisEventData 0 1 20000))
                        ]
                        50
                        (V2 200 100)
                    )
                `shouldBe` [ ( "gen/player.bmp"
                             , Just (Rectangle (P (V2 5 10)) (V2 32 32))
                             , 0
                             )
                           ]
        it "creates a shot given the right event" $ do
            toDrawableGame
                    (updateGame
                        createGame
                        [ Event
                              0
                              (JoyButtonEvent
                                  (JoyButtonEventData 0 5 JoyButtonPressed)
                              )
                        ]
                        50
                        (V2 50 50)
                    )
                `shouldContain` [ ( "gen/shot.bmp"
                                  , Just (Rectangle (P (V2 46 11)) (V2 11 11))
                                  , 0
                                  )
                                ]
        it "removes a shot if it is out of bounds" $ do
            toDrawableGame
                    (updateGame
                        (updateGame
                            createGame
                            [ Event
                                  0
                                  (JoyButtonEvent
                                      (JoyButtonEventData 0 5 JoyButtonPressed)
                                  )
                            ]
                            25
                            (V2 45 45)
                        )
                        []
                        50
                        (V2 45 45)
                    )
                `shouldBe` [ ( "gen/player.bmp"
                             , Just (Rectangle (P (V2 0 0)) (V2 32 32))
                             , 0
                             )
                           ]
        it "can handle multiple shots at the same time" $ do
            toDrawableGame
                    (updateGame
                        (updateGame
                            createGame
                            [ Event
                                  0
                                  (JoyButtonEvent
                                      (JoyButtonEventData 0 5 JoyButtonPressed)
                                  )
                            ]
                            25
                            (V2 45 45)
                        )
                        [ Event 0 (JoyAxisEvent (JoyAxisEventData 0 4 10000))
                        , Event
                            0
                            (JoyButtonEvent
                                (JoyButtonEventData 0 5 JoyButtonPressed)
                            )
                        ]
                        50
                        (V2 50 50)
                    )
                `shouldContain` [ ( "gen/shot.bmp"
                                  , Just (Rectangle (P (V2 46 11)) (V2 11 11))
                                  , 0
                                  )
                                , ( "gen/shot.bmp"
                                  , Just (Rectangle (P (V2 11 28)) (V2 11 11))
                                  , 0
                                  )
                                ]
        it "is not finished when the user has not closed the window" $ do
            not $ isFinished $ updateGame createGame [] 50 (V2 50 50)
        it "is finished when the user closes the window" $ do
            isFinished $ updateGame
                createGame
                [ Event
                      0
                      (WindowClosedEvent
                          (WindowClosedEventData (Window nullPtr))
                      )
                ]
                50
                (V2 50 50)
