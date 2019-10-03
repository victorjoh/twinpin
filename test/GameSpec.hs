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
import           Data.Tuple.Extra               ( fst3 )

spec :: Spec
spec = do
    describe "gameTextureFiles" $ do
        it "retreives all the paths to texture files in the game" $ do
            gameTextureFiles
                `shouldMatchList` ["gen/shot.bmp", "gen/player.bmp"]

    describe "toDrawableGame" $ do
        it "converts from game to something that can be drawn by SDL"
            $               do
                                toDrawableGame (createGame (V2 200 70))
            `shouldContain` [ ( "gen/player.bmp"
                              , Just (Rectangle (P (V2 32 19)) (V2 32 32))
                              , 0
                              )
                            , ( "gen/player.bmp"
                              , Just (Rectangle (P (V2 136 19)) (V2 32 32))
                              , 180
                              )
                            ]

    describe "updateGame" $ do
        it "updates the player given the right event" $ do
            toDrawableGame
                    (updateGame
                        [ Event 0 (JoyAxisEvent (JoyAxisEventData 0 0 10000))
                        , Event 0 (JoyAxisEvent (JoyAxisEventData 0 1 20000))
                        ]
                        50
                        (V2 200 100)
                        (createGame (V2 200 100))
                    )
                `shouldContain` [ ( "gen/player.bmp"
                                  , Just (Rectangle (P (V2 37 44)) (V2 32 32))
                                  , 0
                                  )
                                ]
        it "creates a shot given the right event" $ do
            toDrawableGame
                    (updateGame
                        [ Event
                              0
                              (JoyButtonEvent
                                  (JoyButtonEventData 0 5 JoyButtonPressed)
                              )
                        ]
                        50
                        (V2 80 80)
                        (createGame (V2 80 80))
                    )
                `shouldContain` [ ( "gen/shot.bmp"
                                  , Just (Rectangle (P (V2 78 35)) (V2 11 11))
                                  , 0
                                  )
                                ]
        it "removes a shot if it is out of bounds" $ do
            toDrawableGame
                    (updateGame
                        []
                        50
                        (V2 70 70)
                        (updateGame
                            [ Event
                                  0
                                  (JoyButtonEvent
                                      (JoyButtonEventData 0 5 JoyButtonPressed)
                                  )
                            ]
                            25
                            (V2 70 70)
                            (createGame (V2 70 70))
                        )
                    )
                `shouldNotSatisfy` any ((== "gen/shot.bmp") . fst3)
        it "can handle multiple shots at the same time" $ do
            toDrawableGame
                    (updateGame
                        [ Event 0 (JoyAxisEvent (JoyAxisEventData 0 4 10000))
                        , Event
                            0
                            (JoyButtonEvent
                                (JoyButtonEventData 0 5 JoyButtonPressed)
                            )
                        ]
                        50
                        (V2 100 100)
                        (updateGame
                            [ Event
                                  0
                                  (JoyButtonEvent
                                      (JoyButtonEventData 0 5 JoyButtonPressed)
                                  )
                            ]
                            25
                            (V2 100 100)
                            (createGame (V2 100 100))
                        )
                    )
                `shouldContain` [ ( "gen/shot.bmp"
                                  , Just (Rectangle (P (V2 78 45)) (V2 11 11))
                                  , 0
                                  )
                                , ( "gen/shot.bmp"
                                  , Just (Rectangle (P (V2 43 62)) (V2 11 11))
                                  , 0
                                  )
                                ]
        it "is not finished when the user has not closed the window" $ do
            not $ isFinished $ updateGame []
                                          50
                                          (V2 50 50)
                                          (createGame (V2 50 50))
        it "is finished when the user closes the window" $ do
            isFinished $ updateGame
                [ Event
                      0
                      (WindowClosedEvent
                          (WindowClosedEventData (Window nullPtr))
                      )
                ]
                50
                (V2 50 50)
                (createGame (V2 50 50))
