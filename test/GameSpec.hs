module GameSpec where

import           Test.Hspec
import           Game
import           Player
import           Circle
import           Space
import           Shot
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
import           SDL.Raw.Types                  ( JoystickID )

moveRight :: JoystickID -> Event
moveRight playerId = Event 0 (JoyAxisEvent (JoyAxisEventData playerId 0 20000))

getFirstPlayer :: Game -> Player
getFirstPlayer (Game _ (Movables _ ((PlayerWithBarrel player _) : _)) _ _) =
    player

getPlayerPosition :: Player -> Position2D
getPlayerPosition player = let (Circle pos _) = playerToCircle player in pos

playerRadius :: Radius
playerRadius = playerSide / 2

getShots :: Game -> [Shot]
getShots (Game _ (Movables shots _) _ _) = shots

spec :: Spec
spec = do
    describe "gameTextureFiles" $ do
        it "retreives all the paths to texture files in the game" $ do
            gameTextureFiles
                `shouldMatchList` [ "gen/shot.bmp"
                                  , "gen/shot-hit.bmp"
                                  , "gen/player.bmp"
                                  , "gen/pillar.bmp"
                                  ]

    describe "toDrawableGame" $ do
        it "converts from game to something that can be drawn by SDL"
            $                 do
                                  toDrawableGame (createGame (V2 2000 700))
            `shouldMatchList` [ ( "gen/player.bmp"
                                , Just (Rectangle (P (V2 32 334)) (V2 32 32))
                                , 0.0
                                )
                              , ( "gen/player.bmp"
                                , Just (Rectangle (P (V2 1936 334)) (V2 32 32))
                                , 180.0
                                )
                              , ( "gen/pillar.bmp"
                                , Just (Rectangle (P (V2 96 96)) (V2 96 96))
                                , 0.0
                                )
                              , ( "gen/pillar.bmp"
                                , Just (Rectangle (P (V2 1808 96)) (V2 96 96))
                                , 0.0
                                )
                              , ( "gen/pillar.bmp"
                                , Just (Rectangle (P (V2 96 508)) (V2 96 96))
                                , 0.0
                                )
                              , ( "gen/pillar.bmp"
                                , Just (Rectangle (P (V2 1808 508)) (V2 96 96))
                                , 0.0
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
                        (V2 800 80)
                        (createGame (V2 800 80))
                    )
                `shouldContain` [ ( "gen/shot.bmp"
                                  , Just (Rectangle (P (V2 43 35)) (V2 11 11))
                                  , 0
                                  )
                                ]
        it "removes a shot if it is out of bounds" $ do
            toDrawableGame
                    (updateGame
                        []
                        70
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
                        (V2 200 100)
                        (updateGame
                            [ Event
                                  0
                                  (JoyButtonEvent
                                      (JoyButtonEventData 0 5 JoyButtonPressed)
                                  )
                            ]
                            25
                            (V2 200 100)
                            (createGame (V2 200 100))
                        )
                    )
                `shouldContain` [ ( "gen/shot.bmp"
                                  , Just (Rectangle (P (V2 60 45)) (V2 11 11))
                                  , 0
                                  )
                                , ( "gen/shot.bmp"
                                  , Just (Rectangle (P (V2 43 45)) (V2 11 11))
                                  , 0
                                  )
                                ]
        it "changes color for shots that pass through target players" $ do
            toDrawableGame
                    (updateGame
                        []
                        200
                        (V2 177 600)
                        (updateGame
                            [ Event
                                  0
                                  (JoyButtonEvent
                                      (JoyButtonEventData 0 5 JoyButtonPressed)
                                  )
                            ]
                            100
                            (V2 177 600)
                            (createGame (V2 177 600))
                        )
                    )
                `shouldContain` [ ( "gen/shot-hit.bmp"
                                  , Just (Rectangle (P (V2 113 295)) (V2 11 11))
                                  , 0.0
                                  )
                                ]
        it
                ("changes color for shots that pass through target players even"
                ++ " though they haven't left the barrel of the player"
                ++ " triggering the shot"
                )
            $               do
                                toDrawableGame
                                    (updateGame
                                        []
                                        46
                                        (V2 128 600)
                                        (updateGame
                                            [ Event
                                                  0
                                                  (JoyButtonEvent
                                                      (JoyButtonEventData 0 5 JoyButtonPressed
                                                      )
                                                  )
                                            ]
                                            23
                                            (V2 128 600)
                                            (createGame (V2 128 600))
                                        )
                                    )
            `shouldContain` [ ( "gen/shot-hit.bmp"
                              , Just (Rectangle (P (V2 59 295)) (V2 11 11))
                              , 0.0
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
        it "can collide two players" $ do
            toDrawableGame
                    (updateGame
                        [Event 0 (JoyAxisEvent (JoyAxisEventData 0 0 20000))]
                        1000
                        (V2 200 100)
                        (createGame (V2 200 100))
                    )
                `shouldContain` [ ( "gen/player.bmp"
                                  , Just (Rectangle (P (V2 104 34)) (V2 32 32))
                                  , 0
                                  )
                                ]
        it "can collide a player with a pillar" $ do
            playerId     <- return 0
            player       <- return $ createPlayer (V2 100 300) 0 playerId
            pillarRadius <- return 48
            old <- return $ Game 0
                                 (Movables [] [PlayerWithBarrel player []])
                                 [Circle (V2 200 300) pillarRadius]
                                 False
            new <- return $ updateGame [(moveRight 0)] 1000 (V2 800 600) old
            getPlayerPosition (getFirstPlayer new)
                `shouldBe` (V2 (200 - pillarRadius - playerRadius) 300)
        it "removes a shot that has hit a pillar" $ do
            pillarRadius <- return 48
            old <- return $ Game 0
                                 (Movables [createShot (V2 200 300) 0] [])
                                 [Circle (V2 200 300) pillarRadius]
                                 False
            new <- return $ updateGame [] 100 (V2 800 600) old
            getShots new `shouldBe` []
