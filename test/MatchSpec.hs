module MatchSpec where

import           Test.Hspec
import           Match
import           Player
import           PlayerUtil
import           Circle
import           CircleUtil
import           Space
import           Shot
import           ShotUtil
import           SDL
import           SDL.Vect
import           SDL.Event
import           Foreign.C.Types
import           GHC.Int
import           MatchUtil

spec :: Spec
spec = do
    describe "createMatch"
        $ it "creates two players opposite of each other"
        $ let Match (Movables _ players) _ = createMatch (V2 800 600)
              angles = map (getPlayerAngle . toPlayer) players
              angle1 : angle2 : _ = angles
          in  abs (angleDifference2D angle1 angle2) `shouldBe` pi

    describe "drawMatch"
        $ it "converts from match to something that can be drawn by SDL"
        $ let player           = createPlayer (V2 48 350) 0 0
              playerWithBarrel = PlayerWithBarrel player []
              shot             = createShot (V2 100 200) 0
              pillar           = Circle (V2 60 70) 48
              match = Match (Movables [shot] [playerWithBarrel])
                            (Obstacles (createBounds 800 600) [pillar])
              -- just test the order, the tests for the individual draw
              -- functions test that the position and rotation is right
          in  map fst (drawMatch match)
                  `shouldBe` [ toTextureArea $ shotToCircle shot
                             , toTextureArea $ playerToCircle player
                             , toTextureArea pillar
                             ]

    describe "updateMatch" $ do
        it "can move a player"
            $ let
                  player = PlayerWithBarrel (createPlayer (V2 48 350) 0 0) []
                  old    = Match (Movables [] [player])
                                 (Obstacles (createBounds 800 600) [])
                  td        = 200
                  moveEvent = createMoveRightEvent 0 50 (fromIntegral td)
                  new       = updateMatch [moveEvent] td old
              in
                  getPlayerPosition (getFirstPlayer new) `shouldBe` (V2 98 350)
        it "can create a shot"
            $ let
                  position = V2 48 350
                  angle    = 45
                  player   = PlayerWithBarrel (createPlayer position angle 0) []
                  old      = Match (Movables [] [player])
                                   (Obstacles (createBounds 800 600) [])
                  triggerPressed = createTriggerEvent 0 JoyButtonPressed
                  new            = updateMatch [triggerPressed] 100 old
              in
                  getFirstBarrel new `shouldBe` [createShot position angle]
        it "cannot create 2 shots from the same player in rapid succession"
            $ let
                  player = PlayerWithBarrel (createPlayer (V2 48 350) 45 0) []
                  old    = Match (Movables [] [player])
                                 (Obstacles (createBounds 800 600) [])
                  triggerPressed = createTriggerEvent 0 JoyButtonPressed
                  between        = updateMatch [triggerPressed] 1 old
                  new            = updateMatch [triggerPressed] 1 between
              in
                  length (getFirstBarrel new) `shouldBe` 1
        it "can move a shot"
            $ let
                  shot = createShot (V2 400 300) (pi / 2)
                  old  = Match (Movables [shot] [])
                               (Obstacles (createBounds 800 600) [])
                  td        = getShotMovementTime 50
                  new       = updateMatch [] td old
                  -- expected y should be about 300 + 50 but calculate anyway
                  -- since time is not a float
                  expectedY = 300 + (fromIntegral td) * shotSpeed
              in
                  map getShotPosition (getShots new)
                      `shouldBe` [V2 400 $ expectedY]
        it "removes a shot if it is out of bounds"
            $ let bounds = createBounds 800 600
                  shot   = createShot (V2 750 300) 0
                  old    = Match (Movables [shot] []) (Obstacles bounds [])
                  td     = getShotMovementTime 100
              in  getShots (updateMatch [] td old) `shouldBe` []
        it "changes color of shots that hit players"
            $ let
                  shot   = createShot (V2 100 300) 0
                  player = PlayerWithBarrel (createPlayer (V2 200 300) 0 0) []
                  old    = Match (Movables [shot] [player])
                                 (Obstacles (createBounds 800 600) [])
                  new = updateMatch [] (getShotMovementTime 100) old
              in
                  map getShotState (getShots new) `shouldBe` [HasHitPlayer]
        it
                (  "changes color of shots that hit players even though they"
                ++ " haven't left the barrel of the player triggering the shot"
                )
            $ let
                  shot = createShot (V2 400 300) 0
                  sourcePlayer =
                      PlayerWithBarrel (createPlayer (V2 400 300) 0 0) [shot]
                  targetPlayer = PlayerWithBarrel
                      (createPlayer (V2 (400 + playerSide) 300) 0 0)
                      []
                  old = Match (Movables [] [sourcePlayer, targetPlayer])
                              (Obstacles (createBounds 800 600) [])
                  new = updateMatch [] (getShotMovementTime playerRadius) old
              in
                  map getShotState (getFirstBarrel new)
                      `shouldBe` [HasHitPlayer]
        it "can collide two players"
            $ let
                  player1 = PlayerWithBarrel (createPlayer (V2 100 300) 0 0) []
                  player2 = PlayerWithBarrel (createPlayer (V2 200 300) 0 1) []
                  old     = Match (Movables [] [player1, player2])
                                  (Obstacles (createBounds 800 600) [])
                  td = 1000
                  movePlayer1Right =
                      createMoveRightEvent 0 200 (fromIntegral td)
                  new = updateMatch [movePlayer1Right] td old
              in
                  getPlayerPosition (getFirstPlayer new)
                      `shouldBe` (V2 (200 - playerSide) 300)
        it "can collide a player with a pillar"
            $ let
                  player       = createPlayer (V2 100 300) 0 0
                  pillarRadius = 48
                  pillar       = Circle (V2 200 300) pillarRadius
                  old = Match (Movables [] [PlayerWithBarrel player []])
                              (Obstacles (createBounds 800 600) [pillar])
                  new = updateMatch [(createMoveRightEvent 0 200 1000)]
                                    1000
                                    old
              in
                  getPlayerPosition (getFirstPlayer new)
                      `shouldBe` (V2 (200 - pillarRadius - playerRadius) 300)
        it "removes a shot that has hit a pillar"
            $ let pillar = Circle (V2 200 300) 48
                  old    = Match
                      (Movables [createShot (V2 200 300) 0] [])
                                -- use wide bounds to make sure the shot is not
                                -- removed because it is outside the bounds
                      (Obstacles (createBounds 100000 600) [pillar])
              in  getShots (updateMatch [] 1000 old) `shouldBe` []
