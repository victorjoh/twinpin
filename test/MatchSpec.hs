{-# LANGUAGE ImplicitParams #-}
module MatchSpec where

import           Test.Hspec
import           Match
import           Player
import           PlayerUtil
import           Circle
import           Space
import           Shot
import           ShotUtil
import           SDL
import           MatchUtil
import           Test.HUnit.Approx

spec :: Spec
spec = do
    describe "createMatch"
        $ it "creates two players opposite of each other"
        $ let Match (Movables 0 _ players) _ = createMatch
              angles = map (getPlayerAngle . toPlayer) players
              angle1 : angle2 : _ = angles
          in  abs (angleDifference2D angle1 angle2) `shouldBe` pi

    describe "drawMatch"
        $ it "converts from match to something that can be drawn by SDL"
        $ let player            = createPlayer (V2 48 350) 0 Red Nothing
              intersectedPlayer = IntersectedPlayer [] player
              shot              = createShot (V2 100 200) 0 0
              pillar            = Circle (V2 60 70) 48
              match = Match (Movables 1 [shot] [intersectedPlayer])
                            (Obstacles (createBounds 800 600) [pillar])
            -- just test the order, the tests for the individual draw
            -- functions test that the position and rotation is right
          in  map fst (drawMatch match)
                  `shouldBe` map
                                 fst
                                 (  [drawShot shot]
                                 ++ drawPlayer player
                                 ++ [drawPillar pillar]
                                 )

    describe "updateMatch" $ do
        let ?epsilon = 0.01
        it "can move a player"
            $ let
                  player = IntersectedPlayer []
                      $ createPlayer (V2 48 350) 0 Red (Just 0)
                  old = Match (Movables 0 [] [player])
                              (Obstacles (createBounds 800 600) [])
                  td        = 200
                  moveEvent = createMoveRightEvent 0 50 (fromIntegral td)
                  new       = updateMatch [moveEvent] td old
              in
                  getPlayerPosition (getFirstPlayer new) @?~ V2 98 350
        it "can create a shot"
            $ let
                  position  = V2 48 350
                  direction = 45
                  player    = IntersectedPlayer []
                      $ createPlayer position direction Red (Just 0)
                  old = Match (Movables 8 [] [player])
                              (Obstacles (createBounds 800 600) [])
                  triggerPressed = createTriggerEvent 0 JoyButtonPressed
                  new            = updateMatch [triggerPressed] 100 old
              in
                  getShots new `shouldBe` [createShot position direction 8]
        it "cannot create 2 shots from the same player in rapid succession"
            $ let
                  player = IntersectedPlayer []
                      $ createPlayer (V2 48 350) 45 Red (Just 0)
                  old = Match (Movables 0 [] [player])
                              (Obstacles (createBounds 800 600) [])
                  triggerPressed = createTriggerEvent 0 JoyButtonPressed
                  between        = updateMatch [triggerPressed] 1 old
                  new            = updateMatch [triggerPressed] 1 between
              in
                  length (getFirstIntersecting new) `shouldBe` 1
        it "can move a shot"
            $ let
                  shot = createShot (V2 400 300) (pi / 2) 0
                  old  = Match (Movables 1 [shot] [])
                               (Obstacles (createBounds 800 600) [])
                  td        = getShotMovementTime 50
                  new       = updateMatch [] td old
                  -- expected y should be about 300 + 50 but calculate anyway
                  -- since time is not a float
                  expectedY = 300 + fromIntegral td * shotSpeed
              in
                  map getShotPosition (getShots new)
                      `shouldBe` [V2 400 expectedY]
        it "removes a shot if it is out of bounds"
            $ let bounds = createBounds 800 600
                  shot   = createShot (V2 750 300) 0 0
                  old    = Match (Movables 1 [shot] []) (Obstacles bounds [])
                  td     = getShotMovementTime 100
              in  getShots (updateMatch [] td old) `shouldBe` []
        it "changes color of shots that hit players"
            $ let
                  shot   = createShot (V2 100 300) 0 0
                  player = IntersectedPlayer []
                      $ createPlayer (V2 200 300) 0 Red Nothing
                  old = Match (Movables 1 [shot] [player])
                              (Obstacles (createBounds 800 600) [])
                  new = updateMatch [] (getShotMovementTime 100) old
              in
                  map getShotState (getShots new) `shouldBe` [HasHitPlayer]
        it "decreases the health of a player that is hit by a shot"
            $ let
                  shot   = createShot (V2 100 300) 0 0
                  player = IntersectedPlayer []
                      $ createPlayer (V2 200 300) 0 Red Nothing
                  old = Match (Movables 1 [shot] [player])
                              (Obstacles (createBounds 800 600) [])
                  new = updateMatch [] (getShotMovementTime 100) old
              in
                  getHealth (getFirstPlayer new)
                      `shouldBe` (playerMaxHealth - shotDamage)
        it
                (  "changes color of shots that hit players even though they"
                ++ " haven't left the barrel of the player triggering the shot"
                )
            $ let
                  shot         = createShot (V2 400 300) 0 7
                  sourcePlayer = IntersectedPlayer [7]
                      $ createPlayer (V2 400 300) 0 Red Nothing
                  targetPlayer = IntersectedPlayer
                      []
                      (createPlayer (V2 (400 + playerSide) 300) 0 Blue Nothing)
                  old = Match
                      (Movables 8 [shot] [sourcePlayer, targetPlayer])
                      (Obstacles (createBounds 800 600) [])
                  new = updateMatch [] (getShotMovementTime playerRadius) old
              in
                  map getShotState (getShots new) `shouldBe` [HasHitPlayer]
        it
                ("decreases the health of a player that is hit by a shot that is"
                ++ " still in the barrel of another player"
                )
            $ let
                  shot         = createShot (V2 400 300) 0 7
                  sourcePlayer = IntersectedPlayer [7]
                      $ createPlayer (V2 400 300) 0 Red Nothing
                  targetPlayer = IntersectedPlayer
                      []
                      (createPlayer (V2 (400 + playerSide) 300) 0 Blue Nothing)
                  old = Match
                      (Movables 8 [shot] [targetPlayer, sourcePlayer])
                      (Obstacles (createBounds 800 600) [])
                  new = updateMatch [] (getShotMovementTime playerRadius) old
              in
                  getHealth (getFirstPlayer new)
                      `shouldBe` (playerMaxHealth - shotDamage)
        it "does not change the color of a shot that has not hit a player"
            $ let
                  shot   = createShot (V2 100 500) 0 0-- the shot will not hit
                  player = IntersectedPlayer []
                      $ createPlayer (V2 200 300) 0 Red Nothing
                  old = Match (Movables 1 [shot] [player])
                              (Obstacles (createBounds 800 600) [])
                  new = updateMatch [] (getShotMovementTime 100) old
              in
                  map getShotState (getShots new) `shouldBe` [HasNotHitPlayer]
        it "does not decrease the health of the player if the shot missed"
            $ let
                  shot   = createShot (V2 100 500) 0 0 -- the shot will not hit
                  player = IntersectedPlayer []
                      $ createPlayer (V2 200 300) 0 Red Nothing
                  old = Match (Movables 1 [shot] [player])
                              (Obstacles (createBounds 800 600) [])
                  new = updateMatch [] (getShotMovementTime 100) old
              in
                  getHealth (getFirstPlayer new) `shouldBe` playerMaxHealth
        it
                ("does not hit a player with their own shot if the shot has not "
                ++ "left the barrel"
                )
            $ let
                  player = IntersectedPlayer []
                      $ createPlayer (V2 400 300) 0 Red Nothing
                  old = Match (Movables 1 [] [player])
                              (Obstacles (createBounds 800 600) [])
                  triggerPressed = createTriggerEvent 0 JoyButtonPressed
                  new = updateMatch [] 1 $ updateMatch [triggerPressed] 1 old
              in
                  getHealth (getFirstPlayer new) `shouldBe` playerMaxHealth
        it
                ("does not hit a player more than once when a shot is passing "
                ++ "through them"
                )
            $ let
                  shot   = createShot (V2 100 300) 0 0
                  player = IntersectedPlayer []
                      $ createPlayer (V2 200 300) 0 Red Nothing
                  old = Match (Movables 1 [shot] [player])
                              (Obstacles (createBounds 800 600) [])
                  new = updateMatch [] 1
                      $ updateMatch [] (getShotMovementTime 100) old
              in
                  getHealth (getFirstPlayer new)
                      `shouldBe` (playerMaxHealth - shotDamage)
        it
                (  "if a shot passes through two players at the same time, "
                ++ "neither of them is hit more than once while the shot is "
                ++ "passing through them"
                )
            $ let
                  shot = createShot (V2 100 300) 0 0
                  players =
                      [ createPlayer (V2 200 (300 - playerRadius)) 0 Red Nothing
                      , createPlayer (V2 200 (300 + playerRadius)) 0 Red Nothing
                      ]
                  old = Match
                      (Movables 1 [shot] $ map (IntersectedPlayer []) players)
                      (Obstacles (createBounds 800 600) [])
                  new = updateMatch [] 1
                      $ updateMatch [] (getShotMovementTime 100) old
              in
                  map getHealth (getPlayers new)
                      `shouldBe` replicate 2 (playerMaxHealth - shotDamage)
        it "can collide two players"
            $ let
                  player1 = createPlayer (V2 100 300) 0 Red (Just 0)
                  player2 = createPlayer (V2 200 300) 0 Blue Nothing
                  players = map (IntersectedPlayer []) [player1, player2]
                  old     = Match (Movables 0 [] players)
                                  (Obstacles (createBounds 800 600) [])
                  td = 1000
                  movePlayer1Right =
                      createMoveRightEvent 0 200 (fromIntegral td)
                  new = updateMatch [movePlayer1Right] td old
              in
                  getPlayerPosition (getFirstPlayer new)
                      `shouldBe` (V2 (200 - playerSide) 300)
        it "can collide a player with a pillar"
            $ let player       = createPlayer (V2 100 300) 0 Red (Just 0)
                  pillarRadius = 48
                  pillar       = Circle (V2 200 300) pillarRadius
                  old = Match (Movables 0 [] [IntersectedPlayer [] player])
                              (Obstacles (createBounds 800 600) [pillar])
                  new = updateMatch [createMoveRightEvent 0 200 1000] 1000 old
              in  getPlayerPosition (getFirstPlayer new)
                      `shouldBe` (V2 (200 - pillarRadius - playerRadius) 300)
        it "removes a shot that has hit a pillar"
            $ let pillar = Circle (V2 200 300) 48
                  old    = Match
                      (Movables 1 [createShot (V2 200 300) 0 0] [])
                                -- use wide bounds to make sure the shot is not
                                -- removed because it is outside the bounds
                      (Obstacles (createBounds 100000 600) [pillar])
              in  getShots (updateMatch [] 1000 old) `shouldBe` []

    describe "assignJoysticksToMatch" $ do
        let
            updateWith oldJoysticks addedJoysticks =
                let
                    players = map
                        (IntersectedPlayer [] . createPlayer (V2 0 0) 0 Red)
                        oldJoysticks
                    match = Match (Movables 0 [] players)
                                  (Obstacles (createBounds 0 0) [])
                in
                    map getJoystickId $ getPlayers $ assignJoysticksToMatch
                        addedJoysticks
                        match
        it "does nothing with an empty list"
            $          updateWith [Just 0, Nothing] []
            `shouldBe` [Just 0, Nothing]
        it "does nothing if all the player already has joysticks assigned"
            $          updateWith [Just 0, Just 1] [5]
            `shouldBe` [Just 0, Just 1]
        it "assigns a joystick to a player missing one"
            $          updateWith [Just 0, Nothing] [5]
            `shouldBe` [Just 0, Just 5]
        it "only assigns as many joysticks as provided"
            $          updateWith [Nothing, Nothing, Nothing] [3, 5]
            `shouldBe` [Just 3, Just 5, Nothing]
        it "only assigns to those that does not already have a joystick"
            $          updateWith [Just 1, Nothing, Just 2, Nothing] [3, 4]
            `shouldBe` [Just 1, Just 3, Just 2, Just 4]
