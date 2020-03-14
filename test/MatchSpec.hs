{-# LANGUAGE ImplicitParams #-}
module MatchSpec where

import           Test.Hspec
import           Match
import           Player
import           PlayerUtil
import           Circle
import           Space
import           Bullet
import           BulletUtil
import           SDL
import           MatchUtil
import           VisualUtil                     ( )
import           SpaceUtil                      ( )
import           Approx

spec :: Spec
spec = do
    describe "createMatch"
        $ it "creates two players opposite of each other"
        $ let Match (Movables 0 _ players) _ = createMatch
              angles = map (getPlayerAngle . toPlayer) players
              angle1 : angle2 : _ = angles
          in  abs (angleDifference2D angle1 angle2) `shouldBe` pi

    describe "drawMatch" $ do
        it "converts from match to something that can be drawn by SDL"
            $ let player            = createPlayer (V2 48 350) 0 Red Nothing
                  intersectedPlayer = IntersectedPlayer [] player
                  bullet            = createBullet (V2 100 200) 0 0
                  pillar            = Circle (V2 60 70) 48
                  match = Match (Movables 1 [bullet] [intersectedPlayer])
                                (Obstacles (createBounds 800 600) [pillar])
                  -- just test the order, the tests for the individual draw
                  -- functions test that the position is right
              in  map fst (drawMatch match) `shouldBe` map
                      fst
                      (  [drawBullet bullet]
                      ++ drawPlayer player
                      ++ [drawPillar pillar]
                      ++ drawScore (V2 60 70) player
                      )
        it "draws the score on the pillars"
            $ let
                  pillars =
                      [ Circle (V2 100 100) 48
                      , Circle (V2 800 100) 48
                      , Circle (V2 100 800) 48
                      ]
                  players =
                      [ IntersectedPlayer []
                            $ createPlayer (V2 400 400) 0 color Nothing
                      | color <- [Red, Blue, Red]
                      ]
                  match = Match
                      (Movables 0 [] players)
                      (Obstacles (createBounds 1920 1080) pillars)
              in
                  drawMatch match
                      `shouldContain` [ ( Rectangle (P $ V2 82.5 77.5)
                                                    (V2 35.0 45.0)
                                        , Right "Red5"
                                        )
                                      , ( Rectangle (P $ V2 782.5 77.5)
                                                    (V2 35.0 45.0)
                                        , Right "Blue5"
                                        )
                                      , ( Rectangle (P $ V2 82.5 777.5)
                                                    (V2 35.0 45.0)
                                        , Right "Red5"
                                        )
                                      ]

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
                  getPlayerPosition (getFirstPlayer new)
                      `shouldApproxBe` V2 98 350
        it "can create a bullet"
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
                  getBullets new `shouldBe` [createBullet position direction 8]
        it "cannot create 2 bullets from the same player in rapid succession"
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
        it "can move a bullet"
            $ let
                  bullet = createBullet (V2 400 300) (pi / 2) 0
                  old    = Match (Movables 1 [bullet] [])
                                 (Obstacles (createBounds 800 600) [])
                  td        = getBulletMovementTime 50
                  new       = updateMatch [] td old
                  -- expected y should be about 300 + 50 but calculate anyway
                  -- since time is not a float
                  expectedY = 300 + fromIntegral td * bulletSpeed
              in
                  map getBulletPosition (getBullets new)
                      `shouldBe` [V2 400 expectedY]
        it "removes a bullet if it is out of bounds"
            $ let bounds = createBounds 800 600
                  bullet = createBullet (V2 750 300) 0 0
                  old    = Match (Movables 1 [bullet] []) (Obstacles bounds [])
                  td     = getBulletMovementTime 100
              in  getBullets (updateMatch [] td old) `shouldBe` []
        it "changes color of bullets that hit players"
            $ let
                  bullet = createBullet (V2 100 300) 0 0
                  player = IntersectedPlayer []
                      $ createPlayer (V2 200 300) 0 Red Nothing
                  old = Match (Movables 1 [bullet] [player])
                              (Obstacles (createBounds 800 600) [])
                  new = updateMatch [] (getBulletMovementTime 100) old
              in
                  map getBulletState (getBullets new) `shouldBe` [HasHitPlayer]
        it "decreases the health of a player that is hit by a bullet"
            $ let
                  bullet = createBullet (V2 100 300) 0 0
                  player = IntersectedPlayer []
                      $ createPlayer (V2 200 300) 0 Red Nothing
                  old = Match (Movables 1 [bullet] [player])
                              (Obstacles (createBounds 800 600) [])
                  new = updateMatch [] (getBulletMovementTime 100) old
              in
                  getHealth (getFirstPlayer new)
                      `shouldBe` (playerMaxHealth - bulletDamage)
        it
                (  "changes color of bullets that hit players even though they "
                ++ "haven't left the barrel of the player triggering the bullet"
                )
            $ let
                  bullet       = createBullet (V2 400 300) 0 7
                  sourcePlayer = IntersectedPlayer [7]
                      $ createPlayer (V2 400 300) 0 Red Nothing
                  targetPlayer = IntersectedPlayer
                      []
                      (createPlayer (V2 (400 + playerSide) 300) 0 Blue Nothing)
                  old = Match
                      (Movables 8 [bullet] [sourcePlayer, targetPlayer])
                      (Obstacles (createBounds 800 600) [])
                  new = updateMatch [] (getBulletMovementTime playerRadius) old
              in
                  map getBulletState (getBullets new) `shouldBe` [HasHitPlayer]
        it
                (  "decreases the health of a player that is hit by a bullet "
                ++ "that is still in the barrel of another player"
                )
            $ let
                  bullet       = createBullet (V2 400 300) 0 7
                  sourcePlayer = IntersectedPlayer [7]
                      $ createPlayer (V2 400 300) 0 Red Nothing
                  targetPlayer = IntersectedPlayer
                      []
                      (createPlayer (V2 (400 + playerSide) 300) 0 Blue Nothing)
                  old = Match
                      (Movables 8 [bullet] [targetPlayer, sourcePlayer])
                      (Obstacles (createBounds 800 600) [])
                  new = updateMatch [] (getBulletMovementTime playerRadius) old
              in
                  getHealth (getFirstPlayer new)
                      `shouldBe` (playerMaxHealth - bulletDamage)
        it "does not change the color of a bullet that has not hit a player"
            $ let -- the bullet will not hit
                  bullet = createBullet (V2 100 500) 0 0
                  player = IntersectedPlayer []
                      $ createPlayer (V2 200 300) 0 Red Nothing
                  old = Match (Movables 1 [bullet] [player])
                              (Obstacles (createBounds 800 600) [])
                  new = updateMatch [] (getBulletMovementTime 100) old
              in
                  map getBulletState (getBullets new)
                      `shouldBe` [HasNotHitPlayer]
        it "does not decrease the health of the player if the bullet missed"
            $ let -- the bullet will not hit
                  bullet = createBullet (V2 100 500) 0 0
                  player = IntersectedPlayer []
                      $ createPlayer (V2 200 300) 0 Red Nothing
                  old = Match (Movables 1 [bullet] [player])
                              (Obstacles (createBounds 800 600) [])
                  new = updateMatch [] (getBulletMovementTime 100) old
              in
                  getHealth (getFirstPlayer new) `shouldBe` playerMaxHealth
        it
                (  "does not hit a player with their own bullet if the bullet "
                ++ "has not left the barrel"
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
                (  "does not hit a player more than once when a bullet is "
                ++ "passing through them"
                )
            $ let
                  bullet = createBullet (V2 100 300) 0 0
                  player = IntersectedPlayer []
                      $ createPlayer (V2 200 300) 0 Red Nothing
                  old = Match (Movables 1 [bullet] [player])
                              (Obstacles (createBounds 800 600) [])
                  new = updateMatch [] 1
                      $ updateMatch [] (getBulletMovementTime 100) old
              in
                  getHealth (getFirstPlayer new)
                      `shouldBe` (playerMaxHealth - bulletDamage)
        it
                (  "if a bullet passes through two players at the same time, "
                ++ "neither of them is hit more than once while the bullet is "
                ++ "passing through them"
                )
            $ let
                  bullet = createBullet (V2 100 300) 0 0
                  players =
                      [ createPlayer (V2 200 (300 - playerRadius)) 0 Red Nothing
                      , createPlayer (V2 200 (300 + playerRadius)) 0 Red Nothing
                      ]
                  old = Match
                      (Movables 1 [bullet] $ map (IntersectedPlayer []) players)
                      (Obstacles (createBounds 800 600) [])
                  new = updateMatch [] 1
                      $ updateMatch [] (getBulletMovementTime 100) old
              in
                  map getHealth (getPlayers new)
                      `shouldBe` replicate 2 (playerMaxHealth - bulletDamage)
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
        it "removes a bullet that has hit a pillar"
            $ let pillar = Circle (V2 200 300) 48
                  old    = Match
                      (Movables 1 [createBullet (V2 200 300) 0 0] [])
                                -- use wide bounds to make sure the bullet is not
                                -- removed because it is outside the bounds
                      (Obstacles (createBounds 100000 600) [pillar])
              in  getBullets (updateMatch [] 1000 old) `shouldBe` []

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

    describe "getWinners" $ do
        context "when there are two players" $ do
            context "when both players have reached zero lives" $ do
                let p1 = setDeaths playerLives
                        $ createPlayer (V2 0 0) 0 Blue Nothing
                    p2 = setDeaths playerLives
                        $ createPlayer (V2 0 0) 0 Red Nothing
                    match = Match
                        (Movables 0 [] (map (IntersectedPlayer []) [p1, p2]))
                        (Obstacles (createBounds 800 600) [])
                it "returns both players' colors"
                    $          getWinners match
                    `shouldBe` [Blue, Red]
            context "when one player have reached zero lives" $ do
                let p1 = setDeaths playerLives
                        $ createPlayer (V2 0 0) 0 Blue Nothing
                    p2    = createPlayer (V2 0 0) 0 Red Nothing
                    match = Match
                        (Movables 0 [] (map (IntersectedPlayer []) [p1, p2]))
                        (Obstacles (createBounds 800 600) [])
                it "returns the other player's color"
                    $          getWinners match
                    `shouldBe` [Red]
            context "when none of the players have reached zero lives" $ do
                let p1    = createPlayer (V2 0 0) 0 Blue Nothing
                    p2    = createPlayer (V2 0 0) 0 Red Nothing
                    match = Match
                        (Movables 0 [] (map (IntersectedPlayer []) [p1, p2]))
                        (Obstacles (createBounds 800 600) [])
                it "returns an empty list" $ getWinners match `shouldBe` []
        context "when there are three players"
            $ context "when one player have reached zero lives"
            $ do
                  let
                      p1 = setDeaths playerLives
                          $ createPlayer (V2 0 0) 0 Blue Nothing
                  context
                          (  "when the remaining players have different amount "
                          ++ "of lives"
                          )
                      $ do
                            let p2 = createPlayer (V2 0 0) 0 Red Nothing
                                p3 = setDeaths 1
                                    $ createPlayer (V2 0 0) 0 Blue Nothing
                                match = Match
                                    ( Movables 0 []
                                    $ map
                                          (IntersectedPlayer [])
                                          [p1, p2, p3]
                                    )
                                    (Obstacles (createBounds 800 600) [])
                            it "returns the player with the most lives"
                                $          getWinners match
                                `shouldBe` [Red]
                  context
                          (  "when the remaining players have the same amount "
                          ++ "of lives"
                          )
                      $ do
                            let p2    = createPlayer (V2 0 0) 0 Red Nothing
                                p3    = createPlayer (V2 0 0) 0 Red Nothing
                                match = Match
                                    ( Movables 0 []
                                    $ map
                                          (IntersectedPlayer [])
                                          [p1, p2, p3]
                                    )
                                    (Obstacles (createBounds 800 600) [])
                            it "returns the remaining players"
                                $          getWinners match
                                `shouldBe` [Red, Red]

    describe "setPlayerIds" $ do
        it "sets the players' IDs in consecutive order"
            $ let playerIds = [PlayerId Red (Just 0), PlayerId Red (Just 1)]
              in  getPlayerIds (setPlayerIds playerIds createMatch)
                      `shouldBe` playerIds
        it
                (  "only sets the IDs for the first players when the number of "
                ++ "IDs are less than the number of players"
                )
            $ let playerIds = [PlayerId Red (Just 0)]
              in  getPlayerIds (setPlayerIds playerIds createMatch)
                  `shouldBe` playerIds
                  ++         [PlayerId Red Nothing]
        it
                (  "only sets the first IDs provided when the number of IDs are"
                ++ " more than the number of players"
                )
            $ let playerIds =
                      [ PlayerId Red  (Just 0)
                      , PlayerId Red  (Just 1)
                      , PlayerId Blue (Just 2)
                      ]
              in  getPlayerIds (setPlayerIds playerIds createMatch)
                      `shouldBe` take 2 playerIds

    describe "getPlayerIds"
        $          it "gets the player IDs"
        $          getPlayerIds (assignJoysticksToMatch [0] createMatch)
        `shouldBe` [PlayerId Blue (Just 0), PlayerId Red Nothing]
