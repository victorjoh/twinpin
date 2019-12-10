module GameSpec where

import           Test.Hspec
import           Game
import           Player
import           PlayerUtil
import           Circle
import           CircleUtil
import           Space
import           Shot
import           ShotUtil
import           SDL.Vect
import           SDL.Event
import           SDL.Internal.Types             ( Window(..) )
import           Foreign.Ptr                    ( nullPtr )
import           Foreign.C.Types
import           Data.Tuple.Extra               ( fst3 )
import           GHC.Int
import           Data.Word                      ( Word32 )

-- returns how much time is needed for a shot to travel a certain distance
getShotMovementTime :: Vector1D -> Word32
getShotMovementTime distance = round $ distance / shotSpeed
        -- actualDistance = time * shotSpeed in (time, actualDistance)

getFirstPlayerWithBarrel (Game _ (Movables _ (playerWithBarrel : _)) _ _) =
    playerWithBarrel

getFirstPlayer :: Game -> Player
getFirstPlayer game =
    let PlayerWithBarrel player _ = getFirstPlayerWithBarrel game in player

getFirstBarrel :: Game -> [Shot]
getFirstBarrel game =
    let PlayerWithBarrel _ barrel = getFirstPlayerWithBarrel game in barrel

getShots :: Game -> [Shot]
getShots (Game _ (Movables shots _) _ _) = shots

getShotTexture :: Shot -> FilePath
getShotTexture (Shot _ _ texture) = texture

spec :: Spec
spec = do
    describe "gameTextureFiles"
        $ it "retreives all the paths to texture files in the game"
        $ gameTextureFiles
        `shouldMatchList` [ "gen/shot.bmp"
                          , "gen/shot-hit.bmp"
                          , "gen/player.bmp"
                          , "gen/pillar.bmp"
                          ]

    describe "toDrawableGame"
        $ it "converts from game to something that can be drawn by SDL"
        $ let player = PlayerWithBarrel (createPlayer (V2 48 350) 0 0) []
              shot   = createShot (V2 100 200) 0
              pillar = Circle (V2 60 70) 48
              game   = Game 0
                            (Movables [shot] [player])
                            (Obstacles (createBounds 800 600) [pillar])
                            Running
              -- just test the order, the tests for the individual draw
              -- functions test that the position and rotation is right
          in  map fst3 (toDrawableGame game)
                  `shouldBe` [ shotDefaultTextureFile
                             , playerTextureFile
                             , pillarTextureFile
                             ]

    describe "updateGame" $ do
        it "can move a player"
            $ let player = PlayerWithBarrel (createPlayer (V2 48 350) 0 0) []
                  old    = Game 0
                                (Movables [] [player])
                                (Obstacles (createBounds 800 600) [])
                                Running
                  newTime   = 200
                  moveEvent = createMoveRightEvent 0 50 newTime
                  new       = updateGame [moveEvent] newTime old
              in  getPlayerPosition (getFirstPlayer new) `shouldBe` (V2 98 350)
        it "can create a shot"
            $ let
                  position = V2 48 350
                  angle    = 45
                  player   = PlayerWithBarrel (createPlayer position angle 0) []
                  old      = Game 0
                                  (Movables [] [player])
                                  (Obstacles (createBounds 800 600) [])
                                  Running
                  new = updateGame [createTriggerEvent 0] 100 old
              in
                  getFirstBarrel new `shouldBe` [createShot position angle]
        it "can move a shot"
            $ let
                  shot = createShot (V2 400 300) 90
                  old  = Game 0
                              (Movables [shot] [])
                              (Obstacles (createBounds 800 600) [])
                              Running
                  newTime   = getShotMovementTime 50
                  new       = updateGame [] (getShotMovementTime 50) old
                  -- expected y should be about 300 + 50 but calculate anyway
                  -- since time is not a float
                  expectedY = 300 + (fromIntegral newTime) * shotSpeed
              in
                  map getShotPosition (getShots new)
                      `shouldBe` [V2 400 $ expectedY]
        it "removes a shot if it is out of bounds"
            $ let
                  bounds = createBounds 800 600
                  shot   = createShot (V2 750 300) 0
                  old =
                      Game 0 (Movables [shot] []) (Obstacles bounds []) Running
                  newTime = getShotMovementTime 100
              in
                  getShots (updateGame [] newTime old) `shouldBe` []
        it "changes color of shots that hit players"
            $ let shot   = createShot (V2 100 300) 0
                  player = PlayerWithBarrel (createPlayer (V2 200 300) 0 0) []
                  old    = Game 0
                                (Movables [shot] [player])
                                (Obstacles (createBounds 800 600) [])
                                Running
                  new = updateGame [] (getShotMovementTime 100) old
              in  map getShotTexture (getShots new)
                      `shouldBe` [shotHitTextureFile]
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
                  old = Game 0
                             (Movables [] [sourcePlayer, targetPlayer])
                             (Obstacles (createBounds 800 600) [])
                             Running
                  new = updateGame [] (getShotMovementTime playerRadius) old
              in
                  map getShotTexture (getFirstBarrel new)
                      `shouldBe` [shotHitTextureFile]
        it "is not finished when the user has not closed the window"
            $ not
            $ isFinished
            $ updateGame [] 50 (createGame (V2 800 600))
        it "is finished when the user closes the window"
            $ let closedEvent = Event
                      0
                      (WindowClosedEvent
                          (WindowClosedEventData (Window nullPtr))
                      )
              in  isFinished $ updateGame [closedEvent] 1 $ createGame $ V2 5 5
        it "can collide two players"
            $ let player1 = PlayerWithBarrel (createPlayer (V2 100 300) 0 0) []
                  player2 = PlayerWithBarrel (createPlayer (V2 200 300) 0 1) []
                  old     = Game 0
                                 (Movables [] [player1, player2])
                                 (Obstacles (createBounds 800 600) [])
                                 Running
                  newTime          = 1000
                  movePlayer1Right = createMoveRightEvent 0 200 newTime
                  new              = updateGame [movePlayer1Right] newTime old
              in  getPlayerPosition (getFirstPlayer new)
                      `shouldBe` (V2 (200 - playerSide) 300)
        it "can collide a player with a pillar"
            $ let player       = createPlayer (V2 100 300) 0 0
                  pillarRadius = 48
                  pillar       = Circle (V2 200 300) pillarRadius
                  old = Game 0
                             (Movables [] [PlayerWithBarrel player []])
                             (Obstacles (createBounds 800 600) [pillar])
                             Running
                  new = updateGame [(createMoveRightEvent 0 200 1000)] 1000 old
              in  getPlayerPosition (getFirstPlayer new)
                      `shouldBe` (V2 (200 - pillarRadius - playerRadius) 300)
        it "removes a shot that has hit a pillar"
            $ let pillar = Circle (V2 200 300) 48
                  old    = Game 0
                                (Movables [createShot (V2 200 300) 0] [])
                                -- use wide bounds to make sure the shot is not
                                -- removed because it is outside the bounds
                                (Obstacles (createBounds 100000 600) [pillar])
                                Running
              in  getShots (updateGame [] 1000 old) `shouldBe` []
