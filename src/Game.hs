module Game where

import           Player
import           Shot
import           Space
import           Circle
import           SDL
import           Data.Word                      ( Word8
                                                , Word32
                                                )
import           Foreign.C.Types
import           Data.Maybe                     ( mapMaybe
                                                , maybeToList
                                                )
import           Data.List                      ( delete
                                                , partition
                                                )

-- The barrel contains shots that still haven't left the player after firing.
-- These are separated from other shots to make sure that the player isn't
-- immediately hit by his own shot after firing.
data PlayerWithBarrel = PlayerWithBarrel Player [Shot] deriving Show
data Movables = Movables [Shot] [PlayerWithBarrel] deriving Show
type Pillar = Circle
type IsFinished = Bool
data Game = Game Time Movables [Pillar] IsFinished deriving Show

createPillars :: Float -> Float -> [Pillar]
createPillars boundsWidth boundsHeight =
    let pillarRadius     = 48
        distanceFromEdge = playerSide * 3 + pillarRadius
    in  [ Circle (V2 x y) pillarRadius
        | x <- [distanceFromEdge, boundsWidth - distanceFromEdge]
        , y <- [distanceFromEdge, boundsHeight - distanceFromEdge]
        ]

pillarTextureFile :: FilePath
pillarTextureFile = "gen/pillar.bmp"

gameTextureFiles :: [FilePath]
gameTextureFiles = pillarTextureFile : playerTextureFile : shotTextureFiles

createGame :: V2 CInt -> Game
createGame bounds = Game
    0
    (Movables
        []
        [ PlayerWithBarrel (createPlayer (V2 xDistanceFromEdge yMiddle) 0 0) []
        , PlayerWithBarrel
            (createPlayer (V2 (boundsWidth - xDistanceFromEdge) yMiddle) 180 1)
            []
        ]
    )
    (createPillars boundsWidth boundsHeight)
    False
  where
    V2 boundsWidth boundsHeight = fromIntegral <$> bounds
    yMiddle                     = boundsHeight / 2
    xDistanceFromEdge           = playerSide + playerSide / 2

toDrawableGame :: Game -> [(FilePath, Maybe (Rectangle CInt), CDouble)]
toDrawableGame (Game _ movables pillars _) =
    let Movables _ playersWithBarrels = movables
    in  map toDrawableShot (getAllShots movables)
            ++ map (toDrawablePlayer . getPlayer) playersWithBarrels
            ++ map (\c -> toDrawableCircle c 0 pillarTextureFile) pillars

getPlayer :: PlayerWithBarrel -> Player
getPlayer (PlayerWithBarrel player _) = player

getBarrel :: PlayerWithBarrel -> [Shot]
getBarrel (PlayerWithBarrel _ barrel) = barrel

mapPlayer :: (Player -> Player) -> PlayerWithBarrel -> PlayerWithBarrel
mapPlayer f (PlayerWithBarrel player barrel) =
    PlayerWithBarrel (f player) barrel

mapBarrel :: ([Shot] -> [Shot]) -> PlayerWithBarrel -> PlayerWithBarrel
mapBarrel f (PlayerWithBarrel player barrel) =
    PlayerWithBarrel player $ f barrel

getAllShots :: Movables -> [Shot]
getAllShots (Movables shots playersWithBarrels) =
    shots ++ concatMap getBarrel playersWithBarrels

mapShots :: ([Shot] -> [Shot]) -> Movables -> Movables
mapShots f (Movables shots playersWithBarrels) =
    Movables (f shots) playersWithBarrels

updateGame :: [Event] -> Word32 -> V2 CInt -> Game -> Game
updateGame events newWordTime (V2 bx by) game = Game
    newTime
    ( removeOutOfBounds bounds
    $ exitBarrels
    $ registerHits
    $ updateShots passedTime
    $ triggerShots events
    $ updatePlayers events passedTime bounds pillars
    $ removePillarHits pillars movables
    )
    pillars
    (isFinished || any isClosedEvent events)
  where
    Game time movables pillars isFinished = game
    newTime    = fromIntegral newWordTime
    passedTime = newTime - time
    bounds     = Bounds2D (0, fromIntegral bx) (0, fromIntegral by)

removePillarHits :: [Pillar] -> Movables -> Movables
removePillarHits pillars = mapShots $ filter $ \shot ->
    not $ any (areIntersecting (shotToCircle shot)) pillars

removeOutOfBounds :: Bounds2D -> Movables -> Movables
removeOutOfBounds bounds = mapShots (filter (isShotWithinBounds bounds))

exitBarrels :: Movables -> Movables
exitBarrels (Movables shots playersWithBarrels) =
    foldl exitBarrel (Movables shots []) playersWithBarrels

exitBarrel :: Movables -> PlayerWithBarrel -> Movables
exitBarrel (Movables shots playersWithBarrels) (PlayerWithBarrel player barrel)
    = Movables
        (shots ++ outsideBarrel)
        ((PlayerWithBarrel player insideBarrel) : playersWithBarrels)
  where
    (outsideBarrel, insideBarrel) = partition
        ((areIntersecting $ playerToCircle player) . shotToCircle)
        barrel

registerHits :: Movables -> Movables
registerHits (Movables shots playersWithBarrels) = Movables
    (map (registerHit players) shots)
    [ (PlayerWithBarrel player
                        (map (registerHit $ delete player players) barrel)
      )
    | (PlayerWithBarrel player barrel) <- playersWithBarrels
    ]
    where players = map getPlayer playersWithBarrels

registerHit :: [Player] -> Shot -> Shot
registerHit players shot
    | any ((areIntersecting $ shotToCircle shot) . playerToCircle) players
    = setShotHit shot
    | otherwise
    = shot

updateShots :: DeltaTime -> Movables -> Movables
updateShots dt (Movables shots playersWithBarrels) = Movables
    (map (updateShot dt) shots)
    (map (mapBarrel (map (updateShot dt))) playersWithBarrels)

triggerShots :: [Event] -> Movables -> Movables
triggerShots events (Movables shots playersWithBarrels) = Movables shots $ map
    (\(PlayerWithBarrel player barrel) ->
        (PlayerWithBarrel
            player
            (barrel ++ maybeToList (triggerShot events player))
        )
    )
    playersWithBarrels

updatePlayers
    :: [Event] -> DeltaTime -> Bounds2D -> [Pillar] -> Movables -> Movables
updatePlayers _ _ _ _ (Movables shots []) = (Movables shots [])
updatePlayers events dt bounds pillars movables =
    let Movables shots (player : playersWithBarrels) = movables
    in  Movables shots $ updatePlayers' events
                                        dt
                                        bounds
                                        pillars
                                        []
                                        player
                                        playersWithBarrels

updatePlayers'
    :: [Event]
    -> DeltaTime
    -> Bounds2D
    -> [Pillar]
    -> [PlayerWithBarrel]
    -> PlayerWithBarrel
    -> [PlayerWithBarrel]
    -> [PlayerWithBarrel]
updatePlayers' events dt bounds pillars updated toBeUpdated [] =
    (mapPlayer (updatePlayer events dt (toObstacles bounds pillars updated))
               toBeUpdated
        )
        : updated
updatePlayers' events dt bounds pillars updated toBeUpdated (next : notUpdated)
    = updatePlayers'
        events
        dt
        bounds
        pillars
        ( (mapPlayer
              (updatePlayer events dt (toObstacles bounds pillars otherPlayers))
              toBeUpdated
          )
        : updated
        )
        next
        notUpdated
    where otherPlayers = updated ++ (next : notUpdated)

toObstacles :: Bounds2D -> [Pillar] -> [PlayerWithBarrel] -> Obstacles
toObstacles bounds pillars playersWithBarrels =
    Obstacles bounds
        $  pillars
        ++ map (playerToCircle . getPlayer) playersWithBarrels

isClosedEvent :: Event -> Bool
isClosedEvent (Event _ (WindowClosedEvent _)) = True
isClosedEvent _                               = False

isFinished :: Game -> Bool
isFinished (Game _ _ _ finished) = finished
