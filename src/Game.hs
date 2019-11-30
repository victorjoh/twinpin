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
import           Data.Zip                       ( fsts )
import           Data.List                      ( delete
                                                , partition
                                                )
import           Data.Bifunctor                 ( first )

data Game = Game Time ([Shot], [(Player, Barrel)]) Bool deriving Show

-- Shots that still haven't left the players barrel after firing. These are
-- separated from other shots to make sure that the player isn't immediately hit
-- by his own shot after firing.
type Barrel = [Shot]

gameTextureFiles :: [FilePath]
gameTextureFiles = playerTextureFile : shotTextureFiles

createGame :: V2 CInt -> Game
createGame (V2 bx by) = Game
    0
    ( []
    , [ ((createPlayer (V2 xDistanceFromEdge yMiddle) 0 0), [])
      , ( (createPlayer (V2 (fromIntegral bx - xDistanceFromEdge) yMiddle) 180 1
          )
        , []
        )
      ]
    )
    False
  where
    yMiddle           = (fromIntegral by / 2)
    xDistanceFromEdge = playerSide + playerSide / 2

toDrawableGame :: Game -> [(FilePath, Maybe (Rectangle CInt), CDouble)]
toDrawableGame (Game _ (shots, playersWithBarrels) _) =
    map (toDrawablePlayer . fst) playersWithBarrels
        ++ map toDrawableShot (allShots playersWithBarrels shots)

allShots :: [(Player, Barrel)] -> [Shot] -> [Shot]
allShots playersWithBarrels shots = shots ++ concatMap snd playersWithBarrels

updateGame :: [Event] -> Word32 -> V2 CInt -> Game -> Game
updateGame events newWordTime (V2 bx by) (Game time movingObjects isFinished) =
    Game
        newTime
        ( filterOutOfBounds bounds
        $ exitBarrels
        $ registerHits
        $ updateShots passedTime
        $ triggerShots events
        $ updatePlayers events passedTime bounds movingObjects
        )
        (isFinished || any isClosedEvent events)
  where
    newTime    = fromIntegral newWordTime
    passedTime = newTime - time
    bounds     = Bounds2D (0, fromIntegral bx) (0, fromIntegral by)

filterOutOfBounds
    :: Bounds2D -> ([Shot], [(Player, Barrel)]) -> ([Shot], [(Player, Barrel)])
filterOutOfBounds bounds (shots, playersWithBarrels) =
    (filter (isShotWithinBounds bounds) shots, playersWithBarrels)

exitBarrels :: ([Shot], [(Player, Barrel)]) -> ([Shot], [(Player, Barrel)])
exitBarrels (shots, playersWithBarrels) =
    foldl exitBarrel (shots, []) playersWithBarrels

exitBarrel
    :: ([Shot], [(Player, Barrel)])
    -> (Player, Barrel)
    -> ([Shot], [(Player, Barrel)])
exitBarrel (shots, playersWithBarrels) (player, barrel) =
    (shots ++ outsideBarrel, (player, insideBarrel) : playersWithBarrels)
  where
    (outsideBarrel, insideBarrel) = partition
        ((areIntersecting $ playerToCircle player) . shotToCircle)
        barrel

registerHits :: ([Shot], [(Player, Barrel)]) -> ([Shot], [(Player, Barrel)])
registerHits (shots, playersWithBarrels) =
    ( map (registerHit players) shots
    , [ (player, map (registerHit $ delete player players) barrel)
      | (player, barrel) <- playersWithBarrels
      ]
    )
    where players = fsts playersWithBarrels

registerHit :: [Player] -> Shot -> Shot
registerHit players shot
    | any ((areIntersecting $ shotToCircle shot) . playerToCircle) players
    = setShotHit shot
    | otherwise
    = shot

updateShots
    :: DeltaTime -> ([Shot], [(Player, Barrel)]) -> ([Shot], [(Player, Barrel)])
updateShots dt (shots, playersWithBarrels) =
    ( map (updateShot dt) shots
    , mapSecond (map (updateShot dt)) playersWithBarrels
    )

triggerShots
    :: [Event] -> ([Shot], [(Player, Barrel)]) -> ([Shot], [(Player, Barrel)])
triggerShots events (shots, playersWithBarrels) =
    ( shots
    , [ (player, barrel ++ maybeToList (triggerShot events player))
      | (player, barrel) <- playersWithBarrels
      ]
    )

updatePlayers
    :: [Event]
    -> DeltaTime
    -> Bounds2D
    -> ([Shot], [(Player, Barrel)])
    -> ([Shot], [(Player, Barrel)])
updatePlayers events dt bounds (shots, player : playersWithBarrels) =
    (shots, updatePlayers' events dt bounds [] player playersWithBarrels)

updatePlayers'
    :: [Event]
    -> DeltaTime
    -> Bounds2D
    -> [(Player, Barrel)]
    -> (Player, Barrel)
    -> [(Player, Barrel)]
    -> [(Player, Barrel)]
updatePlayers' events dt bounds updated toBeUpdated [] =
    updated
        ++ [ (first
                 (updatePlayer
                     events
                     dt
                     (Obstacles bounds (map playerToCircle $ fsts updated))
                 )
                 toBeUpdated
             )
           ]
updatePlayers' events dt bounds updated toBeUpdated (next : notUpdated) =
    updatePlayers'
        events
        dt
        bounds
        (  updated
        ++ [ (first
                 (updatePlayer
                     events
                     dt
                     (Obstacles
                         bounds
                         (map playerToCircle
                              (fsts updated ++ fsts (next : notUpdated))
                         )
                     )
                 )
                 toBeUpdated
             )
           ]
        )
        next
        notUpdated

mapSecond :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSecond f xys = [ (x, f y) | (x, y) <- xys ]

isClosedEvent :: Event -> Bool
isClosedEvent (Event _ (WindowClosedEvent _)) = True
isClosedEvent _                               = False

isFinished :: Game -> Bool
isFinished (Game _ _ isFinished) = isFinished
