{-# LANGUAGE TupleSections #-}

module Match
    ( Match(..)
    , Movables(..)
    , PlayerWithBarrel(..)
    , pillarColor
    , createMatch
    , drawPillar
    , matchSize
    , staticMatchImages
    , drawMatch
    , updateMatch
    )
where

import           Player
import           Shot
import           Space
import           Circle
import           Visual
import           SDL                     hiding ( Paused )
import           Codec.Picture.Types
import           Data.Function                  ( (&) )
import           Data.Maybe                     ( maybeToList )
import           Data.List                      ( delete
                                                , partition
                                                , foldl'
                                                )

-- The barrel contains shots that still haven't left the player after firing.
-- These are separated from other shots to make sure that the player isn't
-- immediately hit by his own shot after firing.
data PlayerWithBarrel = PlayerWithBarrel Player [Shot] deriving (Show, Eq)
data Movables = Movables [Shot] [PlayerWithBarrel] deriving (Show, Eq)
type Pillar = Circle

data Match = Match Movables Obstacles deriving (Show, Eq)

pillarColor :: PixelRGBA8
pillarColor = PixelRGBA8 0x48 0x2D 0x3B 255

pillarRadius = 48
pillarImageId = "pillar"

createPillars :: Float -> Float -> [Pillar]
createPillars boundsWidth boundsHeight =
    let distanceFromEdge = playerSide * 3 + pillarRadius
    in  [ Circle (V2 x y) pillarRadius
        | x <- [distanceFromEdge, boundsWidth - distanceFromEdge]
        , y <- [distanceFromEdge, boundsHeight - distanceFromEdge]
        ]

matchSize :: Size2D
matchSize = V2 width height

width = 800
height = 600

createMatch :: Match
createMatch =
    let
        bounds            = createBounds width height
        xDistanceFromEdge = playerSide + playerSide / 2
        createPlayer' x direction playerId = PlayerWithBarrel
            (createPlayer (V2 x (height / 2)) direction playerId)
            []
    in
        Match
            (Movables
                []
                [ createPlayer' xDistanceFromEdge           0  0
                , createPlayer' (width - xDistanceFromEdge) pi 1
                ]
            )
            (Obstacles bounds $ createPillars width height)

staticMatchImages :: [(ImageId, VectorImage)]
staticMatchImages =
    (pillarImageId, toSolidCircleImage pillarColor pillarRadius)
        : staticPlayerImage
        : staticShotImages

drawMatch :: Match -> [(Rectangle Float, Either VectorImage ImageId)]
drawMatch (Match movables (Obstacles _ pillars)) =
    let Movables _ playersWithBarrels = movables
    in  map drawShot (getAllShots movables)
            ++ concatMap (drawPlayer . getPlayer) playersWithBarrels
            ++ map drawPillar pillars

drawPillar :: Pillar -> (Rectangle Float, Either VectorImage ImageId)
drawPillar = (, Right pillarImageId) . toTextureArea

getAllShots :: Movables -> [Shot]
getAllShots (Movables shots playersWithBarrels) =
    shots ++ concatMap getBarrel playersWithBarrels

getPlayer :: PlayerWithBarrel -> Player
getPlayer (PlayerWithBarrel player _) = player

getBarrel :: PlayerWithBarrel -> [Shot]
getBarrel (PlayerWithBarrel _ barrel) = barrel

updateMatch :: [Event] -> DeltaTime -> Match -> Match
updateMatch events passedTime oldMatch =
    let Match     oldMovables obstacles = oldMatch
        Obstacles bounds      pillars   = obstacles
        newMovables =
                oldMovables
                    -- We remove shots that hit pillars where the shot are in
                    -- their old state. This is because we want to be able to
                    -- draw the frame where the shot hits the pillar, i.e. the
                    -- frame produced from 'oldGame'.
                    & removePillarHits pillars
                    & updatePlayers events passedTime obstacles
                    & updateShots passedTime
                    & triggerShots events
                    & registerHits
                    & exitBarrels
                    & removeOutOfBounds bounds
    in  Match newMovables obstacles

removePillarHits :: [Pillar] -> Movables -> Movables
removePillarHits pillars = mapShots $ filter $ \shot ->
    not $ any (areIntersecting (shotToCircle shot)) pillars

updatePlayers :: [Event] -> DeltaTime -> Obstacles -> Movables -> Movables
updatePlayers _ _ _ (Movables shots []) = Movables shots []
updatePlayers events dt obstacles movables =
    let Movables shots (player : playersWithBarrels) = movables
    in  Movables shots $ updatePlayers' events
                                        dt
                                        obstacles
                                        []
                                        player
                                        playersWithBarrels

updatePlayers'
    :: [Event]
    -> DeltaTime
    -> Obstacles
    -> [PlayerWithBarrel]
    -> PlayerWithBarrel
    -> [PlayerWithBarrel]
    -> [PlayerWithBarrel]
updatePlayers' events dt obstacles updated toBeUpdated [] =
    mapPlayer (updatePlayer events dt (addToObstacles updated obstacles))
              toBeUpdated
        : updated
updatePlayers' events dt obstacles updated toBeUpdated (next : notUpdated) =
    updatePlayers'
        events
        dt
        obstacles
        ( mapPlayer
                (updatePlayer events dt (addToObstacles otherPlayers obstacles))
                toBeUpdated

        : updated
        )
        next
        notUpdated
    where otherPlayers = updated ++ (next : notUpdated)

addToObstacles :: [PlayerWithBarrel] -> Obstacles -> Obstacles
addToObstacles playersWithBarrels (Obstacles bounds pillars) =
    Obstacles bounds
        $  pillars
        ++ map (playerToCircle . getPlayer) playersWithBarrels

updateShots :: DeltaTime -> Movables -> Movables
updateShots dt (Movables shots playersWithBarrels) = Movables
    (map (updateShot dt) shots)
    (map (mapBarrel (map (updateShot dt))) playersWithBarrels)

triggerShots :: [Event] -> Movables -> Movables
triggerShots events (Movables shots playersWithBarrels) = Movables shots $ map
    (\(PlayerWithBarrel player barrel) ->
        let (newPlayer, maybeShot) = triggerShot events player
        in  PlayerWithBarrel newPlayer $ barrel ++ maybeToList maybeShot
    )
    playersWithBarrels

registerHits :: Movables -> Movables
registerHits (Movables shots playersWithBarrels) = Movables
    (map (registerHit players) shots)
    [ PlayerWithBarrel player (map (registerHit $ delete player players) barrel)
    | (PlayerWithBarrel player barrel) <- playersWithBarrels
    ]
    where players = map getPlayer playersWithBarrels

registerHit :: [Player] -> Shot -> Shot
registerHit players shot
    | any (areIntersecting (shotToCircle shot) . playerToCircle) players
    = setShotHit shot
    | otherwise
    = shot

exitBarrels :: Movables -> Movables
exitBarrels (Movables shots playersWithBarrels) =
    foldl' exitBarrel (Movables shots []) playersWithBarrels

exitBarrel :: Movables -> PlayerWithBarrel -> Movables
exitBarrel (Movables shots playersWithBarrels) (PlayerWithBarrel player barrel)
    = Movables (shots ++ outsideBarrel)
               (PlayerWithBarrel player insideBarrel : playersWithBarrels)
  where
    (insideBarrel, outsideBarrel) = partition
        (areIntersecting (playerToCircle player) . shotToCircle)
        barrel

removeOutOfBounds :: Bounds2D -> Movables -> Movables
removeOutOfBounds bounds = mapShots (filter (isShotWithinBounds bounds))

mapShots :: ([Shot] -> [Shot]) -> Movables -> Movables
mapShots f (Movables shots playersWithBarrels) =
    Movables (f shots) playersWithBarrels

mapPlayer :: (Player -> Player) -> PlayerWithBarrel -> PlayerWithBarrel
mapPlayer f (PlayerWithBarrel player barrel) =
    PlayerWithBarrel (f player) barrel

mapBarrel :: ([Shot] -> [Shot]) -> PlayerWithBarrel -> PlayerWithBarrel
mapBarrel f (PlayerWithBarrel player barrel) =
    PlayerWithBarrel player $ f barrel
