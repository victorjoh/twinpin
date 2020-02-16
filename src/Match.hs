{-# LANGUAGE TupleSections #-}

module Match
    ( Match(..)
    , Movables(..)
    , IntersectedPlayer(..)
    , pillarColor
    , createMatch
    , drawPillar
    , matchSize
    , staticMatchImages
    , drawMatch
    , updateMatch
    , assignJoysticksToMatch
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
import           Data.List                      ( delete )
import           SDL.Raw.Types                  ( JoystickID )
import           Data.Bifunctor                 ( second )

-- We keep track of shots that intersect with a player so that they won't be hit
-- twice by shots from other player, or hit by their own shot immediately after
-- firing.
data IntersectedPlayer = IntersectedPlayer [ShotId] Player deriving (Show, Eq)
data Movables = Movables ShotId [Shot] [IntersectedPlayer] deriving (Show, Eq)
type Pillar = Circle

data Match = Match Movables Obstacles deriving (Show, Eq)

pillarColor :: PixelRGBA8
pillarColor = PixelRGBA8 0x48 0x2D 0x3B 255

pillarRadius = 86
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

width = 1920
height = 1080

createMatch :: Match
createMatch =
    let
        bounds            = createBounds width height
        xDistanceFromEdge = playerSide + playerSide / 2
        createPlayer' x dir playerId = IntersectedPlayer
            []
            (createPlayer (V2 x (height / 2)) dir playerId Nothing)
    in
        Match
            (Movables
                0
                []
                [ createPlayer' xDistanceFromEdge           0  Red
                , createPlayer' (width - xDistanceFromEdge) pi Blue
                ]
            )
            (Obstacles bounds $ createPillars width height)

staticMatchImages :: [(ImageId, VectorImage)]
staticMatchImages =
    (pillarImageId, toSolidCircleImage pillarColor pillarRadius)
        : staticPlayerImage
        : staticShotImages

drawMatch :: Match -> [(Rectangle Float, Either VectorImage ImageId)]
drawMatch (Match (Movables _ shots intersectedPlayers) (Obstacles _ pillars)) =
    map drawShot shots
        ++ concatMap (drawPlayer . getPlayer) intersectedPlayers
        ++ map drawPillar pillars

drawPillar :: Pillar -> (Rectangle Float, Either VectorImage ImageId)
drawPillar = (, Right pillarImageId) . toTextureArea

getPlayer :: IntersectedPlayer -> Player
getPlayer (IntersectedPlayer _ player) = player

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
                    & moveShots passedTime
                    & triggerShots events
                    & updateIntersectingStatus
                    & removeOutOfBounds bounds
    in  Match newMovables obstacles

removePillarHits :: [Pillar] -> Movables -> Movables
removePillarHits pillars = mapShots $ filter $ \shot ->
    not $ any (areIntersecting (shotToCircle shot)) pillars

updatePlayers :: [Event] -> DeltaTime -> Obstacles -> Movables -> Movables
updatePlayers events dt obstacles (Movables nextShotId shots intersectedPlayers)
    = Movables nextShotId shots $ godFoldr
        ( concatApply
        $ (mapPlayer . updatePlayer events dt)
        . addToObstacles obstacles
        )
        intersectedPlayers

-- Fold a list from the right with a function that depends on all folded values
-- and all non-folded values when folding a single value.
godFoldr :: ([a] -> a -> [b] -> b) -> [a] -> [b]
godFoldr f l = godFoldr' f (reverse l) []

godFoldr' :: ([a] -> a -> [b] -> b) -> [a] -> [b] -> [b]
godFoldr' _ [] changed = changed
godFoldr' f (next : remaining) changed =
    godFoldr' f remaining (f remaining next changed : changed)

concatApply :: ([a] -> a -> a) -> [a] -> a -> [a] -> a
concatApply f remaining next changed = f (remaining ++ changed) next

addToObstacles :: Obstacles -> [IntersectedPlayer] -> Obstacles
addToObstacles (Obstacles bounds pillars) intersectedPlayers =
    Obstacles bounds
        $  pillars
        ++ map (playerToCircle . getPlayer) intersectedPlayers

moveShots :: DeltaTime -> Movables -> Movables
moveShots dt (Movables nextShotId shots intersectedPlayers) =
    Movables nextShotId (map (updateShot dt) shots) intersectedPlayers

triggerShots :: [Event] -> Movables -> Movables
triggerShots events (Movables nextShotId shots intersectedPlayers) = foldr
    (triggerShot' events)
    (Movables nextShotId shots [])
    intersectedPlayers

triggerShot' :: [Event] -> IntersectedPlayer -> Movables -> Movables
triggerShot' events toBeUpdated updated =
    let
        Movables nextShotId updatedShots updatedPlayers = updated
        IntersectedPlayer passingThrough player = toBeUpdated
        (newPlayer, maybeShot) = triggerShot events nextShotId player
        ifJust maybeSomething thenDo = maybe id (const thenDo) maybeSomething
        ifShot            = ifJust maybeShot
        newPassingThrough = ifShot (nextShotId :) passingThrough
        newNextShotId     = ifShot (+ 1) nextShotId
        updatedIntersectedPlayer =
            IntersectedPlayer newPassingThrough newPlayer
    in
        Movables newNextShotId
                 (maybe id (:) maybeShot updatedShots)
                 (updatedIntersectedPlayer : updatedPlayers)

updateIntersectingStatus :: Movables -> Movables
updateIntersectingStatus (Movables nextShotId shots players) =
    foldr updateIntersectingStatus' (Movables nextShotId [] players) shots

updateIntersectingStatus' :: Shot -> Movables -> Movables
updateIntersectingStatus' shot (Movables nextShotId registeredShots players) =
    let (newShot, registeredPlayers) = foldr registerHit (shot, []) players
    in  Movables nextShotId (newShot : registeredShots) registeredPlayers

registerHit
    :: IntersectedPlayer
    -> (Shot, [IntersectedPlayer])
    -> (Shot, [IntersectedPlayer])
registerHit piercedPlayer (shot, registered) =
    let
        IntersectedPlayer piercingBullets player = piercedPlayer
        shotId          = getShotId shot
        alreadyPiercing = elem shotId piercingBullets
        intersecting =
            areIntersecting (shotToCircle shot) (playerToCircle player)
    in
        second (: registered) $ if not alreadyPiercing && intersecting
            then
                ( setShotHit shot
                , IntersectedPlayer (shotId : piercingBullets)
                                    (inflictDamage shotDamage player)
                )
            else if alreadyPiercing && not intersecting
                then (shot, mapIntersecting (delete shotId) piercedPlayer)
                else (shot, piercedPlayer)

removeOutOfBounds :: Bounds2D -> Movables -> Movables
removeOutOfBounds bounds = mapShots (filter (isShotWithinBounds bounds))

mapShots :: ([Shot] -> [Shot]) -> Movables -> Movables
mapShots f (Movables nextShotId shots intersectedPlayers) =
    Movables nextShotId (f shots) intersectedPlayers

mapPlayer :: (Player -> Player) -> IntersectedPlayer -> IntersectedPlayer
mapPlayer f (IntersectedPlayer intersecting player) =
    IntersectedPlayer intersecting (f player)

mapIntersecting
    :: ([ShotId] -> [ShotId]) -> IntersectedPlayer -> IntersectedPlayer
mapIntersecting f (IntersectedPlayer intersecting player) =
    IntersectedPlayer (f intersecting) player

assignJoysticksToMatch :: [JoystickID] -> Match -> Match
assignJoysticksToMatch newJoysticks (Match movables obstacles) =
    let Movables nextShotId shots players = movables
        newPlayers = assignJoysticksToPlayers newJoysticks players
    in  Match (Movables nextShotId shots newPlayers) obstacles

assignJoysticksToPlayers
    :: [JoystickID] -> [IntersectedPlayer] -> [IntersectedPlayer]
assignJoysticksToPlayers _        []       = []
assignJoysticksToPlayers []       ps       = ps
assignJoysticksToPlayers (j : js) (p : ps) = if hasJoystick (getPlayer p)
    then p : assignJoysticksToPlayers (j : js) ps
    else mapPlayer (setJoystickId j) p : assignJoysticksToPlayers js ps
