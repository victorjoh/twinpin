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
import           Bullet
import           Space
import           Circle
import           Visual
import           SDL                     hiding ( Paused )
import           Codec.Picture.Types
import           Data.Function                  ( (&) )
import           Data.List                      ( delete )
import           SDL.Raw.Types                  ( JoystickID )
import           Data.Bifunctor                 ( second )

-- We keep track of bullets that intersect with a player so that they won't be
-- hit twice by a single bullet from other player, or hit by their own bullet
-- immediately after firing.
data IntersectedPlayer = IntersectedPlayer [BulletId] Player deriving (Show, Eq)
data Movables = Movables BulletId [Bullet] [IntersectedPlayer]
                         deriving (Show, Eq)
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
        : staticBulletImages

drawMatch :: Match -> [(Rectangle Float, Either VectorImage ImageId)]
drawMatch (Match (Movables _ bullets intersectedPlayers) (Obstacles _ pillars))
    = map drawBullet bullets
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
                    -- We remove bullets that hit pillars when the bullet are in
                    -- their old state. This is because we want to be able to
                    -- draw the frame where the bullet hits the pillar, i.e. the
                    -- frame produced from 'oldGame'.
                    & removePillarHits pillars
                    & updatePlayers events passedTime obstacles
                    & moveBullets passedTime
                    & fireBullets events
                    & updateIntersectingStatus
                    & removeOutOfBounds bounds
    in  Match newMovables obstacles

removePillarHits :: [Pillar] -> Movables -> Movables
removePillarHits pillars = mapIntersected $ filter $ \bullet ->
    not $ any (areIntersecting (bulletToCircle bullet)) pillars

mapIntersectedPlayers
    :: ([IntersectedPlayer] -> [IntersectedPlayer]) -> Movables -> Movables
mapIntersectedPlayers f (Movables nextBulletId bullets intersectedPlayers) =
    Movables nextBulletId bullets (f intersectedPlayers)

updatePlayers :: [Event] -> DeltaTime -> Obstacles -> Movables -> Movables
updatePlayers events dt obstacles = mapIntersectedPlayers $ godFoldr
    ( concatApply
    $ (mapPlayer . updatePlayer events dt)
    . addToObstacles obstacles
    )

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

moveBullets :: DeltaTime -> Movables -> Movables
moveBullets dt (Movables nextBulletId bullets intersectedPlayers) =
    Movables nextBulletId (map (updateBullet dt) bullets) intersectedPlayers

fireBullets :: [Event] -> Movables -> Movables
fireBullets events (Movables nextBulletId bullets intersectedPlayers) = foldr
    (fireBullet' events)
    (Movables nextBulletId bullets [])
    intersectedPlayers

fireBullet' :: [Event] -> IntersectedPlayer -> Movables -> Movables
fireBullet' events toBeUpdated updated =
    let
        Movables nextBulletId updatedBullets updatedPlayers = updated
        IntersectedPlayer passingThrough player = toBeUpdated
        (newPlayer, maybeBullet) = fireBullet events nextBulletId player
        ifJust maybeSomething thenDo = maybe id (const thenDo) maybeSomething
        ifBullet          = ifJust maybeBullet
        newPassingThrough = ifBullet (nextBulletId :) passingThrough
        newNextBulletId   = ifBullet (+ 1) nextBulletId
        updatedIntersectedPlayer =
            IntersectedPlayer newPassingThrough newPlayer
    in
        Movables newNextBulletId
                 (maybe id (:) maybeBullet updatedBullets)
                 (updatedIntersectedPlayer : updatedPlayers)

updateIntersectingStatus :: Movables -> Movables
updateIntersectingStatus (Movables nextBulletId bullets players) =
    foldr updateIntersectingStatus' (Movables nextBulletId [] players) bullets

updateIntersectingStatus' :: Bullet -> Movables -> Movables
updateIntersectingStatus' bullet updated =
    let Movables nextBulletId updatedBullets players = updated
        (newBullet, updatedPlayers) = foldr registerHit (bullet, []) players
    in  Movables nextBulletId (newBullet : updatedBullets) updatedPlayers

registerHit
    :: IntersectedPlayer
    -> (Bullet, [IntersectedPlayer])
    -> (Bullet, [IntersectedPlayer])
registerHit intersectedPlayer (bullet, registered) =
    let
        IntersectedPlayer piercingBullets player = intersectedPlayer
        bulletId               = getBulletId bullet
        previouslyIntersecting = elem bulletId piercingBullets
        intersecting =
            areIntersecting (bulletToCircle bullet) (playerToCircle player)
    in
        second (: registered) $ if not previouslyIntersecting && intersecting
            then
                ( setBulletHit bullet
                , IntersectedPlayer (bulletId : piercingBullets)
                                    (inflictDamage bulletDamage player)
                )
            else if previouslyIntersecting && not intersecting
                then
                    ( bullet
                    , mapIntersecting (delete bulletId) intersectedPlayer
                    )
                else (bullet, intersectedPlayer)

removeOutOfBounds :: Bounds2D -> Movables -> Movables
removeOutOfBounds bounds =
    mapIntersected (filter (isBulletWithinBounds bounds))

mapIntersected :: ([Bullet] -> [Bullet]) -> Movables -> Movables
mapIntersected f (Movables nextBulletId bullets intersectedPlayers) =
    Movables nextBulletId (f bullets) intersectedPlayers

mapPlayer :: (Player -> Player) -> IntersectedPlayer -> IntersectedPlayer
mapPlayer f (IntersectedPlayer intersecting player) =
    IntersectedPlayer intersecting (f player)

mapIntersecting
    :: ([BulletId] -> [BulletId]) -> IntersectedPlayer -> IntersectedPlayer
mapIntersecting f (IntersectedPlayer intersecting player) =
    IntersectedPlayer (f intersecting) player

assignJoysticksToMatch :: [JoystickID] -> Match -> Match
assignJoysticksToMatch newJoysticks (Match movables obstacles) =
    let Movables nextBulletId bullets players = movables
        newPlayers = assignJoysticksToPlayers newJoysticks players
    in  Match (Movables nextBulletId bullets newPlayers) obstacles

assignJoysticksToPlayers
    :: [JoystickID] -> [IntersectedPlayer] -> [IntersectedPlayer]
assignJoysticksToPlayers _        []       = []
assignJoysticksToPlayers []       ps       = ps
assignJoysticksToPlayers (j : js) (p : ps) = if hasJoystick (getPlayer p)
    then p : assignJoysticksToPlayers (j : js) ps
    else mapPlayer (setJoystickId j) p : assignJoysticksToPlayers js ps
