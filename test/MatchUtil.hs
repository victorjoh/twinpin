module MatchUtil where

import           Player
import           Match
import           Shot

getFirstIntersectedPlayer :: Match -> IntersectedPlayer
getFirstIntersectedPlayer (Match (Movables _ _ players) _) = head players

getPlayers :: Match -> [Player]
getPlayers (Match (Movables _ _ intersectedPlayers) _) =
    map toPlayer intersectedPlayers

toPlayer :: IntersectedPlayer -> Player
toPlayer (IntersectedPlayer _ player) = player

getFirstPlayer :: Match -> Player
getFirstPlayer match = toPlayer $ getFirstIntersectedPlayer match

getFirstIntersecting :: Match -> [ShotId]
getFirstIntersecting match =
    let IntersectedPlayer intersecting _ = getFirstIntersectedPlayer match
    in  intersecting

getShots :: Match -> [Shot]
getShots (Match (Movables _ shots _) _) = shots
