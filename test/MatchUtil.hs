module MatchUtil where

import Bullet
import Match
import Player

getFirstIntersectedPlayer :: Match -> IntersectedPlayer
getFirstIntersectedPlayer (Match (Movables _ _ players) _) = head players

getPlayers :: Match -> [Player]
getPlayers (Match (Movables _ _ intersectedPlayers) _) =
  map toPlayer intersectedPlayers

toPlayer :: IntersectedPlayer -> Player
toPlayer (IntersectedPlayer _ player) = player

getFirstPlayer :: Match -> Player
getFirstPlayer match = toPlayer $ getFirstIntersectedPlayer match

getFirstIntersecting :: Match -> [BulletId]
getFirstIntersecting match =
  let IntersectedPlayer intersecting _ = getFirstIntersectedPlayer match
   in intersecting

getBullets :: Match -> [Bullet]
getBullets (Match (Movables _ bullets _) _) = bullets
