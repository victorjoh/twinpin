module MatchUtil where

import           Player
import           Match
import           Shot

getFirstPlayerWithBarrel :: Match -> PlayerWithBarrel
getFirstPlayerWithBarrel (Match (Movables _ players) _) = head players

toPlayer :: PlayerWithBarrel -> Player
toPlayer (PlayerWithBarrel player _) = player

getFirstPlayer :: Match -> Player
getFirstPlayer match = toPlayer $ getFirstPlayerWithBarrel match

getFirstBarrel :: Match -> [Shot]
getFirstBarrel match =
    let PlayerWithBarrel _ barrel = getFirstPlayerWithBarrel match in barrel

getShots :: Match -> [Shot]
getShots (Match (Movables shots _) _) = shots
