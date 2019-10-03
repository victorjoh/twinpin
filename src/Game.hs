module Game where

import           Player
import           Shot
import           Space
import           SDL
import           Data.Word                      ( Word8
                                                , Word32
                                                )
import           Foreign.C.Types
import           Data.Maybe                     ( mapMaybe )

data Game = Game Time [Player] [Shot] Bool

gameTextureFiles :: [FilePath]
gameTextureFiles = [playerTextureFile, shotTextureFile]

createGame :: V2 CInt -> Game
createGame (V2 bx by) = Game
    0
    [ (createPlayer (V2 xDistanceFromEdge yMiddle) 0 0)
    , (createPlayer (V2 (fromIntegral bx - xDistanceFromEdge) yMiddle) 180 1)
    ]
    []
    False
  where
    yMiddle           = (fromIntegral by / 2)
    xDistanceFromEdge = playerSide + playerSide / 2

toDrawableGame :: Game -> [(FilePath, Maybe (Rectangle CInt), CDouble)]
toDrawableGame (Game _ players shots _) =
    map toDrawablePlayer players ++ map toDrawableShot shots

updateGame :: Game -> [Event] -> Word32 -> V2 CInt -> Game
updateGame (Game time players shots isFinished) events newWordTime (V2 bx by) =
    Game
        newTime
        newPlayers
        (filter
            (flip isShotWithinBounds bounds)
            (map (updateShot passedTime)
                 (shots ++ (mapMaybe (triggerShot events) newPlayers))
            )
        )
        (isFinished || any isClosedEvent events)
  where
    newTime    = fromIntegral newWordTime
    passedTime = newTime - time
    bounds     = Bounds2D (0, fromIntegral bx) (0, fromIntegral by)
    newPlayers = (map (updatePlayer events passedTime bounds) players)

isClosedEvent :: Event -> Bool
isClosedEvent (Event _ (WindowClosedEvent _)) = True
isClosedEvent _                               = False

isFinished :: Game -> Bool
isFinished (Game _ _ _ isFinished) = isFinished
