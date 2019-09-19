module Game where

import           Player
import           Shot
import           Space
import           SDL
import           Data.Word                      ( Word8
                                                , Word32
                                                )
import           Foreign.C.Types
import           Data.Maybe                     ( maybeToList )

data Game = Game Time Player [Shot] Bool

gameTextureFiles :: [FilePath]
gameTextureFiles = [playerTextureFile, shotTextureFile]

createGame :: Game
createGame = Game 0 createPlayer [] False

toDrawableGame :: Game -> [(FilePath, Maybe (Rectangle CInt), CDouble)]
toDrawableGame (Game _ player shots _) =
    toDrawablePlayer player : map toDrawableShot shots

updateGame :: Game -> [Event] -> Word32 -> V2 CInt -> Game
updateGame (Game time player shots isFinished) events newWordTime (V2 bx by)
    = Game
        newTime
        newPlayer
        (filter
            (flip isShotWithinBounds bounds)
            (map (flip updateShot passedTime)
                 (shots ++ (maybeToList (triggerShot newPlayer events)))
            )
        )
        (isFinished || any isClosedEvent events)
  where
    newTime    = fromIntegral newWordTime
    passedTime = newTime - time
    bounds     = Bounds2D (0, fromIntegral bx) (0, fromIntegral by)
    newPlayer  = updatePlayer player events passedTime bounds

isClosedEvent :: Event -> Bool
isClosedEvent (Event _ (WindowClosedEvent _)) = True
isClosedEvent _                               = False

isFinished :: Game -> Bool
isFinished (Game _ _ _ isFinished) = isFinished
