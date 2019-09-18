module Model where

import           Player
import           Shot
import           Space
import           SDL
import           Data.Word                      ( Word8
                                                , Word32
                                                )
import           Foreign.C.Types
import           Data.Maybe                     ( maybeToList )

data Model = Model Time Player [Shot] Bool

getModelImages :: [FilePath]
getModelImages = [playerTextureFile, shotTextureFile]

createModel :: Model
createModel = Model 0 createPlayer [] False

toDrawableModel :: Model -> [(FilePath, Maybe (Rectangle CInt), CDouble)]
toDrawableModel (Model _ player shots _) =
    toDrawablePlayer player : map toDrawableShot shots

updateModel :: Model -> [Event] -> Word32 -> V2 CInt -> Model
updateModel (Model time player shots isFinished) events newWordTime (V2 bx by)
    = Model
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

isFinished :: Model -> Bool
isFinished (Model _ _ _ isFinished) = isFinished
