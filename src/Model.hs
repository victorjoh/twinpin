module Model where

import           Time
import           Player
import           Shot
import           Space
import           SDL
import           Data.Word                      ( Word8
                                                , Word32
                                                )
import           Foreign.C.Types
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( maybeToList )

data Model = Model Time Player [Shot] (Map.Map FilePath Texture) Bool

getModelImages :: [FilePath]
getModelImages = [playerTextureFile, shotTextureFile]

createModel :: Map.Map FilePath Texture -> Model
createModel textureMap = Model 0 (createPlayer textureMap) [] textureMap False

drawModel :: Model -> [(Texture, Maybe (Rectangle CInt), CDouble)]
drawModel (Model _ player shots _ _) = drawPlayer player : map drawShot shots

updateModel :: Model -> [Event] -> Word32 -> V2 CInt -> Model
updateModel (Model time player shots textureMap isFinished) events newWordTime (V2 bx by)
    = Model
        newTime
        newPlayer
        (filter
            (flip isShotWithinBounds bounds)
            (map
                (flip updateShot passedTime)
                (  shots
                ++ (maybeToList (triggerShot newPlayer events textureMap))
                )
            )
        )
        textureMap
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
isFinished (Model _ _ _ _ isFinished) = isFinished
