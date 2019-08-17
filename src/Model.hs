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

data Model = Model Time Player [Shot] (Map.Map FilePath Texture)

getModelImages :: [FilePath]
getModelImages = [playerTextureFile, shotTextureFile]

createModel :: Map.Map FilePath Texture -> Model
createModel textureMap = Model 0 (createPlayer textureMap) [] textureMap

drawModel :: Model -> [(Texture, Maybe (Rectangle CInt), CDouble)]
drawModel (Model _ player shots _) = drawPlayer player : map drawShot shots

updateModel :: Model -> [Event] -> Word32 -> V2 CInt -> Model
updateModel (Model t player shots textureMap) events newWordTime (V2 bx by) =
    Model
        newT
        newPlayer
        (filter (flip isShotWithinBounds bounds)
            (map (flip updateShot dt)
                (shots ++
                    (maybeToList (triggerShot newPlayer events textureMap)))))
        textureMap
            where newT = fromIntegral newWordTime
                  dt = (newT - t)
                  bounds = Bounds2D (0, fromIntegral bx) (0, fromIntegral by)
                  newPlayer = updatePlayer player events dt bounds
