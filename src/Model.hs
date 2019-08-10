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

initModel :: Map.Map FilePath Texture -> Model
initModel textureMap = Model 0 (initPlayer textureMap) [] textureMap

drawModel :: Model -> [(Texture, Maybe (Rectangle CInt), CDouble)]
drawModel (Model _ p shots _) = drawPlayer p : map drawShot shots

updateModel :: Model -> [Event] -> Word32 -> V2 CInt -> Model
updateModel (Model t p shots textureMap) es tw' (V2 x y) = Model
    t' p' (
            map (flip updateShot td)
                    (filter (not . flip shotOutsideBounds bounds) shots)
            ++ (maybeToList (triggerShot p' es textureMap))
        ) textureMap
            where t' = fromIntegral tw'
                  td = (t' - t)
                  bounds = Bounds2D (0, fromIntegral x) (0, fromIntegral y)
                  p' = updatePlayer p es td bounds
