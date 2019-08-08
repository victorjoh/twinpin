module Model where

import           Time
import           Player
import           Space
import           SDL
import           Data.Word                      ( Word8
                                                , Word32
                                                )
import           Foreign.C.Types
import qualified Data.Map.Strict               as Map

data Model = Model Time Player

getModelImages :: [FilePath]
getModelImages = getPlayerImages

initModel :: Map.Map FilePath Texture -> Model
initModel textureMap = Model 0 $ initPlayer textureMap

drawModel :: Model -> [(Texture, Maybe (Rectangle CInt), CDouble)]
drawModel (Model _ p) = drawPlayer p

updateModel :: Model -> [Event] -> Word32 -> V2 CInt -> Model
updateModel (Model t p) es tw' (V2 x y) =
    Model t' $ updatePlayer p es (t' - t)
        (Bounds2D (0, fromIntegral x) (0, fromIntegral y))
            where t' = fromIntegral tw'
