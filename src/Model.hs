module Model where

import           Time
import           Player
import           SDL
import           Data.Word                      ( Word8
                                                , Word32
                                                )
import           Foreign.C.Types

data Model = Model Time Player deriving (Show)

initModel :: Model
initModel = Model 0 initPlayer

drawModel :: Model -> [(V4 Word8, Maybe (Rectangle CInt))]
drawModel (Model _ p) = drawPlayer p

updateModel :: Model -> [Event] -> Word32 -> Model
updateModel (Model t p) es tw' = Model t' $ updatePlayer p es (t' - t)
    where t' = fromIntegral tw'
