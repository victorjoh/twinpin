module Player
    ( Player
    , getPlayerImages
    , initPlayer
    , drawPlayer
    , updatePlayer
    )
where

import           Time
import           SDL
import           Data.Word                      ( Word8
                                                , Word32
                                                )
import           Foreign.C.Types
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromJust )

type Position1D = Float
type Velocity1D = Float
data Position2D = Position2D Position1D Position1D
data Velocity2D = Velocity2D Velocity1D Velocity1D
data Player = Player Position2D Velocity2D Texture

textureFile :: FilePath
textureFile = "gen/player.bmp"

getPlayerImages :: [FilePath]
getPlayerImages = [textureFile]

initPlayer :: Map.Map FilePath Texture -> Player
initPlayer textureMap = Player
    (Position2D 0 0)
    (Velocity2D 0 0)
    (fromJust (Map.lookup textureFile textureMap))

drawPlayer :: Player -> [(Texture, Maybe (Rectangle CInt))]
drawPlayer (Player (Position2D x y) _ t) =
    [(t, Just (Rectangle (P (V2 (round x) (round y))) (V2 32 32)))]

updatePlayer :: Player -> [Event] -> DeltaTime -> Player
updatePlayer (Player p v t) es td = Player (updatePosition2D p v' td) v' t
    where v' = foldl updateVelocity v es

updateVelocity :: Velocity2D -> Event -> Velocity2D
updateVelocity (Velocity2D x y) (Event _ (JoyAxisEvent (JoyAxisEventData _ axis pos)))
    = let pos' = (0.00001 * fromIntegral pos)
      in  case axis of
              0 -> Velocity2D pos' y
              1 -> Velocity2D x pos'
              _ -> Velocity2D x y
updateVelocity v _ = v

updatePosition2D :: Position2D -> Velocity2D -> DeltaTime -> Position2D
updatePosition2D (Position2D x y) (Velocity2D x' y') dt =
    Position2D (updatePosition1D x x' dt) (updatePosition1D y y' dt)

updatePosition1D :: Position1D -> Velocity1D -> DeltaTime -> Position1D
updatePosition1D p v dt = p + v * fromIntegral dt
