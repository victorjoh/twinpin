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
type Aim1D = Float
type Rotation2D = Float
data Position2D = Position2D Position1D Position1D
data Velocity2D = Velocity2D Velocity1D Velocity1D
-- Keep a 2D vector to cover the case when the aim stick is only moved in one
-- axis. Keep the rotation to cover the case when the aim stick is moved to the
-- default position (0, 0).
data Aim2D = Aim2D Aim1D Aim1D Rotation2D
data Player = Player Position2D Velocity2D Aim2D Texture

textureFile :: FilePath
textureFile = "gen/player.bmp"

getPlayerImages :: [FilePath]
getPlayerImages = [textureFile]

initPlayer :: Map.Map FilePath Texture -> Player
initPlayer textureMap = Player
    (Position2D 0 0)
    (Velocity2D 0 0)
    (Aim2D 0 1 0)
    (fromJust (Map.lookup textureFile textureMap))

drawPlayer :: Player -> [(Texture, Maybe (Rectangle CInt), CDouble)]
drawPlayer (Player (Position2D x y) _ (Aim2D _ _ r) t) =
    [ ( t
      , Just (Rectangle (P (V2 (round x) (round y))) (V2 32 32))
      , (realToFrac r)
      )
    ]

angle' :: Aim1D -> Aim1D -> Rotation2D -> Rotation2D
angle' 0 0 r = r
angle' x y r | isTooSmall x && isTooSmall y = r
             | otherwise = (atan2 x (-y)) * (180 / pi)
        where isTooSmall c = (abs c) < 5000

updatePlayer :: Player -> [Event] -> DeltaTime -> Player
updatePlayer (Player p v a t) es td = Player (updatePosition2D p v' td) v' a' t
  where
    v' = foldl updateVelocity v es
    a' = foldl updateAim a es

updateAim :: Aim2D -> Event -> Aim2D
updateAim (Aim2D x y r) (Event _ (JoyAxisEvent (JoyAxisEventData _ axis pos)))
    = let pos' = fromIntegral pos
      in  case axis of
              3 -> Aim2D pos' y $ angle' pos' y r
              4 -> Aim2D x pos' $ angle' x pos' r
              _ -> Aim2D x y r
updateAim aim _ = aim

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
