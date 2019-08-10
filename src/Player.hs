module Player
    ( Player
    , playerTextureFile
    , initPlayer
    , drawPlayer
    , updatePlayer
    , triggerShot
    )
where

import           Time
import           Space
import           Shot
import           SDL
import           Data.Word                      ( Word8
                                                , Word32
                                                )
import           Foreign.C.Types
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromJust )

type Direction1D = Float
-- Keep a 2D vector to cover the case when the aim stick is only moved in one
-- axis. Keep the rotation to cover the case when the aim stick is moved to the
-- default position (0, 0).
data Aim2D = Aim2D Direction1D Direction1D Rotation2D
data Player = Player Position2D Velocity2D Aim2D Texture

width :: Size1D
width = 32

height :: Size1D
height = 32

playerTextureFile :: FilePath
playerTextureFile = "gen/player.bmp"

initPlayer :: Map.Map FilePath Texture -> Player
initPlayer textureMap = Player
    (Position2D 0 0)
    (Velocity2D 0 0)
    (Aim2D 0 1 0)
    (fromJust (Map.lookup playerTextureFile textureMap))

drawPlayer :: Player -> (Texture, Maybe (Rectangle CInt), CDouble)
drawPlayer (Player (Position2D x y) _ (Aim2D _ _ r) t) =
    ( t
    , Just (Rectangle
            (P (V2 (round (x - width/2)) (round (y - height/2))))
            (V2 (round width) (round height)))
    , (realToFrac r)
    )

angle' :: Direction1D -> Direction1D -> Rotation2D -> Rotation2D
angle' 0 0 r = r
angle' x y r | isTooSmall x && isTooSmall y = r
             | otherwise = (atan2 y x) * (180 / pi)
        where isTooSmall c = (abs c) < 5000

updatePlayer :: Player -> [Event] -> DeltaTime -> Bounds2D -> Player
updatePlayer (Player p v a t) es td bounds = Player
    (updatePosition2D p v' td bounds) v' a' t
      where
          v' = foldl updateVelocity v es
          a' = foldl updateAim a es

triggerShot :: Player -> [Event] -> Map.Map FilePath Texture -> Maybe Shot
triggerShot (Player pos _ (Aim2D _ _ angle) _) events textureMap
    | any isTriggerEvent events = Just $ initShot pos angle textureMap
    | otherwise = Nothing

isTriggerEvent :: Event -> Bool
isTriggerEvent (Event _ (JoyButtonEvent (JoyButtonEventData _ 5 JoyButtonPressed))) = True
isTriggerEvent _ = False

-- TODO: this is similar to updateVelocity
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

updatePosition2D
    :: Position2D
    -> Velocity2D
    -> DeltaTime
    -> Bounds2D
    -> Position2D
updatePosition2D (Position2D x y) (Velocity2D x' y') dt (Bounds2D xb yb) =
    Position2D
        (updatePosition1D x (-width/2, width/2) x' dt xb)
        (updatePosition1D y (-height/2, height/2) y' dt yb)

updatePosition1D
    :: Position1D
    -> Shape1D
    -> Velocity1D
    -> DeltaTime
    -> Bounds1D
    -> Position1D
updatePosition1D p s v dt b = limit1D (p + v * fromIntegral dt) s b

limit1D :: Position1D -> Shape1D -> Bounds1D -> Position1D
limit1D p (lowerShape, upperShape) (lowerBound, upperBound) =
    max (lowerBound - lowerShape) $ min (upperBound - upperShape) p
