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
import           Shape
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
data Aim2D = Aim2D Direction1D Direction1D Angle2D
data Player = Player Shape Aim2D

side :: Size1D
side = 32

size :: Size2D
size = V2 side side

playerTextureFile :: FilePath
playerTextureFile = "gen/player.bmp"

initPlayer :: Map.Map FilePath Texture -> Player
initPlayer textureMap = Player
    (Shape (size / 2)
           (V2 0 0)
           (fromJust (Map.lookup playerTextureFile textureMap)))
    (Aim2D 0 1 0)

drawPlayer :: Player -> (Texture, Maybe (Rectangle CInt), CDouble)
drawPlayer (Player shape (Aim2D _ _ angle)) = drawShape shape angle size

angle' :: Direction1D -> Direction1D -> Angle2D -> Angle2D
angle' 0 0 r = r
angle' x y r | isTooSmall x && isTooSmall y = r
             | otherwise = (atan2 y x) * (180 / pi)
        where isTooSmall c = (abs c) < 5000

updatePlayer :: Player -> [Event] -> DeltaTime -> Bounds2D -> Player
updatePlayer (Player (Shape p v t) a) es td bounds = Player (Shape p' v' t) a'
    where
        v' = foldl updateVelocity v es
        a' = foldl updateAim a es
        p' = limitPosition2D (updatePosition2D p v' td)
                             (decreaseBounds2D bounds size)


triggerShot :: Player -> [Event] -> Map.Map FilePath Texture -> Maybe Shot
triggerShot (Player (Shape pos _ _) (Aim2D _ _ angle)) events textureMap
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
updateVelocity (V2 x y) (Event _ (JoyAxisEvent (JoyAxisEventData _ axis pos)))
    = let pos' = (0.00001 * fromIntegral pos)
      in  case axis of
              0 -> V2 pos' y
              1 -> V2 x pos'
              _ -> V2 x y
updateVelocity v _ = v
