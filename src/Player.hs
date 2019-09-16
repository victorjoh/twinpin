module Player
    ( Player
    , playerTextureFile
    , createPlayer
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

createPlayer :: Map.Map FilePath Texture -> Player
createPlayer textureMap = Player
    (Shape (size / 2)
           (V2 0 0)
           (fromJust (Map.lookup playerTextureFile textureMap))
    )
    (Aim2D 0 1 0)

drawPlayer :: Player -> (Texture, Maybe (Rectangle CInt), CDouble)
drawPlayer (Player shape (Aim2D _ _ angle)) = drawShape shape angle size

createAngle :: Direction1D -> Direction1D -> Angle2D -> Angle2D
createAngle 0 0 oldAngle = oldAngle
createAngle x y oldAngle | isTooSmall x && isTooSmall y = oldAngle
                         | otherwise = (atan2 y x) * (180 / pi)
    where isTooSmall direction = (abs direction) < 5000

updatePlayer :: Player -> [Event] -> DeltaTime -> Bounds2D -> Player
updatePlayer (Player (Shape position velocity texture) aim) events dt bounds =
    Player (Shape newPosition newVelocity texture) newAim
  where
    newVelocity = foldl updateVelocity velocity events
    newAim      = foldl updateAim aim events
    newPosition = limitPosition2D
        (updatePosition2D position newVelocity dt)
        (decreaseBounds2D bounds size)


triggerShot :: Player -> [Event] -> Map.Map FilePath Texture -> Maybe Shot
triggerShot (Player (Shape position _ _) (Aim2D _ _ angle)) events textureMap
    | any isTriggerEvent events = Just $ createShot position angle textureMap
    | otherwise                 = Nothing

isTriggerEvent :: Event -> Bool
isTriggerEvent (Event _ (JoyButtonEvent (JoyButtonEventData _ 5 JoyButtonPressed)))
    = True
isTriggerEvent _ = False

updateAim :: Aim2D -> Event -> Aim2D
updateAim (Aim2D x y angle) (Event _ (JoyAxisEvent (JoyAxisEventData _ axis position)))
    = let pos = fromIntegral position
      in  case axis of
              3 -> Aim2D pos y $ createAngle pos y angle
              4 -> Aim2D x pos $ createAngle x pos angle
              _ -> Aim2D x y angle
updateAim aim _ = aim

updateVelocity :: Velocity2D -> Event -> Velocity2D
updateVelocity (V2 x y) (Event _ (JoyAxisEvent (JoyAxisEventData _ axis position)))
    = let pos = (0.00001 * fromIntegral position)
      in  case axis of
              0 -> V2 pos y
              1 -> V2 x pos
              _ -> V2 x y
updateVelocity velocity _ = velocity
