module Player
    ( Player
    , playerSide
    , playerSize
    , playerTextureFile
    , createPlayer
    , toDrawablePlayer
    , updatePlayer
    , triggerShot
    )
where

import           Space
import           Shot
import           Shape
import           SDL
import           Data.Word                      ( Word8
                                                , Word32
                                                )
import           Foreign.C.Types

type Direction1D = Float
-- Keep a 2D vector to cover the case when the aim stick is only moved in one
-- axis. Keep the rotation to cover the case when the aim stick is moved to the
-- default position (0, 0).
data Aim2D = Aim2D Direction1D Direction1D Angle2D deriving (Show, Eq)
data Player = Player Shape Aim2D deriving (Show, Eq)

playerSide :: Size1D
playerSide = 32

axisPositionToVelocity = 0.00001
minAxisPosition = 5000

playerSize :: Size2D
playerSize = V2 playerSide playerSide

playerTextureFile :: FilePath
playerTextureFile = "gen/player.bmp"

createPlayer :: Position2D -> Angle2D -> Player
createPlayer pos angle = Player (Shape pos (V2 0 0)) (Aim2D 0 0 angle)

toDrawablePlayer :: Player -> (FilePath, Maybe (Rectangle CInt), CDouble)
toDrawablePlayer (Player shape (Aim2D _ _ angle)) =
    toDrawableShape shape angle playerSize playerTextureFile

createAngle :: Direction1D -> Direction1D -> Angle2D -> Angle2D
createAngle 0 0 oldAngle = oldAngle
createAngle x y oldAngle | isCloseToDefault x && isCloseToDefault y = oldAngle
                         | otherwise = (atan2 y x) * (180 / pi)
    where isCloseToDefault direction = abs direction < minAxisPosition

createVelocity :: Direction1D -> Direction1D -> Velocity2D
createVelocity x y | isCloseToDefault x && isCloseToDefault y = V2 0 0
                   | otherwise = V2 x y
  where
    isCloseToDefault direction =
        abs direction < minAxisPosition * axisPositionToVelocity

updatePlayer :: Player -> [Event] -> DeltaTime -> Bounds2D -> Player
updatePlayer (Player (Shape position velocity) aim) events dt bounds = Player
    (Shape newPosition newVelocity)
    newAim
  where
    newVelocity = foldl updateVelocity velocity events
    newAim      = foldl updateAim aim events
    newPosition = limitPosition2D
        (updatePosition2D position newVelocity dt)
        (decreaseBounds2D bounds playerSize)


triggerShot :: Player -> [Event] -> Maybe Shot
triggerShot (Player (Shape position _) (Aim2D _ _ angle)) events
    | any isTriggerEvent events = Just $ createShot position angle
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
    = let newV = axisPositionToVelocity * fromIntegral position
      in  case axis of
              0 -> createVelocity newV y
              1 -> createVelocity x newV
              _ -> V2 x y
updateVelocity velocity _ = velocity
