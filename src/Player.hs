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
import           Data.Int                       ( Int16 )
import           Foreign.C.Types
import           SDL.Raw.Types                  ( JoystickID )
import           Data.Maybe
import           Data.Tuple.Extra               ( (&&&) )

type ButtonId = Word8
type AxisId = Word8
type AxisPosition = Int16

type Direction1D = Float
-- Keep a 2D vector to cover the case when the aim stick is only moved in one
-- axis. Keep the rotation to cover the case when the aim stick is moved to the
-- default position (0, 0).
data Aim2D = Aim2D Direction1D Direction1D Angle2D deriving (Show, Eq)
data Player = Player Shape Aim2D JoystickID deriving (Show, Eq)

playerSide :: Size1D
playerSide = 32

axisPositionToVelocity = 0.00001
minAxisPosition = 5000
rightBumberButtonId = 5

playerSize :: Size2D
playerSize = V2 playerSide playerSide

playerTextureFile :: FilePath
playerTextureFile = "gen/player.bmp"

createPlayer :: Position2D -> Angle2D -> JoystickID -> Player
createPlayer pos angle joystickId =
    Player (Shape pos (V2 0 0)) (Aim2D 0 0 angle) joystickId

toDrawablePlayer :: Player -> (FilePath, Maybe (Rectangle CInt), CDouble)
toDrawablePlayer (Player shape (Aim2D _ _ angle) _) =
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

updatePlayer :: [Event] -> DeltaTime -> Bounds2D -> Player -> Player
updatePlayer events dt bounds (Player (Shape position velocity) aim joystickId)
    = Player (Shape newPosition newVelocity) newAim joystickId
  where
    axisEvents =
        map (joyAxisEventAxis &&& joyAxisEventValue)
            $ filter ((joystickId ==) . joyAxisEventWhich)
            $ mapMaybe toJoyAxis events
    newVelocity = foldl updateVelocity velocity axisEvents
    newAim      = foldl updateAim aim axisEvents
    newPosition = limitPosition2D
        (updatePosition2D position newVelocity dt)
        (decreaseBounds2D bounds playerSize)

toJoyAxis :: Event -> Maybe JoyAxisEventData
toJoyAxis (Event _ (JoyAxisEvent joyAxisEventData)) = Just joyAxisEventData
toJoyAxis _ = Nothing

updateAim :: Aim2D -> (AxisId, AxisPosition) -> Aim2D
updateAim (Aim2D x y angle) (axisId, axisPosition) =
    let pos = fromIntegral axisPosition
    in  case axisId of
            3 -> Aim2D pos y $ createAngle pos y angle
            4 -> Aim2D x pos $ createAngle x pos angle
            _ -> Aim2D x y angle

updateVelocity :: Velocity2D -> (AxisId, AxisPosition) -> Velocity2D
updateVelocity (V2 x y) (axisId, axisPosition) =
    let newV = axisPositionToVelocity * fromIntegral axisPosition
    in  case axisId of
            0 -> createVelocity newV y
            1 -> createVelocity x newV
            _ -> V2 x y

triggerShot :: [Event] -> Player -> Maybe Shot
triggerShot events (Player (Shape position _) (Aim2D _ _ angle) joystickId) 
    | any (== (rightBumberButtonId, JoyButtonPressed))
        $ map (joyButtonEventButton &&& joyButtonEventState)
        $ filter ((joystickId ==) . joyButtonEventWhich)
        $ mapMaybe toJoyButton events
    = Just $ createShot position angle
    | otherwise
    = Nothing

toJoyButton :: Event -> Maybe JoyButtonEventData
toJoyButton (Event _ (JoyButtonEvent joyButtonEventData)) =
    Just joyButtonEventData
toJoyButton _ = Nothing
