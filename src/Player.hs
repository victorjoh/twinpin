module Player
    ( Player(..)
    , Gun(..)
    , GunState(..)
    , Aim2D(..)
    , ReloadTime
    , playerSide
    , minShotInterval
    , playerSize
    , createPlayer
    , toDrawablePlayer
    , axisPositionToVelocity
    , updatePlayer
    , triggerShot
    , playerToCircle
    , setGun
    )
where

import           Space
import           Shot
import           Circle
import           SDL
import           Graphics.Rasterific     hiding ( V2(..) )
import qualified Graphics.Rasterific           as Rasterific
                                                ( V2(..) )
import           Graphics.Rasterific.Texture
import           Graphics.Rasterific.Transformations
import           Codec.Picture.Types
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
type ReloadTime = DeltaTime
data GunState = Firing | Idle deriving (Show, Eq)
data Gun = Gun Aim2D ReloadTime GunState deriving (Show, Eq)
data Player = Player Circle Velocity2D Gun JoystickID deriving (Show, Eq)

playerSide :: Size1D
playerSide = 32

minShotInterval :: ReloadTime
minShotInterval = 250

axisPositionToVelocity :: Float
axisPositionToVelocity = 0.00001

minAxisPosition = 5000
rightBumberButtonId = 5
baseColor = PixelRGBA8 0xE6 0xE6 0xE6 255
aimColor = PixelRGBA8 0xD3 0x5F 0x5F 255

playerSize :: Size2D
playerSize = V2 playerSide playerSide

createPlayer :: Position2D -> Angle2D -> JoystickID -> Player
createPlayer pos angle =
    Player (Circle pos (playerSide / 2)) (V2 0 0) (Gun (Aim2D 0 0 angle) 0 Idle)

toDrawablePlayer :: Player -> (Rectangle CInt, Image PixelRGBA8)
toDrawablePlayer (Player shape _ (Gun (Aim2D _ _ angle) _ _) joystickId) =
    let Circle _ radius = shape
        center          = Rasterific.V2 radius radius
    in  toCircleTextureWithOverlay
            ( withTexture (uniformTexture aimColor)
            $ withClipping
                  ( withTransformation (rotateCenter angle center)
                  $ stroke (playerSide / 3) JoinRound (CapRound, CapRound)
                  $ line center (Rasterific.V2 playerSide radius)
                  )
            $ toDrawing shape
            )
            baseColor
            shape

createAngle :: Direction1D -> Direction1D -> Angle2D -> Angle2D
createAngle 0 0 oldAngle = oldAngle
createAngle x y oldAngle | isCloseToDefault x && isCloseToDefault y = oldAngle
                         | otherwise = atan2 y x
    where isCloseToDefault direction = abs direction < minAxisPosition

createVelocity :: Direction1D -> Direction1D -> Velocity2D
createVelocity x y | isCloseToDefault x && isCloseToDefault y = V2 0 0
                   | otherwise = V2 x y
  where
    isCloseToDefault direction =
        abs direction < minAxisPosition * axisPositionToVelocity

updatePlayer :: [Event] -> DeltaTime -> Obstacles -> Player -> Player
updatePlayer events dt obstacles player =
    let
        Player circle velocity (Gun aim reloadTime state) joystickId = player
        axisEvents =
            map (joyAxisEventAxis &&& joyAxisEventValue)
                $ filter ((joystickId ==) . joyAxisEventWhich)
                $ mapMaybe toJoyAxis events
        newVelocity = foldl updateVelocity velocity axisEvents
        newAim      = foldl updateAim aim axisEvents
        newCircle =
            updateCollidingCirclePosition newVelocity dt obstacles circle
        newReloadTime = max 0 $ reloadTime - dt
    in
        Player newCircle newVelocity (Gun newAim newReloadTime state) joystickId

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

triggerShot :: [Event] -> Player -> (Player, Maybe Shot)
triggerShot events player =
    let
        Player (Circle position _) _ gun joystickId = player
        Gun   aim reloadTime state = gun
        Aim2D _   _          angle = aim
        newState                   = getGunState events state joystickId
        (newReloadTime, maybeShot) = if reloadTime == 0 && newState == Firing
            then (minShotInterval, Just $ createShot position angle)
            else (reloadTime, Nothing)
    in
        (setGun (Gun aim newReloadTime newState) player, maybeShot)

getGunState :: [Event] -> GunState -> JoystickID -> GunState
getGunState events gunState joystickId =
    foldr
            (\eventData _ -> case joyButtonEventState eventData of
                JoyButtonPressed  -> Firing
                JoyButtonReleased -> Idle
            )
            gunState
        $ filter ((rightBumberButtonId ==) . joyButtonEventButton)
        $ filter ((joystickId ==) . joyButtonEventWhich)
        $ mapMaybe toJoyButton events

setGun :: Gun -> Player -> Player
setGun gun (Player circle velocity _ joystickId) =
    Player circle velocity gun joystickId

toJoyButton :: Event -> Maybe JoyButtonEventData
toJoyButton (Event _ (JoyButtonEvent joyButtonEventData)) =
    Just joyButtonEventData
toJoyButton _ = Nothing

playerToCircle :: Player -> Circle
playerToCircle (Player circle _ _ _) = circle
