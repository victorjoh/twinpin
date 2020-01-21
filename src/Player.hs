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
    , staticPlayerImage
    , drawPlayer
    , axisPositionToVelocity
    , updatePlayer
    , triggerShot
    , playerToCircle
    , setGun
    , triggerMinFireValue
    )
where

import           Space
import           Shot
import           Circle
import           Visual
import           SDL
import           Graphics.Rasterific     hiding ( V2(..) )
import qualified Graphics.Rasterific           as Rasterific
                                                ( V2(..) )
import           Graphics.Rasterific.Texture
import           Graphics.Rasterific.Transformations
import           Codec.Picture.Types
import           Data.Word                      ( Word8 )
import           Data.Int                       ( Int16 )
import           SDL.Raw.Types                  ( JoystickID )
import           Data.Maybe
import           Data.Tuple.Extra               ( (&&&) )
import           Data.List                      ( foldl' )

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

playerRadius :: Radius
playerRadius = playerSide / 2

minShotInterval :: ReloadTime
minShotInterval = 250

axisPositionToVelocity :: Float
axisPositionToVelocity = 0.00001

triggerMinFireValue = 0
minAxisPosition = 5000
rightBumberButtonId = 5
rightTriggerButtonId = 5
baseColor = PixelRGBA8 0xE6 0xE6 0xE6 255
loadShadeColor = PixelRGBA8 0x16 0x0f 0x35 120
baseImageId = "playerBase"

playerSize :: Size2D
playerSize = V2 playerSide playerSide

createPlayer :: Position2D -> Angle2D -> JoystickID -> Player
createPlayer pos direction = Player (Circle pos (playerSide / 2))
                                    (V2 0 0)
                                    (Gun (Aim2D 0 0 direction) 0 Idle)

staticPlayerImage :: (ImageId, VectorImage)
staticPlayerImage = (baseImageId, toSolidCircleImage baseColor playerRadius)

drawPlayer :: Player -> [(Rectangle Float, Either VectorImage ImageId)]
drawPlayer (Player shape _ gun joystickId) =
    let Circle _ radius = shape
        Gun (Aim2D _ _ direction) reloadTime _ = gun
        center          = Rasterific.V2 radius radius
        textureArea     = toTextureArea shape
        diameter        = toDiameter radius
        aimShape =
                withClipping
                        ( stroke (playerSide / 3) JoinRound (CapRound, CapRound)
                        $ line center (Rasterific.V2 playerSide radius)
                        )
                    $ toDrawing radius
        shadowLength = reloadTimeToAimShadowLength (radius * 4 / 3) reloadTime
    in  [ (textureArea, Right baseImageId)
        , ( textureArea
          , Left
              $ VectorImage (V2 diameter diameter) transparent
              $ withTransformation (rotateCenter direction center)
              $ do
                    withTexture (uniformTexture $ aimColor joystickId) aimShape
                    withTexture (uniformTexture loadShadeColor) $ withClipping
                        (fill $ rectangle
                            (Rasterific.V2 (radius * 2 / 3) (radius * 2 / 3))
                            shadowLength
                            (playerSide / 3)
                        )
                        aimShape
          )
        ]

reloadTimeToAimShadowLength :: Float -> ReloadTime -> Float
reloadTimeToAimShadowLength maxShadowWidth reloadTime =
    maxShadowWidth * fromIntegral reloadTime / fromIntegral minShotInterval

aimColor :: JoystickID -> PixelRGBA8
aimColor 0 = PixelRGBA8 0x5F 0x5F 0xD3 255
aimColor _ = PixelRGBA8 0xD3 0x5F 0x5F 255

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
        Player shape velocity (Gun aim reloadTime state) joystickId = player
        axisEvents =
            map (joyAxisEventAxis &&& joyAxisEventValue)
                $ filter ((joystickId ==) . joyAxisEventWhich)
                $ mapMaybe toJoyAxis events
        newVelocity = foldl' updateVelocity velocity axisEvents
        newAim      = foldl' updateAim aim axisEvents
        newCircle =
            updateCollidingCirclePosition newVelocity dt obstacles shape
        newReloadTime = max 0 $ reloadTime - dt
    in
        Player newCircle newVelocity (Gun newAim newReloadTime state) joystickId

toJoyAxis :: Event -> Maybe JoyAxisEventData
toJoyAxis (Event _ (JoyAxisEvent joyAxisEventData)) = Just joyAxisEventData
toJoyAxis _ = Nothing

updateAim :: Aim2D -> (AxisId, AxisPosition) -> Aim2D
updateAim (Aim2D x y direction) (axisId, axisPos) =
    let pos = fromIntegral axisPos
    in  case axisId of
            3 -> Aim2D pos y $ createAngle pos y direction
            4 -> Aim2D x pos $ createAngle x pos direction
            _ -> Aim2D x y direction

updateVelocity :: Velocity2D -> (AxisId, AxisPosition) -> Velocity2D
updateVelocity (V2 x y) (axisId, axisPos) =
    let newV = axisPositionToVelocity * fromIntegral axisPos
    in  case axisId of
            0 -> createVelocity newV y
            1 -> createVelocity x newV
            _ -> V2 x y

triggerShot :: [Event] -> Player -> (Player, Maybe Shot)
triggerShot events player =
    let
        Player (Circle position _) _ gun joystickId = player
        Gun   aim reloadTime state     = gun
        Aim2D _   _          direction = aim
        newState                       = getGunState state joystickId events
        (newReloadTime, maybeShot) = if reloadTime == 0 && newState == Firing
            then (minShotInterval, Just $ createShot position direction)
            else (reloadTime, Nothing)
    in
        (setGun (Gun aim newReloadTime newState) player, maybeShot)

getGunState :: GunState -> JoystickID -> [Event] -> GunState
getGunState gunState joystickId =
    foldl' seq gunState . mapMaybe (eventToGunState joystickId)

eventToGunState :: JoystickID -> Event -> Maybe GunState
eventToGunState playerId (Event _ (JoyAxisEvent axisEventData)) =
    let JoyAxisEventData joystickId buttonId amountPressed = axisEventData
    in  if joystickId == playerId && buttonId == rightTriggerButtonId
            then if amountPressed > triggerMinFireValue
                then Just Firing
                else Just Idle
            else Nothing
eventToGunState playerId (Event _ (JoyButtonEvent buttonEventData)) =
    let JoyButtonEventData joystickId buttonId buttonState = buttonEventData
    in  if joystickId == playerId && buttonId == rightBumberButtonId
            then case buttonState of
                JoyButtonPressed  -> Just Firing
                JoyButtonReleased -> Just Idle
            else Nothing
eventToGunState _ _ = Nothing

setGun :: Gun -> Player -> Player
setGun gun (Player shape velocity _ joystickId) =
    Player shape velocity gun joystickId

playerToCircle :: Player -> Circle
playerToCircle (Player shape _ _ _) = shape
