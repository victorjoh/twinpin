module Player
    ( PlayerId(..)
    , Player(..)
    , Gun(..)
    , GunState(..)
    , Aim2D(..)
    , playerMaxHealth
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
    , getPlayerId
    , hasJoystick
    , setJoystickId
    , squareSector
    , scaleAndOffset
    , inflictDamage
    )
where

import           Space
import           Shot
import           Circle
import           Visual
import           SDL
import           Graphics.Rasterific     hiding ( V2(..) )
import qualified Graphics.Rasterific           as R
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
data PlayerId = Red | Blue deriving (Show, Eq, Ord, Enum)
data Player = Player Circle Velocity2D Gun Health PlayerId (Maybe JoystickID)
              deriving (Show, Eq)

playerMaxHealth :: Health
playerMaxHealth = 1.0

playerSide :: Size1D
playerSide = 60

playerRadius :: Radius
playerRadius = playerSide / 2

minShotInterval :: ReloadTime
minShotInterval = 250

axisPositionToVelocity :: Float
axisPositionToVelocity = 0.000018

triggerMinFireValue = 0
minAxisPosition = 5000
rightBumberButtonId = 5
rightTriggerButtonId = 5
baseColor = PixelRGBA8 0xE6 0xE6 0xE6 255
loadShadeColor = PixelRGBA8 0x16 0x0f 0x35 120
healthShadeColor = PixelRGBA8 0x48 0x2D 0x3B 120 -- 0x16 0x0f 0x35 50
baseImageId = "playerBase"

playerSize :: Size2D
playerSize = V2 playerSide playerSide

createPlayer :: Position2D -> Angle2D -> PlayerId -> Maybe JoystickID -> Player
createPlayer pos direction = Player (Circle pos (playerSide / 2))
                                    (V2 0 0)
                                    (Gun (Aim2D 0 0 direction) 0 Idle)
                                    playerMaxHealth

staticPlayerImage :: (ImageId, VectorImage)
staticPlayerImage = (baseImageId, toSolidCircleImage baseColor playerRadius)

drawPlayer :: Player -> [(Rectangle Float, Either VectorImage ImageId)]
drawPlayer (Player shape _ gun health playerId _) =
    let Circle _ radius = shape
        Gun (Aim2D _ _ direction) reloadTime _ = gun
        center          = R.V2 radius radius
        textureArea     = toTextureArea shape
        diameter        = toDiameter radius
        healthRadians   = (1 - health) * (2 * pi)
        aimShape =
                withClipping
                        ( stroke (playerSide / 3) JoinRound (CapRound, CapRound)
                        $ line center (R.V2 playerSide radius)
                        )
                    $ toDrawing radius
        shadowLength = reloadTimeToAimShadowLength (radius * 4 / 3) reloadTime
    in  [ (textureArea, Right baseImageId)
        , ( textureArea
          , Left
              $ VectorImage (V2 diameter diameter) transparent
              $ withTransformation (rotateCenter direction center)
              $ do
                    withTexture (uniformTexture healthShadeColor) $ withClipping
                        (fill $ polygon $ scaleAndOffset radius $ squareSector
                            healthRadians
                        )
                        (toDrawing radius)
                    withTexture (uniformTexture $ aimColor playerId) aimShape
                    withTexture (uniformTexture loadShadeColor) $ withClipping
                        (fill $ rectangle
                            (R.V2 (radius * 2 / 3) (radius * 2 / 3))
                            shadowLength
                            (playerSide / 3)
                        )
                        aimShape
          )
        ]

scaleAndOffset :: Size1D -> [R.V2 Float] -> [R.V2 Float]
scaleAndOffset value = map $ fmap (+ value) . (^* value)

squareSector :: Angle2D -> [R.V2 Float]
squareSector a
    | a <= 0
    = []
    | a > 0 && a <= (pi / 4)
    = R.V2 1 (-tan a) : squareSector0
    | a > (pi / 4) && a <= (pi * 3 / 4)
    = R.V2 (tan $ pi / 2 - a) (-1) : squareSector45
    | a > (pi * 3 / 4) && a <= (pi * 5 / 4)
    = R.V2 (-1) (tan a) : squareSector135
    | a > (pi * 5 / 4) && a <= (pi * 7 / 4)
    = R.V2 (tan $ a - pi / 2) 1 : squareSector225
    | a > (pi * 7 / 4) && a < (2 * pi)
    = R.V2 1 (-tan a) : squareSector315
    | otherwise
    = [R.V2 1 1, R.V2 (-1) 1, R.V2 (-1) (-1), R.V2 1 (-1)]
  where
    squareSector0   = [R.V2 1 0, R.V2 0 0]
    squareSector45  = R.V2 1 (-1) : squareSector0
    squareSector135 = R.V2 (-1) (-1) : squareSector45
    squareSector225 = R.V2 (-1) 1 : squareSector135
    squareSector315 = R.V2 1 1 : squareSector225

reloadTimeToAimShadowLength :: Float -> ReloadTime -> Float
reloadTimeToAimShadowLength maxShadowWidth reloadTime =
    maxShadowWidth * fromIntegral reloadTime / fromIntegral minShotInterval

aimColor :: PlayerId -> PixelRGBA8
aimColor Red  = PixelRGBA8 0x5F 0x5F 0xD3 255
aimColor Blue = PixelRGBA8 0xD3 0x5F 0x5F 255

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
        Player shape velocity gun health playerId maybeJoystickId = player
        Gun aim reloadTime state = gun
        axisEvents               = case maybeJoystickId of
            Just joystickId ->
                map (joyAxisEventAxis &&& joyAxisEventValue)
                    $ filter ((joystickId ==) . joyAxisEventWhich)
                    $ mapMaybe toJoyAxis events
            Nothing -> []
        newVelocity = foldl' updateVelocity velocity axisEvents
        newAim      = foldl' updateAim aim axisEvents
        newCircle =
            updateCollidingCirclePosition newVelocity dt obstacles shape
        newReloadTime = max 0 $ reloadTime - dt
    in
        Player newCircle
               newVelocity
               (Gun newAim newReloadTime state)
               health
               playerId
               (updateJoystick events =<< maybeJoystickId)

updateJoystick :: [Event] -> JoystickID -> Maybe JoystickID
updateJoystick events joystickId = if hasJoystickRemovedEvent events joystickId
    then Nothing
    else Just joystickId

hasJoystickRemovedEvent :: [Event] -> JoystickID -> Bool
hasJoystickRemovedEvent events joystickId = any
    ( (==) (JoyDeviceEvent $ JoyDeviceEventData JoyDeviceRemoved joystickId)
    . eventPayload
    )
    events

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

triggerShot :: [Event] -> ShotId -> Player -> (Player, Maybe Shot)
triggerShot events shotId player =
    let
        Player (Circle position _) _ gun _ _ maybeJoystickId = player
        Gun aim reloadTime state = gun
        Aim2D _ _ direction = aim
        newState = getGunState state maybeJoystickId events
        (newReloadTime, maybeShot) = if reloadTime == 0 && newState == Firing
            then (minShotInterval, Just $ createShot position direction shotId)
            else (reloadTime, Nothing)
    in
        (setGun (Gun aim newReloadTime newState) player, maybeShot)

getGunState :: GunState -> Maybe JoystickID -> [Event] -> GunState
getGunState gunState (Just joystickId) events =
    foldl' seq gunState $ mapMaybe (eventToGunState joystickId) events
getGunState gunState Nothing _ = gunState

eventToGunState :: JoystickID -> Event -> Maybe GunState
eventToGunState playerJoystickId (Event _ (JoyAxisEvent axisEventData)) =
    let JoyAxisEventData joystickId buttonId amountPressed = axisEventData
    in  if joystickId == playerJoystickId && buttonId == rightTriggerButtonId
            then if amountPressed > triggerMinFireValue
                then Just Firing
                else Just Idle
            else Nothing
eventToGunState playerJoystickId (Event _ (JoyButtonEvent buttonEventData)) =
    let JoyButtonEventData joystickId buttonId buttonState = buttonEventData
    in  if joystickId == playerJoystickId && buttonId == rightBumberButtonId
            then case buttonState of
                JoyButtonPressed  -> Just Firing
                JoyButtonReleased -> Just Idle
            else Nothing
eventToGunState _ _ = Nothing

setGun :: Gun -> Player -> Player
setGun gun (Player shape velocity _ health playerId joystickId) =
    Player shape velocity gun health playerId joystickId

playerToCircle :: Player -> Circle
playerToCircle (Player shape _ _ _ _ _) = shape

getPlayerId :: Player -> PlayerId
getPlayerId (Player _ _ _ _ playerId _) = playerId

hasJoystick :: Player -> Bool
hasJoystick (Player _ _ _ _ _ maybeJoystickId) = isJust maybeJoystickId

setJoystickId :: JoystickID -> Player -> Player
setJoystickId joystickId (Player shape velocity gun health playerId _) =
    Player shape velocity gun health playerId $ Just joystickId

inflictDamage :: Health -> Player -> Player
inflictDamage damage (Player shape velocity gun health playerId joystickId) =
    Player shape velocity gun newHealth playerId joystickId
  where
    newHealth = if damage >= health then playerMaxHealth else health - damage
