module Player
    ( PlayerId(..)
    , Color(..)
    , Player(..)
    , Gun(..)
    , GunState(..)
    , Aim2D(..)
    , Vitality(..)
    , Movement(..)
    , playerMaxHealth
    , Deaths
    , ReloadTime
    , BoostTime
    , playerSide
    , minFiringInterval
    , boostRechargeTime
    , boostDuration
    , fullBoostCycle
    , boostSpeed
    , playerSize
    , createPlayer
    , staticPlayerImage
    , drawPlayer
    , axisPositionToVelocity
    , updatePlayer
    , fireBullet
    , playerToCircle
    , setGun
    , triggerMinFireValue
    , hasJoystick
    , setJoystickId
    , inflictDamage
    , aimColor
    , getDeaths
    , getColorId
    , getPlayerId
    , setPlayerId
    , aimShadowShape
    , topLeftCurve
    , bottomLeftCurve
    , bottomLine
    , bottomRightCurve
    , topRightCurve
    , topLine
    , aimShape
    )
where

import           Space
import           Bullet
import           Circle
import           Visual
import           SDL                     hiding ( (^+^)
                                                , (^*)
                                                , lerp
                                                )
import           Graphics.Rasterific     hiding ( V2(..) )
import qualified Graphics.Rasterific           as R
                                                ( V2(..) )
import           Graphics.Rasterific.Texture
import           Graphics.Rasterific.Transformations
import           Graphics.Rasterific.Linear     ( (^*) )
import           Codec.Picture.Types
import           Data.Word                      ( Word8 )
import           Data.Int                       ( Int16 )
import           SDL.Raw.Types                  ( JoystickID )
import           Data.Maybe
import           Data.List                      ( foldl' )

type AxisId = Word8
type AxisPosition = Int16
type ButtonId = Word8
data AxisEvent = AxisEvent AxisId AxisPosition deriving Eq
data ButtonEvent = ButtonEvent ButtonId JoyButtonState deriving Eq
type InputEvent = Either AxisEvent ButtonEvent

-- Keep a 2D vector to cover the case when the aim stick is only moved in one
-- axis. Keep the rotation to cover the case when the aim stick is moved to the
-- default position (0, 0).
data Aim2D = Aim2D Direction1D Direction1D Angle2D deriving (Show, Eq)
type ReloadTime = Time
data GunState = Firing | Idle deriving (Show, Eq)
data Gun = Gun Aim2D ReloadTime GunState deriving (Show, Eq)

type Deaths = Int
data Vitality = Vitality Deaths Health deriving (Show, Eq)

data Color = Red | Blue deriving (Show, Eq)
data PlayerId = PlayerId Color (Maybe JoystickID) deriving (Show, Eq)

type BoostTime = Time
-- Keep the direction in addition to the velocity to record left trigger
-- movements while in boost mode. In boost mode the velocity is locked.
data Movement = Movement Velocity2D Direction2D BoostTime deriving (Show, Eq)

data Player = Player Circle Movement Gun Vitality PlayerId deriving (Show, Eq)

playerMaxHealth :: Health
playerMaxHealth = 1.0

playerSide :: Size1D
playerSide = 60

playerRadius :: Radius
playerRadius = playerSide / 2

minFiringInterval :: ReloadTime
minFiringInterval = 250

boostDuration :: BoostTime
boostDuration = 300

boostRechargeTime :: BoostTime
boostRechargeTime = 3000

fullBoostCycle :: BoostTime
fullBoostCycle = boostDuration + boostRechargeTime

boostSpeed :: Float
boostSpeed = 1.6

axisPositionToVelocity :: Float
axisPositionToVelocity = 0.000018

triggerMinFireValue = 0
minAxisPosition = 5000
leftBumberButtonId = 4
rightBumberButtonId = 5
leftTriggerButtonId = 2
rightTriggerButtonId = 5
baseColor = PixelRGBA8 0xE6 0xE6 0xE6 255
loadShadeColor = PixelRGBA8 0x16 0x0f 0x35 120
healthShadeColor = PixelRGBA8 0x48 0x2D 0x3B 120
baseImageId = "playerBase"

playerSize :: Size2D
playerSize = V2 playerSide playerSide

createPlayer :: Position2D -> Angle2D -> Color -> Maybe JoystickID -> Player
createPlayer pos direction color joystickId = Player
    (Circle pos (playerSide / 2))
    (Movement (V2 0 0) (V2 0 0) 0)
    (Gun (Aim2D 0 0 direction) 0 Idle)
    (Vitality 0 playerMaxHealth)
    (PlayerId color joystickId)

staticPlayerImage :: (ImageId, VectorImage)
staticPlayerImage = (baseImageId, toSolidCircleImage baseColor playerRadius)

drawPlayer :: Player -> [(Rectangle Float, Either VectorImage ImageId)]
drawPlayer (Player outline movement gun vitality (PlayerId colorId _)) =
    let center                        = R.V2 playerRadius playerRadius
        textureArea                   = toTextureArea outline
        diameter                      = toDiameter playerRadius
        Vitality _ health             = vitality
        healthRadians                 = (1 - health) * (2 * pi)
        Movement _ _ boostTime        = movement
        shadowLength = boostTimeToAimShadowLength (4 / 3) boostTime
        Gun (Aim2D _ _ direction) _ _ = gun
    in  [ (textureArea, Right baseImageId)
        , ( textureArea
          , Left
              $ VectorImage (V2 diameter diameter) transparent
              $ withTransformation (rotateCenter direction center)
              $ do
                    fillShape healthShadeColor $ circleSector healthRadians
                    fillShape (aimColor colorId) aimShape
                    fillShape loadShadeColor $ aimShadowShape shadowLength
          )
        ]

fillShape :: PixelRGBA8 -> [Either CubicBezier Line] -> Drawing PixelRGBA8 ()
fillShape color shape = withTexture (uniformTexture color) $ fill $ map
    (either (CubicBezierPrim . scaleAndOffset playerRadius)
            (LinePrim . scaleAndOffset playerRadius)
    )
    shape

aimColor :: Color -> PixelRGBA8
aimColor Red  = PixelRGBA8 0xD3 0x5F 0x5F 255
aimColor Blue = PixelRGBA8 0x5F 0x5F 0xD3 255

boostTimeToAimShadowLength :: Float -> BoostTime -> Float
boostTimeToAimShadowLength maxShadowWidth boostTime =
    maxShadowWidth * if boostTime > boostRechargeTime
        then fromIntegral (fullBoostCycle - boostTime)
            / fromIntegral boostDuration
        else fromIntegral boostTime / fromIntegral boostRechargeTime

--               , - ~ ~ ~ - ,
--           , '               ' ,
--         ,             topLine   ,
--        ,          , ~ ~ ~ ~ ~ ~ ~,
--    topLeftCurve '                 , topRightCurve
--       ,        |  -  -  -  -  -  -,
-- bottomLeftCurve ,                 , bottomRightCurve
--        ,          ' ~ ~ ~ ~ ~ ~ ~,
--         ,           bottomLine  ,
--           ,                  , '
--             ' - , _ _ _ ,  '
rightCurveT = tan (1 / 3) * 2 / pi
topRightCurve = fst $ breakCubicBezierAt circleQuadrant1 rightCurveT
bottomRightCurve = snd $ breakCubicBezierAt circleQuadrant4 (1 - rightCurveT)
bottomLine = Line (R.V2 0 (1 / 3)) (_cBezierX0 bottomRightCurve)
topLine = Line (_cBezierX3 topRightCurve) (R.V2 0 (-1 / 3))
topLeftCurve = transform (^* (1 / 3)) circleQuadrant2
bottomLeftCurve = transform (^* (1 / 3)) circleQuadrant3

aimShape :: [Either CubicBezier Line]
aimShape =
    [ Left topLeftCurve
    , Left bottomLeftCurve
    , Right bottomLine
    , Left bottomRightCurve
    , Left topRightCurve
    , Right topLine
    ]

aimShadowShape :: Float -> [Either CubicBezier Line]
aimShadowShape len
    | len <= 0
    = []
    | len > 0 && len < 1 / 3
    = let
          t                  = len * 3
          cutTopLeftCurve    = snd $ breakCubicBezierAt topLeftCurve (1 - t)
          cutBottomLeftCurve = fst $ breakCubicBezierAt bottomLeftCurve t
      in
          [ Left cutTopLeftCurve
          , Left cutBottomLeftCurve
          , Right $ Line (_cBezierX3 cutBottomLeftCurve)
                         (_cBezierX0 cutTopLeftCurve)
          ]
    | len >= 1 / 3 && len <= lineLength + 1 / 3
    = let t             = (len - 1 / 3) / lineLength
          cutBottomLine = fst $ breakLineAt bottomLine t
          cutTopLine    = snd $ breakLineAt topLine (1 - t)
      in  [ Left topLeftCurve
          , Left bottomLeftCurve
          , Right cutBottomLine
          , Right $ Line (_lineX1 cutBottomLine) (_lineX0 cutTopLine)
          , Right cutTopLine
          ]
    | len > lineLength + 1 / 3 && len < 4 / 3
    = let
          t                   = (len - lineLength - 1 / 3) / (1 - lineLength)
          cutBottomRightCurve = fst $ breakCubicBezierAt bottomRightCurve t
          cutTopRightCurve    = snd $ breakCubicBezierAt topRightCurve (1 - t)
      in
          [ Left topLeftCurve
          , Left bottomLeftCurve
          , Right bottomLine
          , Left cutBottomRightCurve
          , Right $ Line (_cBezierX3 cutBottomRightCurve)
                         (_cBezierX0 cutTopRightCurve)
          , Left cutTopRightCurve
          , Right topLine
          ]
    | otherwise
    = aimShape
  where
    getX (R.V2 x _) = x
    lineLength = getX $ _cBezierX0 bottomRightCurve

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
        Player shape movement gun vitality playerId = player
        PlayerId color maybeJoystickId = playerId
        playerEvents                   = getPlayerEvents maybeJoystickId events
        axisEvents                     = toAxisEvents playerEvents
        newGun                         = updateGun axisEvents dt gun
        Gun newAim _ _                 = newGun
        buttonEvents                   = toButtonEvents playerEvents
        (newMovement, velocities) =
            updateMovement dt buttonEvents axisEvents newAim movement
        newCircle = updateCircle velocities obstacles shape
        newPlayerId =
            (PlayerId color $ updateJoystick events =<< maybeJoystickId)
    in
        Player newCircle newMovement newGun vitality newPlayerId

updateGun :: [AxisEvent] -> DeltaTime -> Gun -> Gun
updateGun axisEvents dt (Gun aim reloadTime state) =
    Gun (foldl' updateAim aim axisEvents) (countDown reloadTime dt) state

updateMovement
    :: DeltaTime
    -> [ButtonEvent]
    -> [AxisEvent]
    -> Aim2D
    -> Movement
    -> (Movement, [(Velocity2D, Time)])
updateMovement dt buttonEvents axisEvents aim movement =
    let Aim2D _ _ aimAngle = aim
        Movement velocity direction boostTime = movement
        newBoostTime = updateBoostTime dt buttonEvents axisEvents boostTime
        newDirection = foldl' updateVelocity direction axisEvents
        willNotBeBoosting = newBoostTime <= boostRechargeTime
        newVelocity
            | newBoostTime > boostTime = if newDirection /= V2 0 0
                then boostSpeed @* toUnitVector newDirection
                else boostSpeed @* angleToVector aimAngle
            | willNotBeBoosting = newDirection
            | otherwise = velocity
        wasBoosting = boostTime >= boostRechargeTime
        velocities  = if wasBoosting && willNotBeBoosting
            then
                [ (velocity   , boostTime - boostRechargeTime)
                , (newVelocity, boostRechargeTime - newBoostTime)
                ]
            else [(newVelocity, dt)]
    in  (Movement newVelocity newDirection newBoostTime, velocities)

countDown :: Time -> DeltaTime -> Time
countDown current dt = max 0 $ current - dt

updateBoostTime
    :: DeltaTime -> [ButtonEvent] -> [AxisEvent] -> BoostTime -> BoostTime
updateBoostTime _ buttonEvents axisEvents 0 =
    if any isLeftBumperPressed buttonEvents
            || any isLeftTriggerPressed axisEvents
        then fullBoostCycle
        else 0
updateBoostTime dt _ _ current = countDown current dt

getPlayerEvents :: Maybe JoystickID -> [Event] -> [Event]
getPlayerEvents Nothing _ = []
getPlayerEvents (Just joystickId) events =
    filter (hasJoystickId joystickId) events

hasJoystickId :: JoystickID -> Event -> Bool
hasJoystickId joystickId = maybe False (joystickId ==) . getJoystickId

getJoystickId :: Event -> Maybe JoystickID
getJoystickId (Event _ payload) = case payload of
    JoyAxisEvent (JoyAxisEventData joyId _ _) -> Just joyId
    JoyButtonEvent (JoyButtonEventData joyId _ _) -> Just joyId
    JoyDeviceEvent (JoyDeviceEventData _ joyId) -> Just joyId
    _ -> Nothing

toAxisEvents :: [Event] -> [AxisEvent]
toAxisEvents = mapMaybe toAxisEvent

toButtonEvents :: [Event] -> [ButtonEvent]
toButtonEvents = mapMaybe toButtonEvent

toAxisEvent :: Event -> Maybe AxisEvent
toAxisEvent (Event _ (JoyAxisEvent joyAxisEventData)) =
    let JoyAxisEventData _ axisId position = joyAxisEventData
    in  Just $ AxisEvent axisId position
toAxisEvent _ = Nothing

toButtonEvent :: Event -> Maybe ButtonEvent
toButtonEvent (Event _ (JoyButtonEvent joyButtonEventData)) =
    let JoyButtonEventData _ buttonId state = joyButtonEventData
    in  Just $ ButtonEvent buttonId state
toButtonEvent _ = Nothing

toInputEvent :: Event -> Maybe InputEvent
toInputEvent event = case toAxisEvent event of
    Just axisEvent -> Just $ Left axisEvent
    _              -> case toButtonEvent event of
        Just buttonEvent -> Just $ Right buttonEvent
        _                -> Nothing

toInputEvents :: [Event] -> [InputEvent]
toInputEvents = mapMaybe toInputEvent

isLeftBumperPressed :: ButtonEvent -> Bool
isLeftBumperPressed = (==) $ ButtonEvent leftBumberButtonId JoyButtonPressed

isRightBumperPressed :: ButtonEvent -> Bool
isRightBumperPressed = (==) $ ButtonEvent rightBumberButtonId JoyButtonPressed

isRightBumperReleased :: ButtonEvent -> Bool
isRightBumperReleased =
    (==) $ ButtonEvent rightBumberButtonId JoyButtonReleased

isLeftTriggerPressed :: AxisEvent -> Bool
isLeftTriggerPressed (AxisEvent axisId axisPositon) =
    axisId == leftTriggerButtonId && axisPositon > triggerMinFireValue

updateCircle :: [(Velocity2D, DeltaTime)] -> Obstacles -> Circle -> Circle
updateCircle [] _ shape = shape
updateCircle ((velocity, dt) : vs) obstacles shape =
    updateCircle vs obstacles $ moveCollidingCircle velocity dt obstacles shape

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

updateAim :: Aim2D -> AxisEvent -> Aim2D
updateAim (Aim2D x y direction) (AxisEvent axisId axisPos) =
    let pos = fromIntegral axisPos
    in  case axisId of
            3 -> Aim2D pos y $ createAngle pos y direction
            4 -> Aim2D x pos $ createAngle x pos direction
            _ -> Aim2D x y direction

updateVelocity :: Velocity2D -> AxisEvent -> Velocity2D
updateVelocity (V2 x y) (AxisEvent axisId axisPos) =
    let newV = axisPositionToVelocity * fromIntegral axisPos
    in  case axisId of
            0 -> createVelocity newV y
            1 -> createVelocity x newV
            _ -> V2 x y

fireBullet :: [Event] -> BulletId -> Player -> (Player, Maybe Bullet)
fireBullet events bulletId player =
    let Player (Circle position _) _ gun _ (PlayerId _ joystickId) = player
        Gun aim reloadTime state = gun
        Aim2D _ _ direction = aim
        playerEvents = getPlayerEvents joystickId events
        newState = getGunState state $ toInputEvents playerEvents
        (newReloadTime, maybeBullet) = if reloadTime == 0 && newState == Firing
            then
                ( minFiringInterval
                , Just $ createBullet position direction bulletId
                )
            else (reloadTime, Nothing)
    in  (setGun (Gun aim newReloadTime newState) player, maybeBullet)

getGunState :: GunState -> [InputEvent] -> GunState
getGunState gunState events =
    foldl' seq gunState $ mapMaybe eventToGunState events

eventToGunState :: InputEvent -> Maybe GunState
eventToGunState (Left (AxisEvent axisId amountPressed)) =
    if axisId == rightTriggerButtonId
        then if amountPressed > triggerMinFireValue
            then Just Firing
            else Just Idle
        else Nothing
eventToGunState (Right buttonEvent)
    | isRightBumperPressed buttonEvent  = Just Firing
    | isRightBumperReleased buttonEvent = Just Idle
    | otherwise                         = Nothing

setGun :: Gun -> Player -> Player
setGun gun (Player shape velocity _ vitality playerId) =
    Player shape velocity gun vitality playerId

playerToCircle :: Player -> Circle
playerToCircle (Player shape _ _ _ _) = shape

hasJoystick :: Player -> Bool
hasJoystick (Player _ _ _ _ (PlayerId _ maybeJoystickId)) =
    isJust maybeJoystickId

setJoystickId :: JoystickID -> Player -> Player
setJoystickId joystickId (Player shape velocity gun vitality playerId) =
    let PlayerId color _ = playerId
    in  Player shape velocity gun vitality (PlayerId color $ Just joystickId)

inflictDamage :: Health -> Player -> Player
inflictDamage damage (Player shape velocity gun vitality playerId) =
    let Vitality deaths health = vitality
        (newDeaths, newHealth) = if damage >= health
            then (deaths + 1, playerMaxHealth)
            else (deaths, health - damage)
    in  Player shape velocity gun (Vitality newDeaths newHealth) playerId

getDeaths :: Player -> Deaths
getDeaths (Player _ _ _ (Vitality deaths _) _) = deaths

getColorId :: Player -> Color
getColorId player = let PlayerId color _ = getPlayerId player in color

getPlayerId :: Player -> PlayerId
getPlayerId (Player _ _ _ _ playerId) = playerId

setPlayerId :: PlayerId -> Player -> Player
setPlayerId playerId (Player shape velocity gun vitality _) =
    Player shape velocity gun vitality playerId
