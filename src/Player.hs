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
import           Data.Tuple.Extra               ( (&&&) )
import           Data.List                      ( foldl' )

type AxisId = Word8
type AxisPosition = Int16
type ButtonId = Word8

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
    let Player shape movement gun vitality (PlayerId color maybeJoystickId)
            = player
        Gun aim reloadTime state = gun
        axisEvents               = case maybeJoystickId of
            Just joystickId ->
                map (joyAxisEventAxis &&& joyAxisEventValue)
                    $ filter ((joystickId ==) . joyAxisEventWhich)
                    $ mapMaybe toJoyAxis events
            Nothing -> []
        newAim = foldl' updateAim aim axisEvents
        (newMovement, velocities) =
                updateMovement dt maybeJoystickId events axisEvents aim movement
        newCircle     = updateCircle velocities obstacles shape
        newReloadTime = countDown reloadTime dt
    in  Player newCircle
               newMovement
               (Gun newAim newReloadTime state)
               vitality
               (PlayerId color $ updateJoystick events =<< maybeJoystickId)

type AxisEvent = (Word8, Int16)

updateMovement
    :: DeltaTime
    -> Maybe JoystickID
    -> [Event]
    -> [AxisEvent]
    -> Aim2D
    -> Movement
    -> (Movement, [(Velocity2D, Time)])
updateMovement dt playerJoystickId events axisEvents aim movement =
    let Aim2D _ _ aimAngle = aim
        Movement velocity direction boostTime = movement
        newBoostTime = updateBoostTime dt playerJoystickId events boostTime
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
    :: DeltaTime -> Maybe JoystickID -> [Event] -> BoostTime -> BoostTime
updateBoostTime _ (Just playerJostickId) events 0 =
    if any (isLeftBumperPressed playerJostickId) events
            || any (isLeftTriggerPressed playerJostickId) events
        then fullBoostCycle
        else 0
updateBoostTime dt _ _ current = countDown current dt

isLeftBumperPressed :: JoystickID -> Event -> Bool
isLeftBumperPressed = isButtonPressedEvent leftBumberButtonId

isRightBumperPressed :: JoystickID -> Event -> Bool
isRightBumperPressed = isButtonPressedEvent rightBumberButtonId

isRightBumperReleased :: JoystickID -> Event -> Bool
isRightBumperReleased = isButtonReleasedEvent rightBumberButtonId

isLeftTriggerPressed :: JoystickID -> Event -> Bool
isLeftTriggerPressed playerJoystickId (Event _ (JoyAxisEvent axisEventData)) =
    let JoyAxisEventData joystickId buttonId amountPressed = axisEventData
    in  playerJoystickId
            == joystickId
            && buttonId
            == leftTriggerButtonId
            && amountPressed
            >  triggerMinFireValue
isLeftTriggerPressed _ _ = False

isButtonReleasedEvent :: ButtonId -> JoystickID -> Event -> Bool
isButtonReleasedEvent buttonId joystickId =
    hasButtonEventData
        $ JoyButtonEventData joystickId buttonId JoyButtonReleased

isButtonPressedEvent :: ButtonId -> JoystickID -> Event -> Bool
isButtonPressedEvent buttonId joystickId =
    hasButtonEventData $ JoyButtonEventData joystickId buttonId JoyButtonPressed

hasButtonEventData :: JoyButtonEventData -> Event -> Bool
hasButtonEventData eventData1 event = case event of
    (Event _ (JoyButtonEvent eventData2)) -> eventData1 == eventData2
    _ -> False

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

fireBullet :: [Event] -> BulletId -> Player -> (Player, Maybe Bullet)
fireBullet events bulletId player =
    let Player (Circle position _) _ gun _ (PlayerId _ joystickId) = player
        Gun   aim reloadTime state     = gun
        Aim2D _   _          direction = aim
        newState                       = getGunState state joystickId events
        (newReloadTime, maybeBullet) = if reloadTime == 0 && newState == Firing
            then
                ( minFiringInterval
                , Just $ createBullet position direction bulletId
                )
            else (reloadTime, Nothing)
    in  (setGun (Gun aim newReloadTime newState) player, maybeBullet)

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
eventToGunState playerJoystickId event
    | isRightBumperPressed playerJoystickId event = Just Firing
    | isRightBumperReleased playerJoystickId event = Just Idle
    | otherwise = Nothing

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
