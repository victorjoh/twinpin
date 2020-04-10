module Player
    ( PlayerId(..)
    , Color(..)
    , Player(..)
    , Gun(..)
    , GunState(..)
    , Aim2D(..)
    , Vitality(..)
    , playerMaxHealth
    , Deaths
    , ReloadTime
    , playerSide
    , minFiringInterval
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
    , aimTopLeftCurve
    , aimBottomLeftCurve
    , aimBottomLine
    , aimBottomRightCurve
    , aimTopRightCurve
    , aimTopLine
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

type Direction1D = Float
-- Keep a 2D vector to cover the case when the aim stick is only moved in one
-- axis. Keep the rotation to cover the case when the aim stick is moved to the
-- default position (0, 0).
data Aim2D = Aim2D Direction1D Direction1D Angle2D deriving (Show, Eq)
type ReloadTime = DeltaTime
data GunState = Firing | Idle deriving (Show, Eq)
data Gun = Gun Aim2D ReloadTime GunState deriving (Show, Eq)
data Color = Red | Blue deriving (Show, Eq)
type Deaths = Int
data Vitality = Vitality Deaths Health deriving (Show, Eq)
data PlayerId = PlayerId Color (Maybe JoystickID) deriving (Show, Eq)
data Player = Player Circle Velocity2D Gun Vitality PlayerId deriving (Show, Eq)

playerMaxHealth :: Health
playerMaxHealth = 1.0

playerSide :: Size1D
playerSide = 60

playerRadius :: Radius
playerRadius = playerSide / 2

minFiringInterval :: ReloadTime
minFiringInterval = 250

axisPositionToVelocity :: Float
axisPositionToVelocity = 0.000018

triggerMinFireValue = 0
minAxisPosition = 5000
rightBumberButtonId = 5
rightTriggerButtonId = 5
baseColor = white
loadShadeColor = PixelRGBA8 0x16 0x0f 0x35 120
healthShadeColor = PixelRGBA8 0x48 0x2D 0x3B 120
boostColor = yellow
baseImageId = "playerBase"

playerSize :: Size2D
playerSize = V2 playerSide playerSide

createPlayer :: Position2D -> Angle2D -> Color -> Maybe JoystickID -> Player
createPlayer pos direction color joystickId = Player
    (Circle pos (playerSide / 2))
    (V2 0 0)
    (Gun (Aim2D 0 0 direction) 0 Idle)
    (Vitality 0 playerMaxHealth)
    (PlayerId color joystickId)

staticPlayerImage :: (ImageId, VectorImage)
staticPlayerImage = (baseImageId, toSolidCircleImage baseColor playerRadius)

drawPlayer :: Player -> [(Rectangle Float, Either VectorImage ImageId)]
drawPlayer (Player outline _ gun (Vitality _ health) (PlayerId colorId _)) =
    let Gun (Aim2D _ _ direction) reloadTime _ = gun
        center        = R.V2 playerRadius playerRadius
        textureArea   = toTextureArea outline
        diameter      = toDiameter playerRadius
        healthRadians = (1 - health) * (2 * pi)
        shadowLength  = reloadTimeToAimShadowLength (4 / 3) reloadTime
    in  [ (textureArea, Right baseImageId)
        , ( textureArea
          , Left
              $ VectorImage (V2 diameter diameter) transparent
              $ withTransformation (rotateCenter direction center)
              $ do
                    fillShape healthShadeColor $ circleSector healthRadians
                    fillShape (aimColor colorId) boostShape
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
aimColor Red  = red
aimColor Blue = blue

reloadTimeToAimShadowLength :: Float -> ReloadTime -> Float
reloadTimeToAimShadowLength maxShadowWidth reloadTime =
    maxShadowWidth * fromIntegral reloadTime / fromIntegral minFiringInterval

--                             , - ~ ~ ~ - ,
--                         , '               ' ,
--                       ,  ', boostTopLine      ,
--    boostTopLeftCurve ,     ',                  ,
--                     ,      ' boostTopRightCurve ,
--                     , - - '                     ,
--                     ,     ' boostBottomRightCurve
-- boostBottomLeftCurve ,     ',                  ,
--                       ,   .'                  ,
--                         ,' boostBottomLine , '
--                           ' - , _ _ _ ,  '

boostTopLeftCurve = snd $ breakCubicBezierAt circleQuadrant2 0.5
boostBottomLeftCurve = fst $ breakCubicBezierAt circleQuadrant3 0.5
boostTopRightCurve = transform (^* (2 / 3)) boostTopLeftCurve
boostBottomRightCurve = transform (^* (2 / 3)) boostBottomLeftCurve
boostTopLine =
    Line (_cBezierX0 boostTopLeftCurve) (_cBezierX0 boostTopRightCurve)
boostBottomLine =
    Line (_cBezierX3 boostBottomLeftCurve) (_cBezierX3 boostBottomRightCurve)

boostShape :: [Either CubicBezier Line]
boostShape =
    [ Left boostTopLeftCurve
    , Left boostBottomLeftCurve
    , Right boostBottomLine
    , Left boostBottomRightCurve
    , Left boostTopRightCurve
    , Right boostTopLine
    ]

--                  , - ~ ~ ~ - ,
--              , '               ' ,
--            ,            aimTopLine ,
--           ,          , ~ ~ ~ ~ ~ ~ ~,
--    aimTopLeftCurve '                 , aimTopRightCurve
--          ,        |  -  -  -  -  -  -,
-- aimBottomLeftCurve ,                 , aimBottomRightCurve
--           ,          ' ~ ~ ~ ~ ~ ~ ~,
--            ,          aimBottomLine,
--              ,                  , '
--                ' - , _ _ _ ,  '
aimRightCurveT = tan (1 / 3) * 2 / pi
aimTopRightCurve = fst $ breakCubicBezierAt circleQuadrant1 aimRightCurveT
aimBottomRightCurve =
    snd $ breakCubicBezierAt circleQuadrant4 (1 - aimRightCurveT)
aimBottomLine = Line (R.V2 0 (1 / 3)) (_cBezierX0 aimBottomRightCurve)
aimTopLine = Line (_cBezierX3 aimTopRightCurve) (R.V2 0 (-1 / 3))
aimTopLeftCurve = transform (^* (1 / 3)) circleQuadrant2
aimBottomLeftCurve = transform (^* (1 / 3)) circleQuadrant3

aimShape :: [Either CubicBezier Line]
aimShape =
    [ Left aimTopLeftCurve
    , Left aimBottomLeftCurve
    , Right aimBottomLine
    , Left aimBottomRightCurve
    , Left aimTopRightCurve
    , Right aimTopLine
    ]

aimShadowShape :: Float -> [Either CubicBezier Line]
aimShadowShape len
    | len <= 0
    = []
    | len > 0 && len < 1 / 3
    = let
          t                  = 2 / pi * acos (3 * len)
          cutTopLeftCurve    = snd $ breakCubicBezierAt aimTopLeftCurve (1 - t)
          cutBottomLeftCurve = fst $ breakCubicBezierAt aimBottomLeftCurve t
      in
          [ Left cutTopLeftCurve
          , Left cutBottomLeftCurve
          , Right $ Line (_cBezierX3 cutBottomLeftCurve)
                         (_cBezierX0 cutTopLeftCurve)
          ]
    | len >= 1 / 3 && len <= lineLength + 1 / 3
    = let t             = (len - 1 / 3) / lineLength
          cutBottomLine = fst $ breakLineAt aimBottomLine t
          cutTopLine    = snd $ breakLineAt aimTopLine (1 - t)
      in  [ Left aimTopLeftCurve
          , Left aimBottomLeftCurve
          , Right cutBottomLine
          , Right $ Line (_lineX1 cutBottomLine) (_lineX0 cutTopLine)
          , Right cutTopLine
          ]
    | len > lineLength + 1 / 3 && len < 4 / 3
    = let
          t = (len - lineLength - 1 / 3) / (1 - lineLength)
          cutBottomRightCurve = fst $ breakCubicBezierAt aimBottomRightCurve t
          cutTopRightCurve = snd $ breakCubicBezierAt aimTopRightCurve (1 - t)
      in
          [ Left aimTopLeftCurve
          , Left aimBottomLeftCurve
          , Right aimBottomLine
          , Left cutBottomRightCurve
          , Right $ Line (_cBezierX3 cutBottomRightCurve)
                         (_cBezierX0 cutTopRightCurve)
          , Left cutTopRightCurve
          , Right aimTopLine
          ]
    | otherwise
    = aimShape
  where
    getX (R.V2 x _) = x
    lineLength = getX $ _cBezierX0 aimBottomRightCurve

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
    let Player shape velocity gun vitality (PlayerId color maybeJoystickId)
            = player
        Gun aim reloadTime state = gun
        axisEvents               = case maybeJoystickId of
            Just joystickId ->
                map (joyAxisEventAxis &&& joyAxisEventValue)
                    $ filter ((joystickId ==) . joyAxisEventWhich)
                    $ mapMaybe toJoyAxis events
            Nothing -> []
        newVelocity   = foldl' updateVelocity velocity axisEvents
        newAim        = foldl' updateAim aim axisEvents
        newCircle     = moveCollidingCircle newVelocity dt obstacles shape
        newReloadTime = max 0 $ reloadTime - dt
    in  Player newCircle
               newVelocity
               (Gun newAim newReloadTime state)
               vitality
               (PlayerId color $ updateJoystick events =<< maybeJoystickId)

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
eventToGunState playerJoystickId (Event _ (JoyButtonEvent buttonEventData)) =
    let JoyButtonEventData joystickId buttonId buttonState = buttonEventData
    in  if joystickId == playerJoystickId && buttonId == rightBumberButtonId
            then case buttonState of
                JoyButtonPressed  -> Just Firing
                JoyButtonReleased -> Just Idle
            else Nothing
eventToGunState _ _ = Nothing

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
