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
import           Graphics.Rasterific.Linear     ( (^+^)
                                                , (^*)
                                                , lerp
                                                )
import           Codec.Picture.Types
import           Data.Word                      ( Word8 )
import           Data.Int                       ( Int16 )
import           SDL.Raw.Types                  ( JoystickID )
import           Data.Maybe
import           Data.Tuple.Extra               ( (&&&) )
import           Data.List                      ( foldl' )
import           Data.Bifoldable                ( biList )
import           Data.Bifunctor                 ( bimap
                                                , first
                                                )

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
baseColor = PixelRGBA8 0xE6 0xE6 0xE6 255
loadShadeColor = PixelRGBA8 0x16 0x0f 0x35 120
healthShadeColor = PixelRGBA8 0x48 0x2D 0x3B 120
baseImageId = "playerBase"

playerSize :: Size2D
playerSize = V2 playerSide playerSide

createPlayer :: Position2D -> Angle2D -> Color -> Maybe JoystickID -> Player
createPlayer pos direction color joystickId = Player
    (Circle pos (playerSide / 2))
    (V2 0 0)
    (Gun (Aim2D 0 0 direction) 0 Firing)
    (Vitality 0 playerMaxHealth)
    (PlayerId color joystickId)

staticPlayerImage :: (ImageId, VectorImage)
staticPlayerImage = (baseImageId, toSolidCircleImage baseColor playerRadius)

drawPlayer :: Player -> [(Rectangle Float, Either VectorImage ImageId)]
drawPlayer (Player shape _ gun (Vitality _ health) (PlayerId color _)) =
    let Circle _ radius = shape
        Gun (Aim2D _ _ direction) reloadTime _ = gun
        center          = R.V2 radius radius
        textureArea     = toTextureArea shape
        diameter        = toDiameter radius
        healthRadians   = (1 - health) * (2 * pi)
        aimShape'       = fill $ aimShape center radius
        shadowLength = reloadTimeToAimShadowLength (radius * 4 / 3) reloadTime
    in  [ (textureArea, Right baseImageId)
        , ( textureArea
          , Left
              $ VectorImage (V2 diameter diameter) transparent
              $ withTransformation (rotateCenter direction center)
              $ do
                    withTexture (uniformTexture healthShadeColor)
                        $ fill
                        $ circleSector center radius healthRadians
                    withTexture (uniformTexture $ aimColor color) aimShape'
                    withTexture (uniformTexture loadShadeColor)
                        $ fill
                        $ aimShadowShape center radius shadowLength
          )
        ]

cubicBezierAimShadow :: Float -> [Either CubicBezier Line]
cubicBezierAimShadow len
    | len <= 0
    = []
    | len > 0 && len < 1 / 3
    = let
          t       = len * 3
          topLeft = snd $ cubicBezierBreakAt
              (transform (^* (1 / 3)) circleQuadrant2)
              (1 - t)
          bottomRight = fst $ cubicBezierBreakAt
              (transform (^* (1 / 3)) circleQuadrant3)
              t
      in
          [ Left topLeft
          , Left bottomRight
          , Right $ Line (_cBezierX3 bottomRight) (_cBezierX0 topLeft)
          ]
    | len >= 1 / 3 && len <= getX (_cBezierX0 outerTop) + 1 / 3
    = let t             = (len - 1 / 3) / getX (_cBezierX0 outerTop)
          cutBottomLine = fst $ lineBreakAt bottomLine t
          cutTopLine    = snd $ lineBreakAt topLine (1 - t)
      in  map (first $ transform (^* (1 / 3)))
              [Left circleQuadrant2, Left circleQuadrant3]
              ++ [ Right cutBottomLine
                 , Right $ Line (_lineX1 cutBottomLine) (_lineX0 cutTopLine)
                 , Right cutTopLine
                 ]
    | len > getX (_cBezierX0 outerTop) + 1 / 3 && len < 4 / 3
    = []
    | otherwise
    = cubicBezierAim
    where getX (R.V2 x _) = x

outerT = tan (1 / 3) * 2 / pi
outerTop = fst $ cubicBezierBreakAt circleQuadrant1 outerT
outerBottom = snd $ cubicBezierBreakAt circleQuadrant4 (1 - outerT)
bottomLine = Line (R.V2 0 (1 / 3)) (_cBezierX0 outerBottom)
topLine = Line (_cBezierX3 outerTop) (R.V2 0 (-1 / 3))

cubicBezierAim :: [Either CubicBezier Line]
cubicBezierAim =
    map (first $ transform (^* (1 / 3)))
        [Left circleQuadrant2, Left circleQuadrant3]
        ++ [Right bottomLine, Left outerBottom, Left outerTop, Right topLine]

circleQuadrant1, circleQuadrant2, circleQuadrant3, circleQuadrant4
    :: CubicBezier
circleQuadrant1 =
    CubicBezier (R.V2 1 0) (R.V2 1 (-cc)) (R.V2 cc (-1)) (R.V2 0 (-1))
circleQuadrant2 =
    CubicBezier (R.V2 0 (-1)) (R.V2 (-cc) (-1)) (R.V2 (-1) (-cc)) (R.V2 (-1) 0)
circleQuadrant3 =
    CubicBezier (R.V2 (-1) 0) (R.V2 (-1) cc) (R.V2 (-cc) 1) (R.V2 0 1)
circleQuadrant4 = CubicBezier (R.V2 0 1) (R.V2 cc 1) (R.V2 1 cc) (R.V2 1 0)
cc = 0.551915024494

-- copied from Graphics.Rasterific.CubicBezier since it is in an unexposed
-- module
-- https://github.com/Twinside/Rasterific/blob/d607a5916a840c173c4a6c60f52c7e1a1533544e/src/Graphics/Rasterific/CubicBezier.hs#L260
cubicBezierBreakAt :: CubicBezier -> Float -> (CubicBezier, CubicBezier)
cubicBezierBreakAt (CubicBezier a b c d) val =
    (CubicBezier a ab abbc abbcbccd, CubicBezier abbcbccd bccd cd d)
  where
    ab       = lerp val b a
    bc       = lerp val c b
    cd       = lerp val d c

    abbc     = lerp val bc ab
    bccd     = lerp val cd bc
    abbcbccd = lerp val bccd abbc

lineBreakAt :: Line -> Float -> (Line, Line)
lineBreakAt (Line a b) t = (Line a ab, Line ab b) where ab = lerp t b a

cubicBezierCircleSector :: Angle2D -> ([CubicBezier], [Line])
cubicBezierCircleSector a
    | a <= 0
    = ([], [])
    | a > 0 && a < 2 * pi
    = let nbrOfQuadrants = ceiling $ a * 2 / pi
          quadrants      = drop (4 - nbrOfQuadrants) bezierCircle
          cutQuadrant    = fst $ cubicBezierBreakAt
              (head quadrants)
              ((a - fromIntegral (nbrOfQuadrants - 1) * pi / 2) * 2 / pi)
          orig = R.V2 0 0
      in  ( cutQuadrant : tail quadrants
          , [Line orig (R.V2 1 0), Line (_cBezierX3 cutQuadrant) orig]
          )
    | otherwise
    = (bezierCircle, [])
  where
    bezierCircle =
        [circleQuadrant4, circleQuadrant3, circleQuadrant2, circleQuadrant1]

aimShape :: R.V2 Float -> Radius -> [Primitive]
aimShape center radius = map
    (either (CubicBezierPrim . transform mv) (LinePrim . transform mv))
    cubicBezierAim
    where mv p = (p ^* radius) ^+^ center

aimShadowShape :: R.V2 Float -> Radius -> Position1D -> [Primitive]
aimShadowShape center radius len = map
    (either (CubicBezierPrim . transform mv) (LinePrim . transform mv))
    (cubicBezierAimShadow (len / radius))
    where mv p = (p ^* radius) ^+^ center

circleSector :: R.V2 Float -> Radius -> Angle2D -> [Primitive]
circleSector center radius a =
    concat
        $ biList
        $ bimap (fmap $ CubicBezierPrim . transform mv)
                (fmap $ LinePrim . transform mv)
        $ cubicBezierCircleSector a
    where mv p = (p ^* radius) ^+^ center

reloadTimeToAimShadowLength :: Float -> ReloadTime -> Float
reloadTimeToAimShadowLength maxShadowWidth reloadTime =
    maxShadowWidth * fromIntegral reloadTime / fromIntegral minFiringInterval

aimColor :: Color -> PixelRGBA8
aimColor Red  = PixelRGBA8 0xD3 0x5F 0x5F 255
aimColor Blue = PixelRGBA8 0x5F 0x5F 0xD3 255

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
