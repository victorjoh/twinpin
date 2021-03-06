module PlayerUtil where

import Bullet
import Circle
import CircleUtil
import Data.Word
  ( Word32,
    Word8,
  )
import GHC.Int (Int16)
import Player
import SDL.Event
import SDL.Input.Joystick
  ( JoyButtonState
      ( JoyButtonPressed
      ),
  )
import SDL.Raw.Types (JoystickID)
import Space

playerRadius :: Radius
playerRadius = playerSide / 2

getPlayerPosition :: Player -> Position2D
getPlayerPosition player = getCirclePosition $ playerToCircle player

getPlayerAngle :: Player -> Angle2D
getPlayerAngle (Player _ _ (Gun (Aim2D _ _ angle) _ _) _ _) = angle

getReloadTime :: Player -> ReloadTime
getReloadTime (Player _ _ (Gun _ reloadTime _) _ _) = reloadTime

getGun :: Player -> Gun
getGun (Player _ _ gun _ _) = gun

getGunState :: Player -> GunState
getGunState player = let (Gun _ _ gunState) = getGun player in gunState

setReloadTime :: ReloadTime -> Player -> Player
setReloadTime reloadTime player =
  let Player _ _ (Gun aim _ state) _ _ = player
   in setGun (Gun aim reloadTime state) player

setPlayerVelocity :: Velocity2D -> Player -> Player
setPlayerVelocity velocity (Player circle movement gun vitality playerId) =
  Player circle (setMovementVelocity velocity movement) gun vitality playerId

setPlayerBoostTime :: BoostTime -> Player -> Player
setPlayerBoostTime boostTime (Player circle movement gun vitality playerId) =
  Player circle newMovement gun vitality playerId
  where
    newMovement = setMovementBoostTime boostTime movement

setMovement :: Movement -> Player -> Player
setMovement movement (Player circle _ gun vitality playerId) =
  Player circle movement gun vitality playerId

setMovementVelocity :: Velocity2D -> Movement -> Movement
setMovementVelocity velocity (Movement _ _ boostTime) =
  Movement velocity velocity boostTime

setMovementBoostTime :: BoostTime -> Movement -> Movement
setMovementBoostTime boostTime (Movement velocity direction _) =
  Movement velocity direction boostTime

getBoostTime :: Player -> BoostTime
getBoostTime (Player _ (Movement _ _ boostTime) _ _ _) = boostTime

getRequiredStickPosition :: Vector1D -> Time -> Integer
getRequiredStickPosition distance time =
  round $ distance / (fromIntegral time * axisPositionToVelocity)

class EventContent c where
  toEventPayload :: c -> EventPayload
  toEvent :: c -> Event
  toEvent = Event 0 . toEventPayload
  toEvents :: [c] -> [Event]
  toEvents = map toEvent

instance EventContent JoyAxisEventData where
  toEventPayload = JoyAxisEvent

instance EventContent JoyButtonEventData where
  toEventPayload = JoyButtonEvent

instance EventContent JoyHatEventData where
  toEventPayload = JoyHatEvent

instance EventContent KeyboardEventData where
  toEventPayload = KeyboardEvent

instance EventContent JoyDeviceEventData where
  toEventPayload = JoyDeviceEvent

createMoveRightEvent :: JoystickID -> Vector1D -> Word32 -> Event
createMoveRightEvent = createMoveEvent 0

createMoveDownEvent :: JoystickID -> Vector1D -> Word32 -> Event
createMoveDownEvent = createMoveEvent 1

createMoveEvent :: Word8 -> JoystickID -> Vector1D -> Word32 -> Event
createMoveEvent direction playerId distance time =
  let stickPos = getRequiredStickPosition distance $ fromIntegral time
      minPos = minBound :: Int16
      maxPos = maxBound :: Int16
   in if stickPos < toInteger minPos || stickPos > toInteger maxPos
        then
          error
            ( "The required stick position "
                ++ show stickPos
                ++ " is not within the possible Int16 range of ["
                ++ show minPos
                ++ ", "
                ++ show maxPos
                ++ "]. the stick position has to be provided in Int16 for"
                ++ " JoyAxisEventData to accept it. Try using more time for the"
                ++ " movement."
            )
        else
          toEvent $
            JoyAxisEventData playerId direction (fromInteger stickPos)

createTriggerEvent :: JoystickID -> JoyButtonState -> Event
createTriggerEvent joystickId state =
  toEvent $ JoyButtonEventData joystickId 5 state

type ButtonID = Word8

createButtonPressedEvent :: JoystickID -> ButtonID -> Event
createButtonPressedEvent joystickId buttonId =
  Event
    0
    (JoyButtonEvent (JoyButtonEventData joystickId buttonId JoyButtonPressed))

getJoystickId :: Player -> Maybe JoystickID
getJoystickId (Player _ _ _ _ (PlayerId _ joystickId)) = joystickId

getHealth :: Player -> Health
getHealth (Player _ _ _ (Vitality _ health) _) = health

setHealth :: Health -> Player -> Player
setHealth newHealth (Player circle velocity gun vitality playerId) =
  let Vitality deaths _ = vitality
   in Player circle velocity gun (Vitality deaths newHealth) playerId

setDeaths :: Deaths -> Player -> Player
setDeaths newDeaths (Player circle velocity gun vitality playerId) =
  let Vitality _ health = vitality
   in Player circle velocity gun (Vitality newDeaths health) playerId
