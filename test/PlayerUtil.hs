module PlayerUtil where

import           Space
import           Circle
import           CircleUtil
import           Player
import           GHC.Int                        ( Int16 )
import           SDL.Event
import           Data.Word                      ( Word32 )
import           SDL.Raw.Types                  ( JoystickID )
import           SDL.Input.Joystick             ( JoyButtonState
                                                    ( JoyButtonPressed
                                                    )
                                                )

playerRadius :: Radius
playerRadius = playerSide / 2

getPlayerPosition :: Player -> Position2D
getPlayerPosition player = getCirclePosition $ playerToCircle player

getPlayerAngle :: Player -> Angle2D
getPlayerAngle (Player _ _ (Gun (Aim2D _ _ angle) _ _) _) = angle

getReloadTime :: Player -> ReloadTime
getReloadTime (Player _ _ (Gun _ reloadTime _) _) = reloadTime

getGun :: Player -> Gun
getGun (Player _ _ gun _) = gun

setReloadTime :: ReloadTime -> Player -> Player
setReloadTime reloadTime player =
    let Player _ _ (Gun aim _ state) _ = player
    in  setGun (Gun aim reloadTime state) player

setPlayerVelocity :: Velocity2D -> Player -> Player
setPlayerVelocity velocity (Player circle _ gun joystickId) =
    Player circle velocity gun joystickId

getRequiredStickPosition :: Vector1D -> Time -> Integer
getRequiredStickPosition distance time =
    round $ distance / (fromIntegral time * axisPositionToVelocity)

class EventContent c where
    toEventPayload :: c -> EventPayload
    toEvents :: [c] -> [Event]
    toEvents = map (Event 0 . toEventPayload)

instance EventContent JoyAxisEventData where
    toEventPayload = JoyAxisEvent

instance EventContent JoyButtonEventData where
    toEventPayload = JoyButtonEvent

createMoveRightEvent :: JoystickID -> Vector1D -> Word32 -> Event
createMoveRightEvent playerId distance time =
    let
        stickPos = getRequiredStickPosition distance $ fromIntegral time
        min      = minBound :: Int16
        max      = maxBound :: Int16
    in
        if stickPos < toInteger min || stickPos > toInteger max
            then error
                (  "The required stick position "
                ++ show stickPos
                ++ " is not within the possible Int16 range of ["
                ++ show min
                ++ ", "
                ++ show max
                ++ "]. the stick position has to be provided in Int16 for"
                ++ " JoyAxisEventData to accept it. Try using more time for the"
                ++ " movement."
                )
            else Event
                0
                (JoyAxisEvent
                    (JoyAxisEventData playerId 0 (fromInteger stickPos))
                )

createTriggerEvent :: JoystickID -> Event
createTriggerEvent id =
    Event 0 (JoyButtonEvent (JoyButtonEventData id 5 JoyButtonPressed))
