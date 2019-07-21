module Model where

import           SDL.Event
import           Data.Word                      ( Word32 )

type Time = Int
type DeltaTime = Time
type Position1D = Float
type Velocity1D = Float
data Position2D = Position2D Position1D Position1D deriving (Show)
data Velocity2D = Velocity2D Velocity1D Velocity1D deriving (Show)
data Player = Player Position2D Velocity2D deriving (Show)
data Model = Model Time Player deriving (Show)

initModel :: Model
initModel = Model 0 (Player (Position2D 0 0) (Velocity2D 0 0))

updateModel :: Model -> [Event] -> Word32 -> Model
updateModel (Model t p) es tw' = Model t' $ updatePlayer p es (t' - t)
    where t' = fromIntegral tw'

updatePlayer :: Player -> [Event] -> DeltaTime -> Player
updatePlayer (Player p v) es td = Player (updatePosition2D p v' td) v'
    where v' = foldl updateVelocity v es

updateVelocity :: Velocity2D -> Event -> Velocity2D
updateVelocity (Velocity2D x y) (Event _ (JoyAxisEvent (JoyAxisEventData _ a p)))
    = let p' = (0.00001 * fromIntegral p)
      in  case a of
              0 -> Velocity2D p' y
              1 -> Velocity2D x p'
updateVelocity v _ = v

updatePosition2D :: Position2D -> Velocity2D -> DeltaTime -> Position2D
updatePosition2D (Position2D x y) (Velocity2D x' y') dt =
    Position2D (updatePosition1D x x' dt) (updatePosition1D y y' dt)

updatePosition1D :: Position1D -> Velocity1D -> DeltaTime -> Position1D
updatePosition1D p v dt = p + v * fromIntegral dt
