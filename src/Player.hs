module Player
    ( Player
    , initPlayer
    , drawPlayer
    , updatePlayer
    )
where

import           Time
import           SDL
import           Data.Word                      ( Word8
                                                , Word32
                                                )
import           Foreign.C.Types

type Position1D = Float
type Velocity1D = Float
data Position2D = Position2D Position1D Position1D deriving (Show)
data Velocity2D = Velocity2D Velocity1D Velocity1D deriving (Show)
data Player = Player Position2D Velocity2D deriving (Show)

initPlayer :: Player
initPlayer = (Player (Position2D 0 0) (Velocity2D 0 0))

drawPlayer :: Player -> [(V4 Word8, Maybe (Rectangle CInt))]
drawPlayer (Player (Position2D x y) _) =
    [ ( V4 maxBound maxBound maxBound maxBound
      , Just (Rectangle (P (V2 (round x) (round y))) (V2 10 10))
      )
    ]

updatePlayer :: Player -> [Event] -> DeltaTime -> Player
updatePlayer (Player p v) es td = Player (updatePosition2D p v' td) v'
    where v' = foldl updateVelocity v es

updateVelocity :: Velocity2D -> Event -> Velocity2D
updateVelocity (Velocity2D x y) (Event _ (JoyAxisEvent (JoyAxisEventData _ axis pos)))
    = let pos' = (0.00001 * fromIntegral pos)
      in  case axis of
              0 -> Velocity2D pos' y
              1 -> Velocity2D x pos'
              _ -> Velocity2D x y
updateVelocity v _ = v

updatePosition2D :: Position2D -> Velocity2D -> DeltaTime -> Position2D
updatePosition2D (Position2D x y) (Velocity2D x' y') dt =
    Position2D (updatePosition1D x x' dt) (updatePosition1D y y' dt)

updatePosition1D :: Position1D -> Velocity1D -> DeltaTime -> Position1D
updatePosition1D p v dt = p + v * fromIntegral dt
