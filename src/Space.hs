module Space where

import           SDL.Vect
import           Foreign.C.Types

type Time = Int
type DeltaTime = Time

type Position1D = Float
type Velocity1D = Float
type Size1D = Float
type Bounds1D = (Position1D, Position1D)
type Shape1D = (Position1D, Position1D)
type Speed = Float

type Angle2D = Float
type Position2D = V2 Position1D
type Velocity2D = V2 Velocity1D
type Size2D = V2 Size1D
data Bounds2D = Bounds2D Bounds1D Bounds1D deriving (Show, Eq)

toVelocity :: Angle2D -> Speed -> Velocity2D
toVelocity angle speed = V2 (axisVelocity cos) (axisVelocity sin)
    where axisVelocity trig = (speed * (trig ((pi / 180) * angle)))

toPixelPoint :: Position2D -> Point V2 CInt
toPixelPoint (V2 x y) = P $ V2 (round x) (round y)

toPixelSize :: Size2D -> V2 CInt
toPixelSize (V2 x y) = V2 (round x) (round y)

toPixelAngle :: Angle2D -> CDouble
toPixelAngle = realToFrac

updatePosition2D :: Position2D -> Velocity2D -> DeltaTime -> Position2D
updatePosition2D position velocity dt = position + velocity * fromIntegral dt

isWithinBounds2D :: Position2D -> Bounds2D -> Bool
isWithinBounds2D (V2 px py) (Bounds2D bx by) =
    isWithinBounds1D px bx && isWithinBounds1D py by

isWithinBounds1D :: Position1D -> Bounds1D -> Bool
isWithinBounds1D position (lowerBound, upperBound) =
    position >= lowerBound && position <= upperBound

limitPosition2D :: Position2D -> Bounds2D -> Position2D
limitPosition2D (V2 px py) (Bounds2D bx by) =
    V2 (limitPosition1D px bx) (limitPosition1D py by)

limitPosition1D :: Position1D -> Bounds1D -> Position1D
limitPosition1D position (lowerBound, upperBound) =
    max lowerBound $ min upperBound position

increaseBounds2D :: Bounds2D -> Size2D -> Bounds2D
increaseBounds2D (Bounds2D bx by) (V2 sx sy) =
    Bounds2D (increaseBounds1D bx sx) (increaseBounds1D by sy)

increaseBounds1D :: Bounds1D -> Size1D -> Bounds1D
increaseBounds1D (lower, upper) size = (lower - size / 2, upper + size / 2)

decreaseBounds2D :: Bounds2D -> Size2D -> Bounds2D
decreaseBounds2D bounds size = increaseBounds2D bounds (-size)

decreaseBounds1D :: Bounds1D -> Size1D -> Bounds1D
decreaseBounds1D bounds size = increaseBounds1D bounds (-size)
