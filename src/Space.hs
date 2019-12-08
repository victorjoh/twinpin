module Space where

import           SDL.Vect
import           Foreign.C.Types
import           Data.Tuple.Extra               ( both )

type Time = Int
type DeltaTime = Time

type Position1D = Float
type Vector1D = Float
type Velocity1D = Float
type Size1D = Float
type Bounds1D = (Position1D, Position1D)
type Shape1D = (Position1D, Position1D)
type Speed = Float

type Angle2D = Float
type Position2D = V2 Position1D
type Vector2D = V2 Vector1D
type Velocity2D = V2 Velocity1D
type Size2D = V2 Size1D
type LineSegment2D = (Position2D, Position2D)
data Bounds2D = Bounds2D Bounds1D Bounds1D deriving (Show, Eq)

-- on the form (a, b, c) where ax + by + c = 0
type Line2D = (Float, Float, Float)

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

createBounds :: Size1D -> Size1D -> Bounds2D
createBounds width height = Bounds2D (0, width) (0, height)

boundsToLines2D :: Bounds2D -> [Line2D]
boundsToLines2D (Bounds2D (xl, xu) (yl, yu)) =
    [(-1, 0, xl), (-1, 0, xu), (0, -1, yl), (0, -1, yu)]

offsetBounds2D :: Position2D -> Bounds2D -> Bounds2D
offsetBounds2D (V2 x y) (Bounds2D bx by) =
    Bounds2D (both (+ x) bx) (both (+ y) by)

isWithinBounds2D :: Position2D -> Bounds2D -> Bool
isWithinBounds2D (V2 px py) (Bounds2D bx by) =
    isWithinBounds1D px bx && isWithinBounds1D py by

isWithinBounds1D :: Position1D -> Bounds1D -> Bool
isWithinBounds1D position (lowerBound, upperBound) =
    position >= lowerBound && position <= upperBound

-- If the lines are the same or parallel, nothing is returned. Converted from:
-- https://cp-algorithms.com/geometry/lines-intersection.html
getLineIntersection2D :: Line2D -> Line2D -> Maybe Position2D
getLineIntersection2D (a1, b1, c1) (a2, b2, c2) =
    let determinant m1 m2 m3 m4 = m1 * m4 - m2 * m3
        zn = determinant a1 b1 a2 b2
    in  if abs zn < epsilon
            then Nothing
            else Just $ V2 (-(determinant c1 b1 c2 b2) / zn)
                           (-(determinant a1 c1 a2 c2) / zn)


getPositionClosestToOrigin :: Line2D -> Position2D
getPositionClosestToOrigin (a, b, c) = k @* (V2 a b)
    where k = -c / (a * a + b * b)

offsetLine2D :: Vector2D -> Line2D -> Line2D
offsetLine2D (V2 ox oy) (a, b, c) = (a, b, c - a * ox - b * oy)

offsetDistanceToOrigin2D :: Vector1D -> Line2D -> Line2D
offsetDistanceToOrigin2D d (a, b, c) =
    let z = sqrt (a * a + b * b)
    in  ( a
        , b
        , if d <= -abs c / z + epsilon
            then 0
            else signum c * abs (abs c + d * z)
        )

-- the angle between the vectors should be less than pi
isLineBetween2D :: Vector2D -> Vector2D -> Line2D -> Bool
isLineBetween2D v1 v2 l =
    isLineCrossingVector2D v1 l || isLineCrossingVector2D v2 l

isLineCrossingVector2D :: Vector2D -> Line2D -> Bool
isLineCrossingVector2D v l =
    let (V2 x y) = v
        signumV2 (V2 a b) = V2 (signum a) (signum b)
    in  maybe False
              ((signumV2 v ==) . signumV2)
              (getLineIntersection2D l (y, -x, 0))

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

getLine2D :: Position2D -> Position2D -> Line2D
getLine2D (V2 x1 y1) (V2 x2 y2) = (y1 - y2, x2 - x1, x1 * y2 - x2 * y1)

(@*) :: Num a => a -> V2 a -> V2 a
(@*) m = fmap (m *)

(/@) :: Fractional a => V2 a -> a -> V2 a
(/@) v m = fmap (\a -> a / m) v

epsilon :: Float
epsilon = 1.19209290e-07

angleDifference2D :: Angle2D -> Angle2D -> Angle2D
angleDifference2D a b =
    let diff = a - b
    in  diff + if (diff > pi)
            then -2 * pi
            else if (diff < -pi) then 2 * pi else 0

vectorToAngle :: Vector2D -> Angle2D
vectorToAngle (V2 x y) = atan2 y x

angleToVector :: Angle2D -> Vector2D
angleToVector a = V2 (cos a) (sin a)

toUnitVector :: Vector2D -> Vector2D
toUnitVector v = v /@ norm v

scalarProjection :: Vector2D -> Vector2D -> Float
scalarProjection v direction = v `dot` toUnitVector direction

flipToClosest :: Vector2D -> Vector2D -> Vector2D
flipToClosest v target = if target `dot` v > 0 then v else -v
