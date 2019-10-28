module Circle
    ( Circle(..)
    , toDrawableCircle
    , areIntersecting
    , isCircleWithinBounds
    , updateCirclePosition
    , updateCollidingCirclePosition
    )
where

import           Space
import           Foreign.C.Types
import           SDL.Video.Renderer             ( Rectangle(..) )
import           SDL.Vect
import           Data.Maybe

type Radius = Float
data Circle = Circle Position2D Radius deriving (Show, Eq)

-- Converts to something that is easily drawable by SDL. Circle has coordinates 
-- on the middle of the texture, whereas the SDL representation has the
-- coordinates specified on the top left corner.
toDrawableCircle
    :: Circle
    -> Angle2D -- texture rotation in clockwise degrees
    -> FilePath
    -> (FilePath, Maybe (Rectangle CInt), CDouble)
toDrawableCircle (Circle position radius) angle textureFile =
    ( textureFile
    , Just
        (Rectangle (toPixelPoint (position - (V2 radius radius)))
                   (toPixelSize $ boundingBoxSize radius)
        )
    , toPixelAngle angle
    )
    where radius' = (V2 radius radius)

boundingBoxSize :: Radius -> V2 Float
boundingBoxSize radius = V2 diameter diameter where diameter = radius * 2

areIntersecting :: Circle -> Circle -> Bool
areIntersecting (Circle (V2 x1 y1) r1) (Circle (V2 x2 y2) r2) =
    (r1 + r2) ^ 2 > (x2 - x1) ^ 2 + (y2 - y1) ^ 2

isCircleWithinBounds :: Bounds2D -> Circle -> Bool
isCircleWithinBounds bounds (Circle position radius) =
    isWithinBounds2D position $ increaseBounds2D bounds $ boundingBoxSize radius
    where diameter = radius * 2

updateCirclePosition :: Velocity2D -> DeltaTime -> Circle -> Circle
updateCirclePosition velocity dt (Circle oldPosition radius) =
    Circle (updatePosition2D oldPosition velocity dt) radius

updateCollidingCirclePosition
    :: Velocity2D -> DeltaTime -> Bounds2D -> [Circle] -> Circle -> Circle
updateCollidingCirclePosition velocity dt bounds obstacles (Circle oldPosition radius)
    = Circle
        (limitPosition2D
            (collideWithCircles obstacles
                                radius
                                oldPosition
                                (updatePosition2D oldPosition velocity dt)
            )
            (decreaseBounds2D bounds $ boundingBoxSize radius)
        )
        radius

collideWithCircles
    :: [Circle] -> Radius -> Position2D -> Position2D -> Position2D
collideWithCircles obstacles radius oldPosition targetPosition =
    foldr (getClosestTo2D oldPosition) targetPosition $ concat $ map
        (findCollisionPoints radius oldPosition targetPosition)
        (filter (isInFrontOf oldPosition targetPosition) obstacles)

isInFrontOf :: Position2D -> Position2D -> Circle -> Bool
isInFrontOf base target (Circle obstacle _) =
    (target - base) `dot` (obstacle - base) > 0

findCollisionPoints
    :: Radius -> Position2D -> Position2D -> Circle -> [Position2D]
findCollisionPoints radius oldPos targetPos (Circle obstaclePos obstacleRadius)
    = map
        (+ obstaclePos)
        (findCircleLineIntersections
            (radius + obstacleRadius)
            (getLine2D (oldPos - obstaclePos) (targetPos - obstaclePos))
        )

-- the circle is centered in origin
findCircleLineIntersections :: Radius -> Line2D -> [Position2D]
findCircleLineIntersections r (a, b, c) =
    let x0 = -a * c / (a * a + b * b)
        y0 = -b * c / (a * a + b * b)
    in  if (c * c > r * r * (a * a + b * b) + epsilon)
            then []
            else if (abs (c * c - r * r * (a * a + b * b)) < epsilon)
                then [V2 x0 y0]
                else
                    let d    = r * r - c * c / (a * a + b * b)
                        mult = sqrt (d / (a * a + b * b))
                        ax   = x0 + b * mult
                        bx   = x0 - b * mult
                        ay   = y0 - a * mult
                        by   = y0 + a * mult
                    in  [V2 ax ay, V2 bx by]
