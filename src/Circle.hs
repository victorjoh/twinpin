module Circle
    ( Circle(..)
    , Radius
    , toTextureArea
    , toDrawing
    , toDiameter
    , toSolidCircleImage
    , areIntersecting
    , isCircleWithinBounds
    , moveCircle
    , moveCollidingCircle
    , Obstacles(..)
    )
where

import           Space
import           Visual
import           SDL.Video.Renderer             ( Rectangle(..) )
import           SDL.Vect
import           Graphics.Rasterific     hiding ( V2(..)
                                                , circle
                                                )
import qualified Graphics.Rasterific           as Rasterific
                                                ( V2(..)
                                                , circle
                                                )
import           Graphics.Rasterific.Texture
import           Codec.Picture.Types
import           Data.Maybe
import           Data.Bifunctor                 ( first
                                                , second
                                                , bimap
                                                )
import           Data.List                      ( delete )
import           Relude.Extra.Tuple             ( traverseToFst )
import           Data.Function                  ( (&) )

type Radius = Float
type Diameter = Float
data Circle = Circle Position2D Radius deriving (Show, Eq)
data Waypoint = Waypoint WaypointType Position2D deriving (Show)
data WaypointType = CircleCollision Circle | BoundsCollision | MovementFinished
                    deriving (Show)
data Obstacles = Obstacles Bounds2D [Circle] deriving (Show, Eq)

class Movement m where
    getStartPosition, getEndPosition :: m -> Position2D
    collideWithBounds :: Radius -> m -> Bounds2D -> Maybe Waypoint
    collideWithCircles :: Radius -> m -> [Circle] -> Maybe Waypoint

type CircleIntersection = (Position2D, Circle)
type TrajectoryInstant = (Position2D, Direction2D)
type TrajectoryStart = TrajectoryInstant
type TrajectoryEnd = TrajectoryInstant
type CirclePath = Circle
data CircularMovement = CircularMovement TrajectoryStart
                                         TrajectoryEnd
                                         CirclePath deriving (Show)
data StraightMovement = StraightMovement Position2D Position2D deriving (Show)

toSolidCircleImage :: PixelRGBA8 -> Radius -> VectorImage
toSolidCircleImage color radius =
    VectorImage (V2 diameter diameter) transparent
        $ withTexture (uniformTexture color)
        $ toDrawing radius
    where diameter = toDiameter radius

toDrawing :: Radius -> Drawing px ()
toDrawing radius =
    fill $ Rasterific.circle (Rasterific.V2 radius radius) radius

toDiameter :: Radius -> Diameter
toDiameter = (*) 2

toTextureArea :: Circle -> Rectangle Float
toTextureArea (Circle position radius) =
    Rectangle (P $ position - V2 radius radius) (boundingBoxSize radius)

boundingBoxSize :: Radius -> V2 Float
boundingBoxSize radius = V2 diameter diameter where diameter = radius * 2

areIntersecting :: Circle -> Circle -> Bool
areIntersecting (Circle (V2 x1 y1) r1) (Circle (V2 x2 y2) r2) =
    (r1 + r2) ^ 2 > (x2 - x1) ^ 2 + (y2 - y1) ^ 2

isCircleWithinBounds :: Bounds2D -> Circle -> Bool
isCircleWithinBounds bounds (Circle position radius) =
    isWithinBounds2D position $ increaseBounds2D bounds $ boundingBoxSize radius

moveCircle :: Velocity2D -> DeltaTime -> Circle -> Circle
moveCircle velocity dt (Circle oldPosition radius) =
    Circle (updatePosition2D oldPosition velocity dt) radius

moveCollidingCircle :: Velocity2D -> DeltaTime -> Obstacles -> Circle -> Circle
moveCollidingCircle velocity dt obstacles (Circle startPosition radius) =
    let endPosition = updatePosition2D startPosition velocity dt
        movement    = StraightMovement startPosition endPosition
    in  Circle (moveFreely radius movement obstacles) radius

increaseRadius :: Float -> Circle -> Circle
increaseRadius increase (Circle position radius) =
    Circle position (radius + increase)

decreaseRadius :: Float -> Circle -> Circle
decreaseRadius decrease (Circle position radius) =
    Circle position (radius - decrease)

moveFreely :: Radius -> StraightMovement -> Obstacles -> Position2D
moveFreely radiusInMotion movement obstacles = moveFromWaypoint
    radiusInMotion
    (getNextWaypoint radiusInMotion movement obstacles)
    (getEndPosition movement)
    obstacles

moveFromWaypoint :: Radius -> Waypoint -> Position2D -> Obstacles -> Position2D
moveFromWaypoint radiusInMotion waypoint endPosition obstacles =
    case waypointType of
        BoundsCollision -> moveAlongBounds radiusInMotion movement obstacles
        CircleCollision collidedWith ->
            moveAlongCircle radiusInMotion movement collidedWith obstacles
        MovementFinished -> waypointPosition
  where
    Waypoint waypointType waypointPosition = waypoint
    movement = StraightMovement waypointPosition endPosition

moveAlongBounds :: Radius -> StraightMovement -> Obstacles -> Position2D
moveAlongBounds radiusInMotion movement (Obstacles bounds circles) =
    let centerBounds = decreaseBounds2D bounds $ boundingBoxSize radiusInMotion
        newEndPosition = limitPosition2D (getEndPosition movement) centerBounds
    in  maybe
            newEndPosition
            (\(Waypoint _ position) -> position)
            (collideWithCircles
                radiusInMotion
                (StraightMovement (getStartPosition movement) newEndPosition)
                circles
            )

moveAlongCircle
    :: Radius -> StraightMovement -> Circle -> Obstacles -> Position2D
moveAlongCircle radiusInMotion movement touchingCircle obstacles
    | abs (startDirection `dot` escapeDirection) < epsilon
    = startPosition
    | escapeAngleDistance > possibleAngleDistance
    = let
          endAngle =
              startAngle'
                  - possibleAngleDistance
                  * signum escapeAngleDisplacement
          endPosition =
              trajectoryRadius @* angleToVector endAngle + trajectoryCenter
          circularMovement = CircularMovement
              (startPosition, startDirection)
              (endPosition  , escapeDirection)
              trajectory
      in
          endMovementOnCircle radiusInMotion
                              circularMovement
                              touchingCircle
                              obstacles
    | otherwise
    = let
          escapePos        = escapePos' + trajectoryCenter
          circularMovement = CircularMovement
              (startPosition, startDirection)
              (escapePos    , escapeDirection)
              trajectory
          distanceMovedOnCircle = escapeAngleDistance * trajectoryRadius
          movementDistanceLeft =
              possibleDistanceOnCircle - distanceMovedOnCircle
          endPosition = escapePos + movementDistanceLeft @* escapeDirectionUnit
      in
          movePastCircle radiusInMotion
                         circularMovement
                         touchingCircle
                         endPosition
                         obstacles
  where
    startPosition       = getStartPosition movement
    trajectory          = increaseRadius radiusInMotion touchingCircle
    (Circle trajectoryCenter trajectoryRadius) = trajectory
    escapeDirection     = getEndPosition movement - startPosition
    startPosition'      = startPosition - trajectoryCenter
    startDirection      = startPosition' `rotate90Towards` escapeDirection
    startAngle'         = vectorToAngle startPosition'
    escapeDirectionUnit = toUnitVector escapeDirection
    escapePos' =
        trajectoryRadius @* escapeDirectionUnit `rotate90Towards` startDirection
    escapeAngle'             = vectorToAngle escapePos'
    escapeAngleDisplacement  = startAngle' `angleDifference2D` escapeAngle'
    escapeAngleDistance      = abs escapeAngleDisplacement
    possibleDistanceOnCircle = scalarProjection escapeDirection startDirection
    possibleAngleDistance    = possibleDistanceOnCircle / trajectoryRadius

endMovementOnCircle
    :: Radius -> CircularMovement -> Circle -> Obstacles -> Position2D
endMovementOnCircle radiusInMotion movement touchedCircle obstacles =
    let otherObstacles = removeFromObstacles touchedCircle obstacles
        waypoint = getNextWaypoint radiusInMotion movement otherObstacles
        Waypoint _ waypointPosition = waypoint
    in  waypointPosition

movePastCircle
    :: Radius
    -> CircularMovement
    -> Circle
    -> Position2D
    -> Obstacles
    -> Position2D
movePastCircle radiusInMotion movement touchedCircle endPosition obstacles =
    let otherObstacles = removeFromObstacles touchedCircle obstacles
        waypoint = getNextWaypoint radiusInMotion movement otherObstacles
        Waypoint waypointType waypointPosition = waypoint
    in  case waypointType of
            MovementFinished ->
                let CircularMovement _ (exit, _) _ = movement
                    exitMovement = StraightMovement exit endPosition
                in  moveFromWaypoint
                        radiusInMotion
                        (getNextWaypoint radiusInMotion
                                         exitMovement
                                         otherObstacles
                        )
                        endPosition
                        obstacles
            _ -> waypointPosition

removeFromObstacles :: Circle -> Obstacles -> Obstacles
removeFromObstacles circle (Obstacles bounds circles) =
    Obstacles bounds (delete circle circles)

getNextWaypoint :: Movement m => Radius -> m -> Obstacles -> Waypoint
getNextWaypoint radiusInMotion movement (Obstacles bounds circles) = foldr
    (getClosestWaypoint (getStartPosition movement))
    (Waypoint MovementFinished (getEndPosition movement))
    (catMaybes
        [ collideWithBounds radiusInMotion movement bounds
        , collideWithCircles radiusInMotion movement circles
        ]
    )

getClosestWaypoint :: Position2D -> Waypoint -> Waypoint -> Waypoint
getClosestWaypoint reference w1 w2 =
    let (Waypoint _ p1) = w1
        (Waypoint _ p2) = w2
    in  if distance reference p1 < distance reference p2 then w1 else w2

instance Movement StraightMovement where
    getStartPosition (StraightMovement startPosition _) = startPosition
    getEndPosition (StraightMovement _ endPosition) = endPosition

    collideWithBounds radiusInMotion movement bounds =
        bounds
            & offsetBounds2D (-startPosition)
            & boundsToLines2D
            & filter (isLineCrossingVector2D $ getEndPosition movement')
            & map (offsetDistanceToOrigin2D (-radiusInMotion))
            & mapMaybe (getLineIntersection2D $ movementToLine2D movement')
            & foldr (getClosestPositionTo (V2 0 0)) Nothing
            & fmap (+ startPosition)
            & fmap (Waypoint BoundsCollision)
      where
        startPosition = getStartPosition movement
        movement'     = offsetStraightMovement (-startPosition) movement

    collideWithCircles radiusInMotion movement circles =
        circles
            & map (increaseRadius radiusInMotion)
            & filter (isInFrontOf movement)
            & concatMap (getMovementCircleIntersections movement)
            & foldr (getClosestCircleIntersectionTo startPosition) Nothing
            & fmap (second (decreaseRadius radiusInMotion))
            & fmap intersectionToWaypoint
        where startPosition = getStartPosition movement

isInFrontOf :: StraightMovement -> Circle -> Bool
isInFrontOf (StraightMovement from to) (Circle obstacle _) =
    (to - from) `dot` (obstacle - from) > epsilon

intersectionToWaypoint :: CircleIntersection -> Waypoint
intersectionToWaypoint (position, circle) =
    Waypoint (CircleCollision circle) position

getMovementCircleIntersections
    :: StraightMovement -> Circle -> [CircleIntersection]
getMovementCircleIntersections movement =
    traverseToFst $ getMovementCircleIntersections' movement

getMovementCircleIntersections' :: StraightMovement -> Circle -> [Position2D]
getMovementCircleIntersections' movement circle =
    movement
        & offsetStraightMovement (-circlePosition)
        & movementToLine2D
        & getCircleLineIntersections circleRadius
        & map (+ circlePosition)
        & filter (not . isFirstPositionCloserTo startPosition endPosition)
  where
    StraightMovement startPosition  endPosition  = movement
    Circle           circlePosition circleRadius = circle

offsetStraightMovement :: Vector2D -> StraightMovement -> StraightMovement
offsetStraightMovement offset (StraightMovement startPosition endPosition) =
    StraightMovement (startPosition + offset) (endPosition + offset)

movementToLine2D :: StraightMovement -> Line2D
movementToLine2D (StraightMovement startPosition endPosition) =
    getLine2D startPosition endPosition

instance Movement CircularMovement where
    getStartPosition (CircularMovement (startPosition, _) _ _) = startPosition
    getEndPosition (CircularMovement _ (endPosition, _) _) = endPosition

    collideWithBounds radiusInMotion movement bounds =
        bounds
            & offsetBounds2D (-trajectoryCenter)
            & boundsToLines2D
            & filter (isLineCollisionPossible start' end')
            & map (offsetDistanceToOrigin2D (-radiusInMotion))
            & concatMap (getCircleLineIntersections trajectoryRadius)
            & foldr (getClosestPositionTo startPosition') Nothing
            & fmap (+ trajectoryCenter)
            & fmap (Waypoint BoundsCollision)
      where
        CircularMovement start end trajectory = movement
        start'              = offsetTrajectoryInstant (-trajectoryCenter) start
        end'                = offsetTrajectoryInstant (-trajectoryCenter) end
        (startPosition', _) = start'
        (Circle trajectoryCenter trajectoryRadius) = trajectory

    collideWithCircles radiusInMotion movement circles =
        circles
            & map (offsetCircle (-trajectoryCenter) radiusInMotion)
            & filter (isCircleCollisionPossible start')
            & concatMap (getCircleIntersections trajectoryRadius)
            & foldr (getClosestCircleIntersectionTo startPosition') Nothing
            & fmap (offsetIntersection trajectoryCenter (-radiusInMotion))
            & fmap intersectionToWaypoint
      where
        CircularMovement start _ trajectory = movement
        Circle trajectoryCenter trajectoryRadius = trajectory
        start' = offsetTrajectoryInstant (-trajectoryCenter) start
        (startPosition', _)                 = start'

-- all positions are relative to the trajectory center
isLineCollisionPossible :: TrajectoryStart -> TrajectoryEnd -> Line2D -> Bool
isLineCollisionPossible (startPosition', startDirection) (_, endDirection) =
    isLineBetween2D startDirection endDirection . offsetLine2D (-startPosition')

offsetCircle :: Position2D -> Radius -> Circle -> Circle
offsetCircle positionOffset radiusOffset (Circle position radius) =
    Circle (position + positionOffset) (radius + radiusOffset)

offsetIntersection
    :: Position2D -> Radius -> CircleIntersection -> CircleIntersection
offsetIntersection positionOffset radiusOffset =
    bimap (+ positionOffset) (offsetCircle positionOffset radiusOffset)

offsetTrajectoryInstant :: Position2D -> TrajectoryInstant -> TrajectoryInstant
offsetTrajectoryInstant offset = first (+ offset)

-- all positions are relative to the circle that has the first argument radius
getCircleIntersections :: Radius -> Circle -> [CircleIntersection]
getCircleIntersections radius =
    traverseToFst $ getCircleCircleIntersections radius

data Quadrant = Quadrant Vector2D Vector2D

-- all positions are relative to the trajectory center
isCircleCollisionPossible :: TrajectoryInstant -> Circle -> Bool
isCircleCollisionPossible (startPosition', startDirection) =
    hasAreaOnSide (getLine2D startPosition' (V2 0 0)) startDirection
        &&& (not . (impossibleQuandrant `containsPosition`) . getPosition)
  where
    impossibleQuandrant = Quadrant
        startPosition'
        (startPosition' `rotate90Towards` (-startDirection))

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) p1 p2 v = p1 v && p2 v

getPosition :: Circle -> Position2D
getPosition (Circle position _) = position

getClosestCircleIntersectionTo
    :: Position2D
    -> CircleIntersection
    -> Maybe CircleIntersection
    -> Maybe CircleIntersection
getClosestCircleIntersectionTo _ intersection Nothing = Just intersection
getClosestCircleIntersectionTo ref i1@(p1, _) (Just i2@(p2, _)) =
    if isFirstPositionCloserTo ref p1 p2 then Just i1 else Just i2

getClosestPositionTo
    :: Position2D -> Position2D -> Maybe Position2D -> Maybe Position2D
getClosestPositionTo _ intersection Nothing = Just intersection
getClosestPositionTo ref p1 (Just p2) =
    if isFirstPositionCloserTo ref p1 p2 then Just p1 else Just p2

isFirstPositionCloserTo :: Position2D -> Position2D -> Position2D -> Bool
isFirstPositionCloserTo ref p1 p2 = distance ref p1 < distance ref p2

containsPosition :: Quadrant -> Position2D -> Bool
containsPosition (Quadrant v1 v2) pos = pos `dot` v1 > 0 && pos `dot` v2 > 0

hasAreaOnSide :: Line2D -> Vector2D -> Circle -> Bool
hasAreaOnSide l side (Circle pos r) =
    let l'          = offsetLine2D (-pos) l
        closestPos' = getPositionClosestToOrigin l'
    in  pos `dot` side > 0 || norm closestPos' <= r

-- The first circle is centered in origin. Converted from:
-- https://cp-algorithms.com/geometry/circle-circle-intersection.html
getCircleCircleIntersections :: Radius -> Circle -> [Position2D]
getCircleCircleIntersections r1 (Circle (V2 x2 y2) r2) =
    getCircleLineIntersections r1 (a, b, c)
  where
    a = -2 * x2
    b = -2 * y2
    c = x2 ^ 2 + y2 ^ 2 + r1 ^ 2 - r2 ^ 2

-- The circle is centered in origin. Converted from:
-- https://cp-algorithms.com/geometry/circle-line-intersection.html
getCircleLineIntersections :: Radius -> Line2D -> [Position2D]
getCircleLineIntersections r l@(a, b, c)
    | c * c > r * r * (a * a + b * b) + epsilon
    = []
    | abs (c * c - r * r * (a * a + b * b)) < epsilon
    = [V2 x0 y0]
    | otherwise
    = let d  = r * r - c * c / (a * a + b * b)
          m  = sqrt (d / (a * a + b * b))
          ax = x0 + b * m
          bx = x0 - b * m
          ay = y0 - a * m
          by = y0 + a * m
      in  [V2 ax ay, V2 bx by]
    where V2 x0 y0 = getPositionClosestToOrigin l
