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
import           Graphics.Rasterific     hiding ( V2(..) )
import qualified Graphics.Rasterific           as Rasterific
                                                ( V2(..) )
import           Graphics.Rasterific.Texture
import           Codec.Picture.Types
import           Data.Maybe
import           Data.Bifunctor                 ( second
                                                , bimap
                                                )
import           Data.List                      ( delete )
import           Relude.Extra.Tuple             ( traverseToFst )

type Radius = Float
type Diameter = Float
data Circle = Circle Position2D Radius deriving (Show, Eq)
data Waypoint = Waypoint Position2D WaypointType deriving (Show)
data WaypointType = CircleCollision Circle | BoundsCollision | MovementFinished
                    deriving (Show)
data Obstacles = Obstacles Bounds2D [Circle] deriving (Show, Eq)

class Show m => Movement m where
    getStart, getEnd :: m -> Position2D
    collideWithBounds :: Radius -> m -> Bounds2D -> Maybe Waypoint
    collideWithCircles :: Radius -> m -> [Circle] -> Maybe Waypoint
-- start position and direction and end position and direction
data CircularMovement = CircularMovement (Position2D, Vector2D)
                                         (Position2D, Vector2D)
                                         Circle deriving (Show)
data StraightMovement = StraightMovement Position2D Position2D deriving (Show)

toSolidCircleImage :: PixelRGBA8 -> Radius -> VectorImage
toSolidCircleImage color radius =
    VectorImage (V2 diameter diameter) transparent
        $ withTexture (uniformTexture color)
        $ toDrawing radius
    where diameter = toDiameter radius

toDrawing :: Radius -> Drawing px ()
toDrawing radius = fill $ circle (Rasterific.V2 radius radius) radius

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
moveCollidingCircle velocity dt obstacles (Circle start radius) =
    let end = updatePosition2D start velocity dt
    in  Circle (moveFreely radius (StraightMovement start end) obstacles) radius

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
    (getEnd movement)
    obstacles

moveFromWaypoint :: Radius -> Waypoint -> Position2D -> Obstacles -> Position2D
moveFromWaypoint radiusInMotion waypoint end obstacles = case waypointType of
    BoundsCollision -> (moveAlongBounds radiusInMotion movement obstacles)
    CircleCollision collidedWith ->
        (moveAlongCircle radiusInMotion movement collidedWith obstacles)
    MovementFinished -> waypointPosition
  where
    Waypoint waypointPosition waypointType = waypoint
    movement = StraightMovement waypointPosition end

moveAlongBounds :: Radius -> StraightMovement -> Obstacles -> Position2D
moveAlongBounds radiusInMotion movement (Obstacles bounds circles) =
    let centerBounds = decreaseBounds2D bounds $ boundingBoxSize radiusInMotion
        newEnd       = limitPosition2D (getEnd movement) centerBounds
    in  maybe
            newEnd
            (\(Waypoint position _) -> position)
            (collideWithCircles
                radiusInMotion
                (StraightMovement (getStart movement) newEnd)
                circles
            )

moveAlongCircle
    :: Radius -> StraightMovement -> Circle -> Obstacles -> Position2D
moveAlongCircle radiusInMotion movement touchingCircle obstacles =
    let
        start               = getStart movement
        trajectory          = increaseRadius radiusInMotion touchingCircle
        (Circle trajectoryCenter trajectoryRadius) = trajectory
        escapeDirection     = getEnd movement - start
        start'              = start - trajectoryCenter
        startDirection      = start' `rotate90Towards` escapeDirection
        startAngle'         = vectorToAngle start'
        escapeDirectionUnit = toUnitVector escapeDirection
        escapePos' =
            trajectoryRadius
                @*                escapeDirectionUnit
                `rotate90Towards` startDirection
        escapeAngle'            = vectorToAngle escapePos'
        escapeAngleDisplacement = startAngle' `angleDifference2D` escapeAngle'
        escapeAngleDistance     = abs escapeAngleDisplacement
        possibleDistanceOnCircle =
            scalarProjection escapeDirection startDirection
        possibleAngleDistance = possibleDistanceOnCircle / trajectoryRadius
    in
        if abs (startDirection `dot` escapeDirection) < epsilon
            then start
            else if escapeAngleDistance > possibleAngleDistance
                then
                    let
                        endAngle =
                            startAngle'
                                - possibleAngleDistance
                                * signum escapeAngleDisplacement
                        end =
                            trajectoryRadius
                                @* angleToVector endAngle
                                +  trajectoryCenter
                        circularMovement = CircularMovement
                            (start, startDirection)
                            (end  , escapeDirection)
                            trajectory
                    in
                        endMovementOnCircle radiusInMotion
                                            circularMovement
                                            touchingCircle
                                            obstacles
                else
                    let
                        escapePos        = escapePos' + trajectoryCenter
                        circularMovement = CircularMovement
                            (start    , startDirection)
                            (escapePos, escapeDirection)
                            trajectory
                        distanceMovedOnCircle =
                            escapeAngleDistance * trajectoryRadius
                        movementDistanceLeft =
                            possibleDistanceOnCircle - distanceMovedOnCircle
                        end =
                            escapePos
                                +  movementDistanceLeft
                                @* escapeDirectionUnit
                    in
                        movePastCircle radiusInMotion
                                       circularMovement
                                       touchingCircle
                                       end
                                       obstacles

endMovementOnCircle
    :: Radius -> CircularMovement -> Circle -> Obstacles -> Position2D
endMovementOnCircle radiusInMotion movement touchedCircle obstacles =
    let Obstacles bounds           circles = obstacles
        Waypoint  waypointPosition _       = getNextWaypoint
            radiusInMotion
            movement
            (Obstacles bounds (delete touchedCircle circles))
    in  waypointPosition

movePastCircle
    :: Radius
    -> CircularMovement
    -> Circle
    -> Position2D
    -> Obstacles
    -> Position2D
movePastCircle radiusInMotion movement touchedCircle end obstacles =
    let
        Obstacles bounds circles               = obstacles
        nonTouchedCircles                      = delete touchedCircle circles
        CircularMovement _ (exit, _) _         = movement
        Waypoint waypointPosition waypointType = getNextWaypoint
            radiusInMotion
            movement
            (Obstacles bounds nonTouchedCircles)
    in
        case waypointType of
            MovementFinished -> moveFromWaypoint
                radiusInMotion
                (getNextWaypoint radiusInMotion
                                 (StraightMovement exit end)
                                 (Obstacles bounds nonTouchedCircles)
                )
                end
                (Obstacles bounds circles)
            _ -> waypointPosition

getNextWaypoint :: Movement m => Radius -> m -> Obstacles -> Waypoint
getNextWaypoint radiusInMotion movement (Obstacles bounds circles) = foldr
    (getClosestWaypoint (getStart movement))
    (Waypoint (getEnd movement) MovementFinished)
    (catMaybes
        [ collideWithBounds radiusInMotion movement bounds
        , collideWithCircles radiusInMotion movement circles
        ]
    )

getClosestWaypoint :: Position2D -> Waypoint -> Waypoint -> Waypoint
getClosestWaypoint reference w1 w2 =
    let (Waypoint p1 _) = w1
        (Waypoint p2 _) = w2
    in  if distance reference p1 < distance reference p2 then w1 else w2

instance Movement StraightMovement where
    getStart (StraightMovement start _) = start
    getEnd (StraightMovement _ end) = end

    collideWithBounds radiusInMotion movement bounds =
        fmap (flip Waypoint BoundsCollision . (+ start))
            $ foldr (getClosestBoundsCollision start') Nothing
            $ mapMaybe
                  ( getLineIntersection2D (getLine2D start' end')
                  . offsetDistanceToOrigin2D (-radiusInMotion)
                  )
            $ filter (isLineCrossingVector2D end')
            $ boundsToLines2D bounds'
      where
        bounds' = offsetBounds2D (-start) bounds
        start'  = V2 0 0
        end'    = getEnd movement - start
        start   = getStart movement

    collideWithCircles radiusInMotion movement obstacles =
        fmap (pairToWaypoint . second (decreaseRadius radiusInMotion))
            $ foldr (getClosestCircleCollision start) Nothing
            $ concatMap
                  (traverseToFst $ getMovementCircleIntersections movement)
            $ filter (isInFrontOf start end)
            $ map (increaseRadius radiusInMotion) obstacles
      where
        isInFrontOf from to (Circle obstacle _) =
            (to - from) `dot` (obstacle - from) > epsilon
        StraightMovement start end = movement

pairToWaypoint :: (Position2D, Circle) -> Waypoint
pairToWaypoint (position, shape) = Waypoint position $ CircleCollision shape

getMovementCircleIntersections :: StraightMovement -> Circle -> [Position2D]
getMovementCircleIntersections (StraightMovement start end) shape =
    filter
            (\collision ->
                epsilon + distance start end > distance start collision
            )
        $ map
              (+ circlePosition)
              (getCircleLineIntersections
                  circleRadius
                  (getLine2D (start - circlePosition) (end - circlePosition))
              )
    where Circle circlePosition circleRadius = shape

getClosestCircleCollision
    :: Position2D
    -> (Position2D, Circle)
    -> Maybe (Position2D, Circle)
    -> Maybe (Position2D, Circle)
getClosestCircleCollision _ collision Nothing = Just collision
getClosestCircleCollision reference (p1, c1) (Just (p2, c2)) =
    if distance reference p1 < distance reference p2
        then Just (p1, c1)
        else Just (p2, c2)

instance Movement CircularMovement where
    getStart (CircularMovement (startPosition, _) _ _) = startPosition
    getEnd (CircularMovement _ (endPosition, _) _) = endPosition

    collideWithBounds radiusInMotion movement bounds =
        fmap (flip Waypoint BoundsCollision . (+ trajectoryCenter))
            $ foldr (getClosestBoundsCollision start') Nothing
            $ concatMap (getCircleLineIntersections trajectoryRadius)
            $ map (offsetDistanceToOrigin2D (-radiusInMotion))
            $ filter
                  ( isLineBetween2D startDirection escapeDirection
                  . offsetLine2D (-start')
                  )
            $ boundsToLines2D bounds'
      where
        CircularMovement start        end              trajectory = movement
        (                startPosition, startDirection )          = start
        (                _            , escapeDirection)          = end
        (Circle trajectoryCenter trajectoryRadius) = trajectory
        bounds' = offsetBounds2D (-trajectoryCenter) bounds
        start'  = startPosition - trajectoryCenter

    collideWithCircles radiusInMotion movement obstacles =
        fmap pairToWaypoint
            $ fmap (offsetIntersection trajectoryCenter (-radiusInMotion))
            $ foldr (getClosestCircleCollision startPosition') Nothing
            $ toCircleIntersections trajectoryRadius
            $ filter (isCollisionPossible startPosition' startDirection)
            $ map (offsetCircle (-trajectoryCenter) radiusInMotion) obstacles
      where
        CircularMovement start        _               trajectory = movement
        (                startPosition, startDirection)          = start
        Circle trajectoryCenter trajectoryRadius = trajectory
        startPosition' = startPosition - trajectoryCenter

offsetCircle :: Position2D -> Radius -> Circle -> Circle
offsetCircle positionOffset radiusOffset (Circle position radius) =
    Circle (position + positionOffset) (radius + radiusOffset)

type CircleIntersection = (Position2D, Circle)

offsetIntersection
    :: Position2D -> Radius -> CircleIntersection -> CircleIntersection
offsetIntersection positionOffset radiusOffset =
    bimap (+ positionOffset) (offsetCircle positionOffset radiusOffset)

-- all positions are relative to the circle that has the first argument radius
toCircleIntersections :: Radius -> [Circle] -> [CircleIntersection]
toCircleIntersections radius =
    concatMap (traverseToFst $ getCircleCircleIntersections radius)

data Quadrant = Quadrant Vector2D Vector2D

-- all positions are relative to the trajectory center
isCollisionPossible :: Position2D -> Vector2D -> Circle -> Bool
isCollisionPossible startPosition' startDirection =
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

getClosestBoundsCollision
    :: Position2D -> Position2D -> Maybe Position2D -> Maybe Position2D
getClosestBoundsCollision _ collision Nothing = Just collision
getClosestBoundsCollision reference p1 (Just p2) =
    if distance reference p1 < distance reference p2 then Just p1 else Just p2

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
getCircleLineIntersections r l =
    let (a, b, c)  = l
        (V2 x0 y0) = getPositionClosestToOrigin l
    in  if c * c > r * r * (a * a + b * b) + epsilon
            then []
            else if abs (c * c - r * r * (a * a + b * b)) < epsilon
                then [V2 x0 y0]
                else
                    let d  = r * r - c * c / (a * a + b * b)
                        m  = sqrt (d / (a * a + b * b))
                        ax = x0 + b * m
                        bx = x0 - b * m
                        ay = y0 - a * m
                        by = y0 + a * m
                    in  [V2 ax ay, V2 bx by]
