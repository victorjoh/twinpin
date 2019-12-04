module Circle
    ( Circle(..)
    , Radius
    , toDrawableCircle
    , areIntersecting
    , isCircleWithinBounds
    , updateCirclePosition
    , updateCollidingCirclePosition
    , Obstacles(..)
    )
where

import           Space
import           Foreign.C.Types
import           SDL.Video.Renderer             ( Rectangle(..) )
import           SDL.Vect
import           Data.Maybe
import           Data.Bifunctor                 ( first
                                                , second
                                                , bimap
                                                )
import           Data.List                      ( delete )
import           Relude.Extra.Tuple             ( traverseToFst )

type Radius = Float
data Circle = Circle Position2D Radius deriving (Show, Eq)
data Waypoint = Waypoint Position2D WaypointType
data WaypointType = CircleCollision Circle | BoundsCollision | MovementFinished
data Obstacles = Obstacles Bounds2D [Circle] deriving (Show)

class (Show m) => Movement m where
    getStart, getEnd :: m -> Position2D
    collideWithBounds :: Radius -> m -> Bounds2D -> Maybe Waypoint
    collideWithCircles :: Radius -> m -> [Circle] -> Maybe Waypoint
-- start position and direction and end position and direction
data CircularMovement = CircularMovement (Position2D, Vector2D)
                                         (Position2D, Vector2D)
                                         Circle deriving (Show)
data StraightMovement = StraightMovement Position2D Position2D deriving (Show)

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
    :: Velocity2D -> DeltaTime -> Obstacles -> Circle -> Circle
updateCollidingCirclePosition velocity dt obstacles (Circle start radius) =
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
        newEnd       = (limitPosition2D (getEnd movement) centerBounds)
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
    let start                = getStart movement
        centerTouchingCircle = increaseRadius radiusInMotion touchingCircle
        (Circle touchingCirclePos centerTouchingR) = centerTouchingCircle
        start'               = start - touchingCirclePos
        endDirection         = (getEnd movement) - start
        endDirectionUnit     = toUnitVector endDirection
        V2 endDirectionUnitX endDirectionUnitY = endDirectionUnit
        V2 startX'           startY'           = start'
        startDirection = flipToClosest (V2 (-startY') startX') endDirection
        escapePos'           = flipToClosest
            (centerTouchingR @* (V2 (-endDirectionUnitY) endDirectionUnitX))
            startDirection
        possibleDistanceOnCircle = scalarProjection endDirection startDirection
        possibleAngleDistance    = possibleDistanceOnCircle / centerTouchingR
        startAngle'              = vectorToAngle start'
        escapeAngleDisplacement =
                startAngle' `angleDifference2D` vectorToAngle escapePos'
        escapeAngleDistance = abs escapeAngleDisplacement
    in  if abs (startDirection `dot` endDirection) < epsilon
            then start
            else if escapeAngleDistance > possibleAngleDistance
                then endMovementOnCircle
                    radiusInMotion
                    (CircularMovement
                        (start, startDirection)
                        ( (centerTouchingR @* angleToVector
                              ( startAngle'
                              - possibleAngleDistance
                              * signum escapeAngleDisplacement
                              )
                          )
                            + touchingCirclePos
                        , endDirection
                        )
                        centerTouchingCircle
                    )
                    touchingCircle
                    obstacles
                else movePastCircle
                    radiusInMotion
                    (CircularMovement
                        (start                           , startDirection)
                        ((escapePos' + touchingCirclePos), endDirection)
                        centerTouchingCircle
                    )
                    touchingCircle
                    (  ( possibleDistanceOnCircle
                       - escapeAngleDistance
                       * centerTouchingR
                       )
                    @* endDirectionUnit
                    +  escapePos'
                    +  touchingCirclePos
                    )
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
        (Obstacles bounds circles)             = obstacles
        nonTouchedCircles                      = (delete touchedCircle circles)
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

getNextWaypoint :: (Movement m) => Radius -> m -> Obstacles -> Waypoint
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
pairToWaypoint (position, circle) = Waypoint position $ CircleCollision circle

getMovementCircleIntersections :: StraightMovement -> Circle -> [Position2D]
getMovementCircleIntersections (StraightMovement start end) circle =
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
    where Circle circlePosition circleRadius = circle

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
                  ( isLineBetween2D startDirection endDirection
                  . offsetLine2D (-start')
                  )
            $ boundsToLines2D bounds'
      where
        CircularMovement start        end             trajectory = movement
        (                startPosition, startDirection)          = start
        (                endPosition  , endDirection  )          = end
        (Circle trajectoryCenter trajectoryRadius) = trajectory
        bounds' = offsetBounds2D (-trajectoryCenter) bounds
        start'  = startPosition - trajectoryCenter
        end'    = endPosition - trajectoryCenter

    collideWithCircles radiusInMotion movement obstacles =
        fmap
                ( pairToWaypoint
                . bimap (+ trajectoryCenter) (flip (+@) trajectoryCenter)
                . second (decreaseRadius radiusInMotion)
                )
            $ foldr (getClosestCircleCollision current') Nothing
            $ concatMap
                  (traverseToFst $ getCircleCircleIntersections trajectoryRadius
                  )
            $ map (increaseRadius radiusInMotion)
            $ filter
                  (hasAreaOnSide (getLine2D current' trajectoryCenter')
                                 startDirection
                  )
                  obstacles'
      where
        CircularMovement start        (endPosition, _) trajectory = movement
        (                startPosition, startDirection )          = start
        Circle trajectoryCenter trajectoryRadius = trajectory
        trajectoryCenter' = V2 0 0
        current'          = startPosition - trajectoryCenter
        target'           = endPosition - trajectoryCenter
        (-@) (Circle pos r) offset = Circle (pos - offset) r
        (+@) (Circle pos r) offset = Circle (pos + offset) r
        obstacles' = map (flip (-@) trajectoryCenter) obstacles

getClosestBoundsCollision
    :: Position2D -> Position2D -> Maybe Position2D -> Maybe Position2D
getClosestBoundsCollision _ collision Nothing = Just collision
getClosestBoundsCollision reference p1 (Just p2) =
    if distance reference p1 < distance reference p2 then Just p1 else Just p2

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
getCircleLineIntersections r line =
    let (a, b, c)  = line
        (V2 x0 y0) = getPositionClosestToOrigin line
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
