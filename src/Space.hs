module Space where

type Position1D = Float
type Velocity1D = Float
type Size1D = Float
type Bounds1D = (Position1D, Position1D)
type Shape1D = (Position1D, Position1D)
type Speed = Float

type Rotation2D = Float
data Position2D = Position2D Position1D Position1D
data Velocity2D = Velocity2D Velocity1D Velocity1D
data Size2D = Size2D Size1D Size1D
data Bounds2D = Bounds2D Bounds1D Bounds1D

toVelocity :: Rotation2D -> Speed -> Velocity2D
toVelocity r s = Velocity2D (axisVelocity cos) (axisVelocity sin)
    where axisVelocity trig = (s * (trig ((pi / 180) * r)))