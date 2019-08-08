module Space where

type Position1D = Float
type Velocity1D = Float
type Size1D = Float
type Bounds1D = (Position1D, Position1D)
type Shape1D = (Position1D, Position1D)

type Rotation2D = Float
data Position2D = Position2D Position1D Position1D
data Velocity2D = Velocity2D Velocity1D Velocity1D
data Size2D = Size2D Size1D Size1D
data Bounds2D = Bounds2D Bounds1D Bounds1D
