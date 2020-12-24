{-# LANGUAGE ImplicitParams #-}

module SpaceSpec where

import Approx
import SDL.Vect
import Space
import Test.Hspec

spec :: Spec
spec = do
  describe "toVelocity" $
    it "converts an angle and a speed to a velocity vector" $
      toVelocity (pi / 6) 2
        `shouldBe` V2 (sqrt 3) 1

  describe "updatePosition2D" $
    it "moves a position given a velocity and a time" $
      updatePosition2D (V2 1 4) (V2 2 (-1)) 2
        `shouldBe` V2 5 2

  describe "boundsToLines2D" $
    it "creates lines from bounds" $
      boundsToLines2D (Bounds2D (10, 100) (20, 200))
        `shouldContain` [(-1, 0, 10), (-1, 0, 100), (0, -1, 20), (0, -1, 200)]

  describe "offsetBounds2D" $
    it "offsets the bounds given a position" $
      offsetBounds2D (V2 3 (-4)) (Bounds2D (1, 2) (7, 9))
        `shouldBe` Bounds2D (4, 5) (3, 5)

  describe "isWithinBounds2D" $ do
    it "returns true if a position is within the bounds" $
      isWithinBounds2D (V2 1 3) (Bounds2D (0, 1) (2, 3))
        `shouldBe` True
    it
      ( "returns false if a position is outside "
          ++ "the bounds on the horizontal axis"
      )
      $ isWithinBounds2D (V2 5 2) (Bounds2D (0, 1) (2, 3))
        `shouldBe` False
    it
      ( "returns false if a position is outside "
          ++ "the bounds on the vertical axis"
      )
      $ isWithinBounds2D (V2 1 0) (Bounds2D (0, 1) (2, 3))
        `shouldBe` False

  describe "isWithinBounds1D" $ do
    it "returns false if below the bounds" $
      isWithinBounds1D 1 (2, 3)
        `shouldBe` False
    it "returns true if within the bounds" $
      isWithinBounds1D 2.5 (2, 3)
        `shouldBe` True
    it "returns false if above the bounds" $
      isWithinBounds1D 3.5 (2, 3)
        `shouldBe` False

  describe "getLineIntersection2D" $ do
    it "returns the point of intersection if it exists" $
      getLineIntersection2D (-2, 1, 1) (1, -1, 1)
        `shouldBe` Just (V2 2 3)
    it "returns nothing if the lines are parallel" $
      getLineIntersection2D (2, -2, 4) (1, -1, 1)
        `shouldBe` Nothing
    it "returns nothing if it is the same line" $
      getLineIntersection2D (2, -2, 2) (1, -1, 1)
        `shouldBe` Nothing

  describe "limitPosition2D" $ do
    it "does not affect position already within the bounds" $
      limitPosition2D (V2 1 3) (Bounds2D (0, 1) (2, 3))
        `shouldBe` V2 1 3
    it
      ( "moves a position outside the bounds "
          ++ "horizontally to be within the bounds"
      )
      $ limitPosition2D (V2 7 3) (Bounds2D (0, 1) (2, 3))
        `shouldBe` V2 1 3
    it
      ( "moves a position outside the bounds "
          ++ "vertically to be within the bounds"
      )
      $ limitPosition2D (V2 1 (-1)) (Bounds2D (0, 1) (2, 3))
        `shouldBe` V2 1 2

  describe "limitPosition1D" $ do
    it "does not affect position already within the bounds" $
      limitPosition1D 2 (1, 3)
        `shouldBe` 2
    it "moves position below the bounds" $
      limitPosition1D 0 (1, 3)
        `shouldBe` 1
    it "moves position above the bounds" $
      limitPosition1D 5 (1, 3)
        `shouldBe` 3

  describe "increaseBounds2D" $
    it "increases the bounds equally on opposing sides" $
      increaseBounds2D (Bounds2D (1, 3) (2, 4)) (V2 2 4)
        `shouldBe` Bounds2D (0, 4) (0, 6)

  describe "increaseBounds1D" $
    it "increases the bounds equally on both sides" $
      increaseBounds1D (1, 3) 2
        `shouldBe` (0, 4)

  describe "decreaseBounds2D" $
    it "decreases the bounds equally on opposing sides" $
      decreaseBounds2D (Bounds2D (0, 4) (0, 6)) (V2 2 4)
        `shouldBe` Bounds2D (1, 3) (2, 4)

  describe "decreaseBounds1D" $
    it "decreases the bounds equally on both sides" $
      decreaseBounds1D (0, 4) 2
        `shouldBe` (1, 3)

  describe "getLine2D" $
    it "finds the line between two positions" $
      getLine2D (V2 5 2) (V2 6 4)
        `shouldBe` (-2, 1, 8)

  describe "angleDifference2D" $ do
    let ?epsilon = epsilon
    it "finds the difference between two angles" $
      angleDifference2D 3 2
        `shouldApproxBe` 1
    it "can handle negative and positive pi" $
      angleDifference2D pi (- pi)
        `shouldApproxBe` 0
    it "can handle from positive to negative numbers" $
      angleDifference2D (pi * 3 / 4) (- pi * 3 / 4)
        `shouldApproxBe` (- pi / 2)
    it "can handle from negative to positive numbers" $
      angleDifference2D (- pi * 3 / 4) (pi * 3 / 4)
        `shouldApproxBe` (pi / 2)
    it "can handle opposite angles" $
      abs (angleDifference2D (pi / 2) (- pi / 2))
        `shouldApproxBe` pi

  describe "offsetLine2D" $ do
    it "does not make any change if the offset is 0" $
      offsetLine2D (V2 0 0) (1, 2, 3)
        `shouldBe` (1, 2, 3)
    it "offsets a horizontal line" $
      offsetLine2D (V2 5 7) (0, 2, 6)
        `shouldBe` (0, 2, -8)
    it "offsets a vertical line by an offset" $
      offsetLine2D (V2 5 7) (2, 0, 6)
        `shouldBe` (2, 0, -4)
    it "offsets a line that is not colinear to the base vectors" $
      offsetLine2D (V2 2 (-1)) (2, 1, -1)
        `shouldBe` (2, 1, -4)

  describe "offsetDistanceToOrigin2D" $ do
    it "offsets a horizontal line above the origin" $
      offsetDistanceToOrigin2D (-1) (0, -1, 3)
        `shouldBe` (0, -1, 2)
    it "offsets a horizontal line under the origin" $
      offsetDistanceToOrigin2D (-1) (0, -1, -3)
        `shouldBe` (0, -1, -2)
    it "offsets a vertical line to the left of the origin" $
      offsetDistanceToOrigin2D (-2) (-1, 0, -5)
        `shouldBe` (-1, 0, -3)
    it "offsets a vertical line to the right of the origin" $
      offsetDistanceToOrigin2D (-2) (-1, 0, 5)
        `shouldBe` (-1, 0, 3)
    it
      ( "when moving towards the origin with an offset bigger than "
          ++ "the distance the line will cross the origin"
      )
      $ offsetDistanceToOrigin2D (-7) (-1, 0, 5)
        `shouldBe` (-1, 0, 0)

  describe "isLineBetween2D" $ do
    it "returns true if line crosses first vector" $
      isLineBetween2D (V2 (-1) 2) (V2 2 1) (-1, 0, -10)
        `shouldBe` True
    it "returns true no matter the order of the vectors" $
      isLineBetween2D (V2 2 1) (V2 (-1) 2) (-1, 0, -10)
        `shouldBe` True
    it "returns true if line crosses second vector" $
      isLineBetween2D (V2 (-1) 2) (V2 2 1) (-1, 0, 10)
        `shouldBe` True
    it "returns false if line does not cross any vector" $
      isLineBetween2D (V2 (-1) 2) (V2 2 1) (0, -1, -1)
        `shouldBe` False
    it "returns false no matter the order of the vectors" $
      isLineBetween2D (V2 2 1) (V2 (-1) 2) (0, -1, -1)
        `shouldBe` False
    it
      ( "can return false no matter if the vectors are above or below"
          ++ " the x axis"
      )
      $ isLineBetween2D (V2 (-2) (-1)) (V2 1 (-2)) (0, -1, 1)
        `shouldBe` False
    it
      ( "can return true no matter if the vectors are above or below"
          ++ " the x axis"
      )
      $ isLineBetween2D (V2 (-2) (-1)) (V2 1 (-2)) (0, -1, -1)
        `shouldBe` True
