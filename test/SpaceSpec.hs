{-# LANGUAGE ImplicitParams #-}

module SpaceSpec where

import           Test.Hspec
import           Space
import           SDL.Vect
import           Foreign.C.Types
import           Test.HUnit.Approx

spec :: Spec
spec = do
    describe "toVelocity" $ do
        it "converts an angle and a speed to a velocity vector" $ do
            toVelocity 30 2 `shouldBe` (V2 (sqrt 3) 1)

    describe "toPixelPoint" $ do
        it "rounds a real 2D point to a pixel 2D point" $ do
            toPixelPoint (V2 3.6 3.4) `shouldBe` (P $ V2 4 3)

    describe "toPixelSize" $ do
        it "rounds a real 2D size to a pixel 2D size" $ do
            toPixelSize (V2 3.6 3.4) `shouldBe` V2 4 3

    describe "toPixelAngle" $ do
        it "converts a 2D angle to the angle format expected by SDL" $ do
            toPixelAngle 10.5 `shouldBe` 10.5

    describe "updatePosition2D" $ do
        it "moves a position given a velocity and a time" $ do
            updatePosition2D (V2 1 4) (V2 2 (-1)) 2 `shouldBe` (V2 5 2)

    describe "boundsToLines2D" $ do
        it "creates lines from bounds" $ do
            boundsToLines2D (Bounds2D (10, 100) (20, 200))
                `shouldContain` [ (-1, 0 , 10)
                                , (-1, 0 , 100)
                                , (0 , -1, 20)
                                , (0 , -1, 200)
                                ]

    describe "offsetBounds2D" $ do
        it "offsets the bounds given a position" $ do
            offsetBounds2D (V2 3 (-4)) (Bounds2D (1, 2) (7, 9))
                `shouldBe` (Bounds2D (4, 5) (3, 5))

    describe "isWithinBounds2D" $ do
        it "returns true if a position is within the bounds" $ do
            isWithinBounds2D (V2 1 3) (Bounds2D (0, 1) (2, 3)) `shouldBe` True
        it
                (  "returns false if a position is outside "
                ++ "the bounds on the horizontal axis"
                )
            $ do
                  isWithinBounds2D (V2 5 2) (Bounds2D (0, 1) (2, 3))
                      `shouldBe` False
        it
                (  "returns false if a position is outside "
                ++ "the bounds on the vertical axis"
                )
            $ do
                  isWithinBounds2D (V2 1 0) (Bounds2D (0, 1) (2, 3))
                      `shouldBe` False

    describe "isWithinBounds1D" $ do
        it "returns false if below the bounds" $ do
            isWithinBounds1D 1 (2, 3) `shouldBe` False
        it "returns true if within the bounds" $ do
            isWithinBounds1D 2.5 (2, 3) `shouldBe` True
        it "returns false if above the bounds" $ do
            isWithinBounds1D 3.5 (2, 3) `shouldBe` False

    describe "getLineIntersection2D" $ do
        it "returns the point of intersection if it exists" $ do
            getLineIntersection2D (-2, 1, 1) (1, -1, 1) `shouldBe` Just (V2 2 3)
        it "returns nothing if the lines are parallel" $ do
            getLineIntersection2D (2, -2, 4) (1, -1, 1) `shouldBe` Nothing
        it "returns nothing if it is the same line" $ do
            getLineIntersection2D (2, -2, 2) (1, -1, 1) `shouldBe` Nothing

    describe "limitPosition2D" $ do
        it "does not affect position already within the bounds" $ do
            limitPosition2D (V2 1 3) (Bounds2D (0, 1) (2, 3))
                `shouldBe` (V2 1 3)
        it
                (  "moves a position outside the bounds "
                ++ "horizontally to be within the bounds"
                )
            $ do
                  limitPosition2D (V2 7 3) (Bounds2D (0, 1) (2, 3))
                      `shouldBe` (V2 1 3)
        it
                (  "moves a position outside the bounds "
                ++ "vertically to be within the bounds"
                )
            $ do
                  limitPosition2D (V2 1 (-1)) (Bounds2D (0, 1) (2, 3))
                      `shouldBe` (V2 1 2)

    describe "limitPosition1D" $ do
        it "does not affect position already within the bounds" $ do
            limitPosition1D 2 (1, 3) `shouldBe` 2
        it "moves position below the bounds" $ do
            limitPosition1D 0 (1, 3) `shouldBe` 1
        it "moves position above the bounds" $ do
            limitPosition1D 5 (1, 3) `shouldBe` 3

    describe "increaseBounds2D" $ do
        it "increases the bounds equally on opposing sides" $ do
            increaseBounds2D (Bounds2D (1, 3) (2, 4)) (V2 2 4)
                `shouldBe` (Bounds2D (0, 4) (0, 6))

    describe "increaseBounds1D" $ do
        it "increases the bounds equally on both sides" $ do
            increaseBounds1D (1, 3) 2 `shouldBe` (0, 4)

    describe "decreaseBounds2D" $ do
        it "decreases the bounds equally on opposing sides" $ do
            decreaseBounds2D (Bounds2D (0, 4) (0, 6)) (V2 2 4)
                `shouldBe` (Bounds2D (1, 3) (2, 4))

    describe "decreaseBounds1D" $ do
        it "decreases the bounds equally on both sides" $ do
            decreaseBounds1D (0, 4) 2 `shouldBe` (1, 3)

    describe "getLine2D" $ do
        it "finds the line between two positions" $ do
            getLine2D (V2 5 2) (V2 6 4) `shouldBe` (-2, 1, 8)

    describe "angleDifference2D" $ do
        let ?epsilon = epsilon
        it "finds the difference between two angles" $ do
            angleDifference2D 3 2 @?~ 1
        it "can handle negative and positive pi" $ do
            angleDifference2D pi (-pi) @?~ 0
        it "can handle from positive to negative numbers" $ do
            angleDifference2D (pi * 3 / 4) (-pi * 3 / 4) @?~ -pi / 2
        it "can handle from negative to positive numbers" $ do
            angleDifference2D (-pi * 3 / 4) (pi * 3 / 4) @?~ pi / 2
        it "can handle opposite angles" $ do
            abs (angleDifference2D (pi / 2) (-pi / 2)) @?~ pi

    describe "offsetLine2D" $ do
        it "does not make any change if the offset is 0" $ do
            offsetLine2D (V2 0 0) (1, 2, 3) `shouldBe` (1, 2, 3)
        it "offsets a horizontal line" $ do
            offsetLine2D (V2 5 7) (0, 2, 6) `shouldBe` (0, 2, -8)
        it "offsets a vertical line by an offset" $ do
            offsetLine2D (V2 5 7) (2, 0, 6) `shouldBe` (2, 0, -4)
        it "offsets a line that is not colinear to the base vectors" $ do
            offsetLine2D (V2 2 (-1)) (2, 1, (-1)) `shouldBe` (2, 1, -4)

    describe "offsetDistanceToOrigin2D" $ do
        it "offsets a horizontal line above the origin" $ do
            offsetDistanceToOrigin2D (-1) (0, -1, 3) `shouldBe` (0, -1, 2)
        it "offsets a horizontal line under the origin" $ do
            offsetDistanceToOrigin2D (-1) (0, -1, -3) `shouldBe` (0, -1, -2)
        it "offsets a vertical line to the left of the origin" $ do
            offsetDistanceToOrigin2D (-2) (-1, 0, -5) `shouldBe` (-1, 0, -3)
        it "offsets a vertical line to the right of the origin" $ do
            offsetDistanceToOrigin2D (-2) (-1, 0, 5) `shouldBe` (-1, 0, 3)
        it
                ("when moving towards the origin with an offset bigger than the"
                ++ "distance the line will cross the origin"
                )
            $ do
                  offsetDistanceToOrigin2D (-7) (-1, 0, 5) `shouldBe` (-1, 0, 0)

    describe "isLineBetween2D" $ do
        it "returns true if line crosses first vector" $ do
            isLineBetween2D (V2 (-1) 2) (V2 2 1) (-1, 0, -10) `shouldBe` True
        it "returns true no matter the order of the vectors" $ do
            isLineBetween2D (V2 2 1) (V2 (-1) 2) (-1, 0, -10) `shouldBe` True
        it "returns true if line crosses second vector" $ do
            isLineBetween2D (V2 (-1) 2) (V2 2 1) (-1, 0, 10) `shouldBe` True
        it "returns false if line does not cross any vector" $ do
            isLineBetween2D (V2 (-1) 2) (V2 2 1) (0, -1, -1) `shouldBe` False
        it "returns false no matter the order of the vectors" $ do
            isLineBetween2D (V2 2 1) (V2 (-1) 2) (0, -1, -1) `shouldBe` False
        it
                ("can return false no matter if the vectors are above or below"
                ++ " the x axis"
                )
            $ do
                  isLineBetween2D (V2 (-2) (-1)) (V2 1 (-2)) (0, -1, 1)
                      `shouldBe` False
        it
                (  "can return true no matter if the vectors are above or below"
                ++ " the x axis"
                )
            $ do
                  isLineBetween2D (V2 (-2) (-1)) (V2 1 (-2)) (0, -1, -1)
                      `shouldBe` True


