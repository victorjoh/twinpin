{-# LANGUAGE ImplicitParams #-}
module VisualSpec where

import           Test.Hspec
import           SDL.Video.Renderer             ( Rectangle(..) )
import           Visual
import           SDL.Vect
import           Circle
import           Codec.Picture.Types
import           Graphics.Rasterific     hiding ( V2(..) )
import qualified Graphics.Rasterific           as R
                                                ( V2(..) )
import           Relude.Extra.Bifunctor         ( bimapBoth )
import           Approx
import           VisualUtil                     ( )

spec :: Spec
spec = do
    describe "renderScaledVectorImage" $ do
        let sizeOfImage image = V2 (imageWidth image) (imageHeight image)
        it "scales a vector image with the given ratio"
            $          sizeOfImage
                           ( renderScaledVectorImage 2
                           $ VectorImage (V2 15 10) transparent
                           $ toDrawing 5
                           )
            `shouldBe` V2 30 20

    describe "getScaleRatioAndOffset" $ do
        it "returns no modification when both are the same"
            $          getScaleRatioAndOffset (V2 800 600) (V2 800 600)
            `shouldBe` (1, V2 0 0)
        it
                (  "returns a change in offset when pixel area has a narrower "
                ++ "aspect ratio"
                )
            $          getScaleRatioAndOffset (V2 800 600) (V2 1600 600)
            `shouldBe` (1, V2 400 0)
        it
                (  "returns a change in offset when pixel area has a narrower "
                ++ "aspect ratio"
                )
            $          getScaleRatioAndOffset (V2 800 600) (V2 800 1200)
            `shouldBe` (1, V2 0 300)
        it
                (  "returns a change in ratio when pixel area has a higher "
                ++ "resolution"
                )
            $          getScaleRatioAndOffset (V2 800 600) (V2 1600 1200)
            `shouldBe` (2, V2 0 0)
        it
                (  "returns a change in ratio and offset when the pixel area "
                ++ "has a higher resolution and a different aspect ratio"
                )
            $          getScaleRatioAndOffset (V2 800 600) (V2 2000 1200)
            `shouldBe` (2, V2 200 0)

    describe "moveRectangle"
        $          it "moves a rectangle a given distance"
        $          moveRectangle (V2 1 2) (Rectangle (P $ V2 5 14) (V2 40 50))
        `shouldBe` Rectangle (P $ V2 6 16) (V2 40 50)

    describe "scaleRectangle"
        $          it "scales a rectangle with a given multiplier"
        $          scaleRectangle 2 (Rectangle (P $ V2 5 7) (V2 11 13))
        `shouldBe` Rectangle (P $ V2 10 14) (V2 22 26)


    describe "circleSector" $ do
        let xLine = Right $ Line (R.V2 0 0) (R.V2 1 0)
        it "gives no points if angle is 0 deg" $ circleSector 0 `shouldBe` []
        it "gives no points if angle is less than 0 deg"
            $          circleSector (-pi / 4)
            `shouldBe` []
        it "gives a circle sector for 45 deg"
            $          circleSector (pi / 4)
            `shouldBe` [ xLine
                       , Right
                           $ Line (R.V2 0.7069682 (-0.7069682)) (R.V2 0.0 0.0)
                       , Left $ CubicBezier (R.V2 1.0 0.0)
                                            (R.V2 1.0 (-0.27595752))
                                            (R.V2 0.8879788 (-0.5259575))
                                            (R.V2 0.7069682 (-0.7069682))
                       ]
        it "gives a circle sector for 135 deg"
            $          circleSector (pi * 3 / 4)
            `shouldBe` [ xLine
                       , Right $ Line (R.V2 (-0.70696807) (-0.70696807))
                                      (R.V2 0.0 0.0)
                       , Left $ CubicBezier
                           (R.V2 0.0 (-1.0))
                           (R.V2 (-0.2759575) (-1.0))
                           (R.V2 (-0.52595747) (-0.8879787))
                           (R.V2 (-0.70696807) (-0.70696807))
                       , Left circleQuadrant1
                       ]
        it "gives a circle sector for 180 deg"
            $          circleSector pi
            `shouldBe` [ xLine
                       , Right $ Line (R.V2 (-1.0) 0.0) (R.V2 0.0 0.0)
                       , Left circleQuadrant2
                       , Left circleQuadrant1
                       ]
        it "gives a circle sector for 225 deg"
            $          circleSector (pi * 5 / 4)
            `shouldBe` [ xLine
                       , Right
                           $ Line (R.V2 (-0.70696807) 0.7069682) (R.V2 0.0 0.0)
                       , Left $ CubicBezier (R.V2 (-1.0) 0.0)
                                            (R.V2 (-1.0) 0.27595755)
                                            (R.V2 (-0.88797873) 0.5259576)
                                            (R.V2 (-0.70696807) 0.7069682)
                       , Left circleQuadrant2
                       , Left circleQuadrant1
                       ]
        it "gives a circle sector for 315 deg"
            $          circleSector (pi * 7 / 4)
            `shouldBe` [ xLine
                       , Right $ Line (R.V2 0.7069683 0.70696795) (R.V2 0.0 0.0)
                       , Left $ CubicBezier (R.V2 0.0 1.0)
                                            (R.V2 0.2759576 1.0)
                                            (R.V2 0.5259577 0.8879787)
                                            (R.V2 0.7069683 0.70696795)
                       , Left circleQuadrant3
                       , Left circleQuadrant2
                       , Left circleQuadrant1
                       ]
        it "gives a circle for 360 deg"
            $          circleSector (2 * pi)
            `shouldBe` map Left bezierCircle
        it "gives a circle for angles above 360 deg"
            $          circleSector (pi * 9 / 4)
            `shouldBe` map Left bezierCircle

    describe "scaleAndOffset"
        $          it "scales and offsets the points with a value"
        $          scaleAndOffset 2 (Line (R.V2 1 (-1)) (R.V2 (-1) (-1)))
        `shouldBe` Line (R.V2 4 0) (R.V2 0 0)

    describe "breakCubicBezierAt" $ do
        let ?epsilon = 0.00001
        let bezierValue t (CubicBezier p0 p1 p2 p3) =
                ((1 - t) ^ 3 *^ p0)
                    + (3 * (1 - t) ^ 2 * t *^ p1)
                    + (3 * (1 - t) * t ^ 2 *^ p2)
                    + (t ^ 3 *^ p3)
        it
                (  "splits a bezier curve in two, keeping the curvature of the "
                ++ "orignal curve when seen together"
                )
            $ let
                  original = CubicBezier (R.V2 0 1)
                                         (R.V2 2 15)
                                         (R.V2 13 (-1))
                                         (R.V2 10 8)
              in  bimapBoth (bezierValue 0.5) (breakCubicBezierAt original 0.6)
                      `shouldApproxBe` ( bezierValue 0.3 original
                                       , bezierValue 0.8 original
                                       )

    describe "breakLineAt" $ do
        let ?epsilon = 0.00001
        it "splits a line in two"
            $                breakLineAt (Line (R.V2 1 0) (R.V2 11 20)) 0.6
            `shouldApproxBe` ( Line (R.V2 1 0)  (R.V2 7 12)
                             , Line (R.V2 7 12) (R.V2 11 20)
                             )
