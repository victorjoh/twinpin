module VisualSpec where

import           Test.Hspec
import           SDL.Video.Renderer             ( Rectangle(..) )
import           Visual
import           SDL.Vect
import           Circle
import           Codec.Picture.Types

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
