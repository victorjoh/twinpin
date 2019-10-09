module CircleSpec where

import           Test.Hspec
import           SDL.Vect
import           Circle
import           Foreign.C.Types
import           SDL.Video.Renderer             ( Rectangle(..) )

spec :: Spec
spec = do
    describe "toDrawableCircle" $ do
        it "converts from circle to something that can be drawn by SDL"
            $          do
                           toDrawableCircle (Circle (V2 30 45) 5) 45 "texture-file-path"
            `shouldBe` ( "texture-file-path"
                       , Just (Rectangle (P (V2 25 40)) (V2 10 10))
                       , 45
                       )

    describe "areIntersecting" $ do
        it "returns true if two circles are intersected" $ do
            areIntersecting (Circle (V2 10 20) 3) (Circle (V2 12 23.4) 1)
        it "returns false if two circles are not intersected" $ do
            not $ areIntersecting (Circle (V2 10 20) 3) (Circle (V2 12 23.5) 1)
