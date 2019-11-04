module CircleSpec where

import           Test.Hspec
import           SDL.Vect
import           Circle
import           Foreign.C.Types
import           SDL.Video.Renderer             ( Rectangle(..) )
import           Space

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

    describe "updateCollidingCirclePosition" $ do
        it "restricts position of the circle when colliding with bounds" $ do
            updateCollidingCirclePosition (V2 10 10)
                                          1
                                          (Bounds2D (0, 10) (0, 20))
                                          []
                                          (Circle (V2 5 7) 1)
                `shouldBe` Circle (V2 9 17) 1
        it
                ("does not keep the circle at the bounds when moving away from"
                ++ " the bounds"
                )
            $ do
                  updateCollidingCirclePosition (V2 5 5)
                                                1
                                                (Bounds2D (0, 10) (0, 10))
                                                []
                                                (Circle (V2 1 1) 1)
                      `shouldBe` Circle (V2 6 6) 1
        it
                ("restricts position of the circle when colliding with another"
                ++ " circle from the top left"
                )
            $ do
                  updateCollidingCirclePosition
                          (V2 10 10)
                          1
                          (Bounds2D (0, 1000) (0, 1000))
                          [(Circle (V2 200 100) 1)]
                          (Circle (V2 195 95) 2)
                      `shouldBe` Circle
                                     (V2
                                         (200 - 3 / (sqrt 2))
                                         (100 - 3 / (sqrt 2))
                                     )
                                     2
        it
                (  "restricts position of the circle when colliding vertically"
                ++ " with another circle"
                )
            $          do
                           updateCollidingCirclePosition
                               (V2 0 10)
                               1
                               (Bounds2D (0, 1000) (0, 1000))
                               [(Circle (V2 200 100) 1)]
                               (Circle (V2 200 95) 2)
            `shouldBe` Circle (V2 200 97) 2
        it
                (  "restricts position of the circle when colliding ALMOST"
                ++ " vertically with another circle"
                )
            $          do
                           updateCollidingCirclePosition
                               (V2 0.0001 10)
                               1
                               (Bounds2D (0, 1000) (0, 1000))
                               [(Circle (V2 200 100) 1)]
                               (Circle (V2 200 95) 2)
            `shouldBe` Circle (V2 200.00002 97) 2
        it
                (  "restricts position of the circle when colliding"
                ++ " horizontally with another circle"
                )
            $          do
                           updateCollidingCirclePosition
                               (V2 10 0)
                               1
                               (Bounds2D (0, 1000) (0, 1000))
                               [(Circle (V2 200 100) 1)]
                               (Circle (V2 195 100) 2)
            `shouldBe` Circle (V2 197 100) 2
        it
                (  "restricts position of the circle when colliding at an angle"
                ++ " with another circle"
                )
            $          do
                           updateCollidingCirclePosition
                               (V2 10 1)
                               1
                               (Bounds2D (0, 1000) (0, 1000))
                               [(Circle (V2 200 100) 1)]
                               (Circle (V2 195 100) 2)
            `shouldBe` Circle (V2 197.00671 100.20067) 2
        it
                (  "does not keep the circle at an obstacle when moving away"
                ++ " from it"
                )
            $          do
                           updateCollidingCirclePosition
                               (V2 (-10) 0)
                               1
                               (Bounds2D (0, 1000) (0, 1000))
                               [(Circle (V2 200 100) 1)]
                               (Circle (V2 197 100) 2)
            `shouldBe` Circle (V2 187 100) 2
        it
                (  "restricts position of the circle when colliding with the"
                ++ " bounds and a circle"
                )
            $ do
                  updateCollidingCirclePosition (V2 10 10)
                                                1
                                                (Bounds2D (0, 200) (0, 200))
                                                [(Circle (V2 195.5 100) 1)]
                                                (Circle (V2 198 95) 2)
                      `shouldBe` Circle (V2 198 98.34169) 2
        it "can handle no velocity" $ do
            updateCollidingCirclePosition (V2 0 0)
                                          1
                                          (Bounds2D (0, 200) (0, 200))
                                          [(Circle (V2 195.5 100) 1)]
                                          (Circle (V2 198 95) 2)
                `shouldBe` (Circle (V2 198 95) 2)
