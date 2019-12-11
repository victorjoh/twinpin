module ShotSpec where

import           Test.Hspec
import           SDL.Vect
import           Shot
import           Foreign.C.Types
import           Space                          ( Bounds2D(..) )
import           SDL.Video.Renderer             ( Rectangle(..) )

spec :: Spec
spec = do
    describe "toDrawableShot"
        $          it "converts from shot to something that can be drawn by SDL"
        $          toDrawableShot (createShot (V2 30.4 49.8) 45)
        `shouldBe` ( "gen/shot.bmp"
                   , Just (Rectangle (P (V2 25 44)) (V2 11 11))
                   , 0
                   )

    describe "updateShot"
        $ it "updates the shot position from passed time and its velocity"
        $ updateShot 10 (createShot (V2 6 2) 30)
        `shouldBe` createShot
                       (V2 (6 + 10 * shotSpeed * sqrt 3 / 2)
                           (2 + 10 * shotSpeed / 2)
                       )
                       30

    describe "isShotWithinBounds" $ do
        it "returns true if the shot is within the borders"
            $          isShotWithinBounds (Bounds2D (0, 100) (100, 200))
                                          (createShot (V2 50 150) 0)
            `shouldBe` True
        it "returns true if the shot is on the border"
            $          isShotWithinBounds (Bounds2D (0, 100) (100, 200))
                                          (createShot (V2 50 204) 0)
            `shouldBe` True
        it "returns false if the shot is outside the border"
            $          isShotWithinBounds (Bounds2D (0, 100) (100, 200))
                                          (createShot (V2 50 208) 0)
            `shouldBe` False

