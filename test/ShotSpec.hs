module ShotSpec where

import           Test.Hspec
import           SDL.Vect
import           Shot
import           Circle
import           Foreign.C.Types
import           Space                          ( Bounds2D(..) )
import           SDL.Video.Renderer             ( Rectangle(..) )

spec :: Spec
spec = do
    describe "toDrawableShot"
        $ it
              (  "transforms the position and size of the player to something"
              ++ " that SDL is familiar with"
              )
        $ let shot             = createShot (V2 30.4 49.8) pi
              (destination, _) = toDrawableShot shot
          in  destination `shouldBe` toTextureArea (shotToCircle shot)

    describe "updateShot"
        $ it "updates the shot position from passed time and the shot velocity"
        $ updateShot 10 (createShot (V2 6 2) (pi / 6))
        `shouldBe` createShot
                       (V2 (6 + 10 * shotSpeed * sqrt 3 / 2)
                           (2 + 10 * shotSpeed / 2)
                       )
                       (pi / 6)

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

