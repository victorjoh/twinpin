module BulletSpec where

import Bullet
import Circle
import SDL.Vect (V2 (V2))
import Space (Bounds2D (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "drawBullet" $
    it
      ( "transforms the position and size of the player to something"
          ++ " that SDL is familiar with"
      )
      $ let bullet = createBullet (V2 30.4 49.8) pi 0
            (destination, _) = drawBullet bullet
         in destination `shouldBe` toTextureArea (bulletToCircle bullet)

  describe "moveBullet" $
    it
      ( "updates the bullet position from passed time and the "
          ++ "bullet velocity"
      )
      $ moveBullet 10 (createBullet (V2 6 2) (pi / 6) 0)
        `shouldBe` createBullet
          ( V2
              (6 + 10 * bulletSpeed * sqrt 3 / 2)
              (2 + 10 * bulletSpeed / 2)
          )
          (pi / 6)
          0

  describe "isBulletWithinBounds" $ do
    it "returns true if the bullet is within the borders" $
      isBulletWithinBounds
        (Bounds2D (0, 100) (100, 200))
        (createBullet (V2 50 150) 0 0)
        `shouldBe` True
    it "returns true if the bullet is on the border" $
      isBulletWithinBounds
        (Bounds2D (0, 100) (100, 200))
        (createBullet (V2 50 208) 0 0)
        `shouldBe` True
    it "returns false if the bullet is outside the border" $
      isBulletWithinBounds
        (Bounds2D (0, 100) (100, 200))
        (createBullet (V2 50 211) 0 0)
        `shouldBe` False
