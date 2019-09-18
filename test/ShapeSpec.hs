module ShapeSpec where

import           Test.Hspec
import           SDL.Vect
import           Shape
import           Foreign.C.Types
import           SDL.Video.Renderer             ( Rectangle(..) )

spec :: Spec
spec = do
    describe "toDrawableShape" $ do
        it "converts from shape to something that can be drawn by SDL"
            $          do
                           toDrawableShape (Shape (V2 30.4 49.8) (V2 (-1) (-1)))
                                           45
                                           (V2 10 20)
                                           "texture-file-path"
            `shouldBe` ( "texture-file-path"
                       , Just (Rectangle (P (V2 25 40)) (V2 10 20))
                       , 45
                       )
