module CircleSpec where

import Circle
import SDL.Vect (Point (P), V2 (V2))
import SDL.Video.Renderer (Rectangle (..))
import Space
import Test.Hspec

spec :: Spec
spec = do
  describe "toTextureArea" $
    it
      ( "transforms the position and size of the circle to "
          ++ "something that SDL is familiar with"
      )
      $ toTextureArea (Circle (V2 30 45) 5)
        `shouldBe` Rectangle (P (V2 25 40)) (V2 10 10)

  describe "areIntersecting" $ do
    it "returns true if two circles are intersected" $
      areIntersecting (Circle (V2 10 20) 3) (Circle (V2 12 23.4) 1)
    it "returns false if two circles are not intersected" $
      not $
        areIntersecting (Circle (V2 10 20) 3) (Circle (V2 12 23.5) 1)

  describe "moveCollidingCircle" $ do
    it "restricts position of the circle when colliding with bounds" $
      moveCollidingCircle
        (V2 10 10)
        1
        (Obstacles (Bounds2D (0, 10) (0, 20)) [])
        (Circle (V2 5 7) 1)
        `shouldBe` Circle (V2 9 17) 1
    it
      ( "does not keep the circle at the bounds when moving away from"
          ++ " the bounds"
      )
      $ moveCollidingCircle
        (V2 5 5)
        1
        (Obstacles (Bounds2D (0, 10) (0, 10)) [])
        (Circle (V2 1 1) 1)
        `shouldBe` Circle (V2 6 6) 1
    it
      ( "restricts position of the circle when colliding with another"
          ++ " circle from the top left"
      )
      $ moveCollidingCircle
        (V2 10 10)
        1
        ( Obstacles
            (Bounds2D (0, 1000) (0, 1000))
            [Circle (V2 200 100) 1]
        )
        (Circle (V2 195 95) 2)
        `shouldBe` Circle (V2 (200 - 3 / sqrt 2) (100 - 3 / sqrt 2)) 2
    it
      ( "restricts position of the circle when colliding vertically"
          ++ " with another circle"
      )
      $ moveCollidingCircle
        (V2 0 10)
        1
        ( Obstacles
            (Bounds2D (0, 1000) (0, 1000))
            [Circle (V2 200 100) 1]
        )
        (Circle (V2 200 95) 2)
        `shouldBe` Circle (V2 200 97) 2
    it
      ( "restricts position of the circle when colliding ALMOST"
          ++ " vertically with another circle"
      )
      $ moveCollidingCircle
        (V2 0.0001 10)
        1
        ( Obstacles
            (Bounds2D (0, 1000) (0, 1000))
            [Circle (V2 200 100) 1]
        )
        (Circle (V2 200 95) 2)
        `shouldBe` Circle (V2 200.00015 97) 2
    it
      ( "restricts position of the circle when colliding"
          ++ " horizontally with another circle"
      )
      $ moveCollidingCircle
        (V2 10 0)
        1
        ( Obstacles
            (Bounds2D (0, 1000) (0, 1000))
            [Circle (V2 200 100) 1]
        )
        (Circle (V2 195 100) 2)
        `shouldBe` Circle (V2 197 100) 2
    it
      ( "moves the player along the circle obstacle when colliding"
          ++ " with it at an angle"
      )
      $ moveCollidingCircle
        (V2 10 1)
        1
        ( Obstacles
            (Bounds2D (0, 1000) (0, 1000))
            [Circle (V2 200 100) 1]
        )
        (Circle (V2 195 100) 2)
        `shouldBe` Circle (V2 197.38324 101.46717) 2
    it
      ( "does not keep the circle at an obstacle when moving away"
          ++ " from it"
      )
      $ moveCollidingCircle
        (V2 (-10) 0)
        1
        ( Obstacles
            (Bounds2D (0, 1000) (0, 1000))
            [Circle (V2 200 100) 1]
        )
        (Circle (V2 197 100) 2)
        `shouldBe` Circle (V2 187 100) 2
    it
      ( "restricts position of the circle when colliding with the"
          ++ " bounds and then a circle"
      )
      $ moveCollidingCircle
        (V2 10 10)
        1
        ( Obstacles
            (Bounds2D (0, 200) (0, 200))
            [Circle (V2 195.5 100) 1]
        )
        (Circle (V2 198 95) 2)
        `shouldBe` Circle (V2 198 98.34169) 2
    it "can handle no velocity" $
      moveCollidingCircle
        (V2 0 0)
        1
        ( Obstacles
            (Bounds2D (0, 200) (0, 200))
            [Circle (V2 195.5 100) 1]
        )
        (Circle (V2 198 95) 2)
        `shouldBe` Circle (V2 198 95) 2
    it
      ( "restricts position of the circle when colliding with a"
          ++ " circle and then the bounds"
      )
      $ moveCollidingCircle
        (V2 0 100)
        1
        ( Obstacles
            (Bounds2D (0, 200) (0, 200))
            [Circle (V2 194 100) 5]
        )
        (Circle (V2 196 90) 1)
        `shouldBe` Circle (V2 199 96.68337) 1
    it
      ( "restricts position of the circle when colliding with a"
          ++ " circle and then another circle"
      )
      $ moveCollidingCircle
        (V2 0 (-4))
        100
        ( Obstacles
            (Bounds2D (0, 800) (0, 600))
            [Circle (V2 396 300) 1, Circle (V2 404 300) 1]
        )
        (Circle (V2 398 306) 4)
        `shouldBe` Circle (V2 400 303) 4
    it
      ( "restricts position of the circle when colliding with a"
          ++ " circle and then after leaving the circle before colliding"
          ++ " with the bounds"
      )
      $ moveCollidingCircle
        (V2 100 0)
        1
        ( Obstacles
            (Bounds2D (0, 120) (0, 120))
            [Circle (V2 100 100) 5]
        )
        (Circle (V2 90 104) 1)
        `shouldBe` Circle (V2 119 106) 1
    it
      ( "keeps the circle in position if it is pushing against"
          ++ " another circle and the bounds"
      )
      $ moveCollidingCircle
        (V2 (-0.2403) (-0.26086))
        22
        ( Obstacles
            (Bounds2D (0, 800) (0, 600))
            [Circle (V2 16.0 16.0) 16.0]
        )
        (Circle (V2 16.0 48.0) 16.0)
        `shouldBe` Circle (V2 16.0 48.0) 16.0
    it
      ( "lets the circle slide off another circle when it is wedged"
          ++ " between that other circle and the bounds given that"
          ++ " the movement direction is right"
      )
      $ moveCollidingCircle
        (V2 (-4) 0)
        100
        ( Obstacles
            (Bounds2D (-10.0, 8.0) (-100, 100))
            [Circle (V2 0 0) 1]
        )
        (Circle (V2 4 3) 4)
        `shouldBe` Circle (V2 (-6) 5) 4
    it
      ( "keeps the circle in position if it is pushing against"
          ++ " two other circles"
      )
      $ moveCollidingCircle
        (V2 0 (-4))
        100
        ( Obstacles
            (Bounds2D (0, 800) (0, 600))
            [Circle (V2 396 300) 1, Circle (V2 404 300) 1]
        )
        (Circle (V2 400 303) 4)
        `shouldBe` Circle (V2 400 303) 4
    it
      ( "lets the circle slide off one of the circles when it is"
          ++ " wedged between two other circles given that the"
          ++ " movement direction is right"
      )
      $ moveCollidingCircle
        (V2 (-4) 0)
        1000
        ( Obstacles
            (Bounds2D (0, 800) (0, 600))
            [Circle (V2 396 300) 1, Circle (V2 404 300) 1]
        )
        (Circle (V2 400 303) 4)
        `shouldBe` Circle (V2 4 305) 4
    it
      ( "moves the circle along the bounds when moving in to the"
          ++ " bounds, it does not matter that there is a circle along"
          ++ " the bounds further away"
      )
      $ moveCollidingCircle
        (V2 1 (-10))
        1
        ( Obstacles
            (Bounds2D (0, 800) (0, 600))
            [Circle (V2 799 300) 1]
        )
        (Circle (V2 799 500) 1)
        `shouldBe` Circle (V2 799 490) 1
    it
      ( "reduces the distance traveled for the moving circle when"
          ++ "hitting another circle at an angle"
      )
      $ moveCollidingCircle
        (V2 0 10)
        1
        ( Obstacles
            (Bounds2D (0, 800) (0, 600))
            [Circle (V2 400 300) 1]
        )
        (Circle (V2 403 295) 4)
        `shouldBe` Circle (V2 405 300.76352) 4
    it
      ( "keeps the circle in position if it is pushing against"
          ++ " two other circles (of different sizes)"
      )
      $ moveCollidingCircle
        (V2 0.0 (-100))
        21
        ( Obstacles
            (Bounds2D (0.0, 800.0) (0.0, 600.0))
            [ Circle (V2 144.0 144.0) 48.0,
              Circle (V2 53.0 144.0) 16.0
            ]
        )
        (Circle (V2 81.89259 157.61285) 16.0)
        `shouldBe` Circle (V2 81.62088 158.31242) 16.0
    it
      ( "lets the circle slide off one of two circles (of different"
          ++ " sizes) when it is wedged between them given that the"
          ++ " movement direction is right (1)"
      )
      $ moveCollidingCircle
        (V2 (-0.32768) 0)
        21
        ( Obstacles
            (Bounds2D (0.0, 800.0) (0.0, 600.0))
            [ Circle (V2 144.0 144.0) 48.0,
              Circle (V2 65.06139 144.48865) 16.0
            ]
        )
        (Circle (V2 81.83336 171.7412) 16.0)
        `shouldBe` Circle (V2 76.58978 174.33987) 16.0
    it
      ( "lets the circle slide off one of two circles (of different"
          ++ " sizes) when it is wedged between them given that the"
          ++ " movement direction is right (2)"
      )
      $ moveCollidingCircle
        (V2 (-0.51724803) 0.46882802)
        15
        ( Obstacles
            (Bounds2D (0.0, 1920.0) (0.0, 1080.0))
            [ Circle (V2 266.0 266.0) 86.0,
              Circle (V2 266.0 432.0) 30.0
            ]
        )
        (Circle (V2 238.4737 378.68674) 30.0)
        `shouldBe` Circle (V2 229.91446 384.06427) 30.0
