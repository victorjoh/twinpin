module MenuSpec where

import           Test.Hspec
import           Menu
import           Shot
import           PlayerUtil
import           MenuUtil
import           SDL.Event
import           SDL

spec :: Spec
spec = do
    describe "drawMenu" $ do
        it "can draw the menu"
            $          fst (head $ drawMenu Resume)
            `shouldBe` Rectangle (P $ V2 690 333) (V2 540 414)
        it "can draw the selection when Resume is selected"
            $          fst (last $ drawMenu Resume)
            `shouldBe` fst (drawShot $ createShot (V2 807 540) 0 (-1))
        it "can draw the selection when Quit is selected"
            $          fst (last $ drawMenu Quit)
            `shouldBe` fst (drawShot $ createShot (V2 807 630) 0 (-1))

    describe "updateMenu" $ do
        it "moves the selection down when the left thumbstick is moved down"
            $ let stickDown = toEvent $ JoyAxisEventData 0 1 30000
              in  updateMenu [stickDown] Resume `shouldBe` Quit
        it "moves the selection up when the left thumbstick is moved up"
            $ let stickUp = toEvent $ JoyAxisEventData 0 1 $ -30000
              in  updateMenu [stickUp] Quit `shouldBe` Resume
        it
                (  "moves the selection down when down is pressed on the "
                ++ "directional buttons"
                )
            $ let dirDown = toEvent $ JoyHatEventData 0 0 HatDown
              in  updateMenu [dirDown] Resume `shouldBe` Quit
        it
                ("moves the selection up when up is pressed on the directional "
                ++ "buttons"
                )
            $ let dirUp = toEvent $ JoyHatEventData 0 0 HatUp
              in  updateMenu [dirUp] Quit `shouldBe` Resume
        it
                (  "moves the selection down when arrow down is pressed on the "
                ++ "keyboard"
                )
            $ let arrowDown = toEvent $ KeyboardEventData
                      Nothing
                      Pressed
                      False
                      (Keysym (Scancode 81) (Keycode 1073741905) noKeyModifier)
              in  updateMenu [arrowDown] Resume `shouldBe` Quit
        it
                (  "moves the selection up when arrow up is pressed on the "
                ++ "keyboard"
                )
            $ let arrowUp = toEvent $ KeyboardEventData
                      Nothing
                      Pressed
                      False
                      (Keysym (Scancode 82) (Keycode 1073741906) noKeyModifier)
              in  updateMenu [arrowUp] Quit `shouldBe` Resume
