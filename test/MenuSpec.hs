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
            $               map fst (drawMenu Resume)
            `shouldContain` [Rectangle (P (V2 250 185)) (V2 300 230)]
        it "can draw the selection when Resume is selected"
            $               map fst (drawMenu Resume)
            `shouldContain` [fst $ drawShot $ createShot (V2 315 300) 0]
        it "can draw the selection when Quit is selected"
            $               map fst (drawMenu Quit)
            `shouldContain` [fst $ drawShot $ createShot (V2 315 350) 0]

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
