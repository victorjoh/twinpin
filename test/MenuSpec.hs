module MenuSpec where

import Bullet
import Menu
import MenuUtil
import PlayerUtil
import SDL
import Test.Hspec

spec :: Spec
spec = do
  describe "drawMenu" $ do
    it "can draw the menu" $
      fst (head $ drawMenu $ basicMenu Continue)
        `shouldBe` Rectangle (P $ V2 690 333) (V2 540 414)
    it "can draw the selection when Continue is selected" $
      fst (last $ drawMenu $ basicMenu Continue)
        `shouldBe` fst (drawBullet $ createBullet (V2 807 540) 0 (-1))
    it "can draw the selection when Quit is selected" $
      fst (last $ drawMenu $ basicMenu Quit)
        `shouldBe` fst (drawBullet $ createBullet (V2 807 630) 0 (-1))

  describe "updateSelection" $ do
    it "moves the selection down when the left thumbstick is moved down" $
      let stickDown = toEvent $ JoyAxisEventData 0 1 30000
       in getSelection
            (updateSelection [stickDown] $ basicMenu Continue)
            `shouldBe` Quit
    it "moves the selection up when the left thumbstick is moved up" $
      let stickUp = toEvent $ JoyAxisEventData 0 1 $ -30000
       in getSelection (updateSelection [stickUp] $ basicMenu Quit)
            `shouldBe` Continue
    it
      ( "moves the selection down when down is pressed on the "
          ++ "directional buttons"
      )
      $ let dirDown = toEvent $ JoyHatEventData 0 0 HatDown
         in getSelection (updateSelection [dirDown] $ basicMenu Continue)
              `shouldBe` Quit
    it
      ( "moves the selection up when up is pressed on the directional "
          ++ "buttons"
      )
      $ let dirUp = toEvent $ JoyHatEventData 0 0 HatUp
         in getSelection (updateSelection [dirUp] $ basicMenu Quit)
              `shouldBe` Continue
    it
      ( "moves the selection down when arrow down is pressed on the "
          ++ "keyboard"
      )
      $ let arrowDown =
              toEvent $
                KeyboardEventData
                  Nothing
                  Pressed
                  False
                  (Keysym (Scancode 81) (Keycode 1073741905) noKeyModifier)
         in getSelection
              (updateSelection [arrowDown] $ basicMenu Continue)
              `shouldBe` Quit
    it
      ( "moves the selection up when arrow up is pressed on the "
          ++ "keyboard"
      )
      $ let arrowUp =
              toEvent $
                KeyboardEventData
                  Nothing
                  Pressed
                  False
                  (Keysym (Scancode 82) (Keycode 1073741906) noKeyModifier)
         in getSelection (updateSelection [arrowUp] $ basicMenu Quit)
              `shouldBe` Continue
