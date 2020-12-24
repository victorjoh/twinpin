module MenuUtil where

import Menu
import SDL (KeyModifier (..))

noKeyModifier :: KeyModifier
noKeyModifier =
  KeyModifier
    False
    False
    False
    False
    False
    False
    False
    False
    False
    False
    False

getSelection :: Menu -> Selection
getSelection (Menu _ _ selection) = selection

basicMenu :: Selection -> Menu
basicMenu = Menu [] ""
