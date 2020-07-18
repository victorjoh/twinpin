module MenuUtil where

import           Menu
import           SDL                            ( KeyModifier(..) )

noKeyModifier :: KeyModifier
noKeyModifier = KeyModifier False
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

basicMenu :: String -> Menu String
basicMenu = Menu [] ["a", "b"]
