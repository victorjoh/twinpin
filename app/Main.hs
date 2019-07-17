module Main where

import           SDL
import           Control.Concurrent             ( threadDelay )

main :: IO ()
main = do
    initialize [InitJoystick]
    joysticks <- availableJoysticks
    mapM_ openJoystick joysticks
    printEvents

printEvents :: IO ()
printEvents = do
    threadDelay 15000
    events <- pollEvents
    mapM_ print events
    printEvents
