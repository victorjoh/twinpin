module Main where

import           SDL
import           Control.Concurrent             ( threadDelay )
import           Model

main :: IO ()
main = do
    initialize [InitJoystick]
    joysticks <- availableJoysticks
    mapM_ openJoystick joysticks
    gameLoop initModel

gameLoop :: Model -> IO ()
gameLoop m = do
    threadDelay 15000
    events <- pollEvents
    --mapM_ print events
    msSinceSdlLibInit <- ticks
    let newM = updateModel m events msSinceSdlLibInit
    print newM
    gameLoop newM
