{-# LANGUAGE OverloadedStrings #-}

module Main where

import           SDL
import           SDL.Vect                       ( Point(..) )
import           Control.Concurrent             ( threadDelay )
import           Model
import           Foreign.C.Types
import           Data.Word                      ( Word8 )

main :: IO ()
main = do
    initialize [InitJoystick, InitVideo]
    window <- createWindow "twinpin"
                           defaultWindow { windowInitialSize = V2 800 600 }
    renderer <- createRenderer window (-1) defaultRenderer
    showWindow window
    joysticks <- availableJoysticks
    mapM_ openJoystick joysticks
    gameLoop renderer initModel

gameLoop :: Renderer -> Model -> IO ()
gameLoop renderer m = do
    threadDelay 15000
    events            <- pollEvents
    msSinceSdlLibInit <- ticks
    let newM = updateModel m events msSinceSdlLibInit
    rendererDrawColor renderer $= V4 0 0 0 maxBound
    clear renderer
    mapM_ (draw renderer) $ drawModel newM
    present renderer
    gameLoop renderer newM

draw :: Renderer -> (V4 Word8, Maybe (Rectangle CInt)) -> IO ()
draw renderer (color, rectangle) = do
    rendererDrawColor renderer $= color
    fillRect renderer rectangle
