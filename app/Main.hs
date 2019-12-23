{-# LANGUAGE OverloadedStrings #-}

module Main where

import           SDL
import           SDL.Vect                       ( Point(..) )
import           Graphics.Rasterific     hiding ( V2(..) )
import qualified Graphics.Rasterific           as Rasterific
                                                ( V2(..) )
import           Graphics.Rasterific.Texture
import           Codec.Picture.Types
import           Control.Concurrent             ( threadDelay )
import           Game
import           Foreign.C.Types
import           Data.Word                      ( Word8 )
import           Paths_twinpin
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromJust )
import           Control.Monad

import           Codec.Picture                  ( PixelRGBA8(..)
                                                , writePng
                                                )
import qualified Data.Vector.Storable          as Data
                                                ( Vector )
import qualified Data.Vector.Storable.Mutable  as Data
                                                ( IOVector )
import           Data.Vector.Generic            ( thaw )

backgroundColor = V4 34 11 21
windowSize' = V2 800 600
frameInterval = 1667 -- this is smoother than 16667. Why is that? Since the
                     -- monitor refresh rate is 60 hz, 1/60 * 1000000 micro
                     -- seconds should be enough

main :: IO ()
main = do
    initialize [InitJoystick, InitVideo]
    window <- createWindow "twinpin"
                           defaultWindow { windowInitialSize = windowSize' }
    renderer <- createRenderer window (-1) defaultRenderer
    showWindow window
    joysticks <- availableJoysticks
    mapM_ openJoystick joysticks
    gameLoop renderer (createGame windowSize')

gameLoop :: Renderer -> Game -> IO ()
gameLoop renderer game = do
    currentTime <- fromIntegral <$> ticks
    events      <- pollEvents
    let newGame = updateGame events currentTime game
    rendererDrawColor renderer $= backgroundColor maxBound
    clear renderer
    mapM_ (draw renderer) $ toDrawableGame newGame
    present renderer
    timeSpent <- fmap (flip (-) currentTime . fromIntegral) ticks
    threadDelay $ frameInterval - timeSpent
    unless (isFinished newGame) (gameLoop renderer newGame)

draw :: Renderer -> (Rectangle CInt, Image PixelRGBA8) -> IO ()
draw renderer (destination, image) = do
    let rawImageData     = imageData image
        pitch            = 4 * fromIntegral (imageWidth image)
        Rectangle _ size = destination
    mutableVector <- thaw rawImageData
    surface       <- createRGBSurfaceFrom mutableVector size pitch ABGR8888
    texture       <- createTextureFromSurface renderer surface
    freeSurface surface
    copyEx renderer texture Nothing (Just destination) 0 Nothing
        $ V2 False False
    destroyTexture texture
