{-# LANGUAGE OverloadedStrings #-}

module Main where

import           SDL
import           SDL.Vect                       ( Point(..) )
import           Control.Concurrent             ( threadDelay )
import           Model
import           Foreign.C.Types
import           Data.Word                      ( Word8 )
import           Paths_twinpin
import qualified Data.Map.Strict               as Map
import           Control.Monad

main :: IO ()
main = do
    initialize [InitJoystick, InitVideo]
    window <- createWindow "twinpin"
                           defaultWindow { windowInitialSize = V2 800 600 }
    renderer <- createRenderer window (-1) defaultRenderer
    showWindow window

    joysticks <- availableJoysticks
    mapM_ openJoystick joysticks

    textureMap <- foldM (appendTexture renderer) Map.empty getModelImages
    gameLoop renderer $ initModel textureMap

appendTexture
    :: Renderer
    -> Map.Map FilePath Texture
    -> FilePath
    -> IO (Map.Map FilePath Texture)
appendTexture renderer textureMap textureFile = do
    filePath <- getDataFileName textureFile
    surface  <- loadBMP filePath
    texture  <- createTextureFromSurface renderer surface
    freeSurface surface
    return $ Map.insert textureFile texture textureMap

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

draw :: Renderer -> (Texture, Maybe (Rectangle CInt)) -> IO ()
draw renderer (texture, destination) = copy renderer texture Nothing destination
