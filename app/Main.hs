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

backgroundColor = V4 34 11 21
windowSize' = V2 800 600

main :: IO ()
main = do
    initialize [InitJoystick, InitVideo]
    window <- createWindow "twinpin"
                           defaultWindow { windowInitialSize = windowSize' }
    renderer <- createRenderer window (-1) defaultRenderer
    showWindow window

    joysticks <- availableJoysticks
    mapM_ openJoystick joysticks

    textureMap <- foldM (appendTexture renderer) Map.empty getModelImages
    mapM_ (print . fst) (Map.toList textureMap)
    gameLoop renderer $ createModel textureMap

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
gameLoop renderer model = do
    threadDelay 20000
    events            <- pollEvents
    msSinceSdlLibInit <- ticks
    let newModel = updateModel model events msSinceSdlLibInit windowSize'
    rendererDrawColor renderer $= backgroundColor maxBound
    clear renderer
    mapM_ (draw renderer) $ drawModel newModel
    present renderer
    gameLoop renderer newModel

draw :: Renderer -> (Texture, Maybe (Rectangle CInt), CDouble) -> IO ()
draw renderer (texture, destination, rotation) =
    copyEx renderer texture Nothing destination rotation Nothing
        $ V2 False False
