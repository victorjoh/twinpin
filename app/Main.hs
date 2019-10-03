{-# LANGUAGE OverloadedStrings #-}

module Main where

import           SDL
import           SDL.Vect                       ( Point(..) )
import           Control.Concurrent             ( threadDelay )
import           Game
import           Foreign.C.Types
import           Data.Word                      ( Word8 )
import           Paths_twinpin
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromJust )
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

    textureMap <- foldM (appendTexture renderer) Map.empty gameTextureFiles
    mapM_ (print . fst) (Map.toList textureMap)
    gameLoop renderer (createGame windowSize') textureMap

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

gameLoop :: Renderer -> Game -> Map.Map FilePath Texture -> IO ()
gameLoop renderer game textureMap = do
    threadDelay 20000
    events            <- pollEvents
    msSinceSdlLibInit <- ticks
    let newGame = updateGame game events msSinceSdlLibInit windowSize'
    rendererDrawColor renderer $= backgroundColor maxBound
    clear renderer
    mapM_ (draw renderer textureMap) $ toDrawableGame newGame
    present renderer
    unless (isFinished newGame) (gameLoop renderer newGame textureMap)

draw
    :: Renderer
    -> Map.Map FilePath Texture
    -> (FilePath, Maybe (Rectangle CInt), CDouble)
    -> IO ()
draw renderer textureMap (textureFile, destination, rotation) =
    copyEx renderer
           (fromJust (Map.lookup textureFile textureMap))
           Nothing
           destination
           rotation
           Nothing
        $ V2 False False
