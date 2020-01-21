{-# LANGUAGE OverloadedStrings #-}

module Main where

import           SDL
import           Codec.Picture.Types
import           Control.Concurrent             ( threadDelay )
import           Game
import           Visual                         ( backgroundColorSDL )
import           Foreign.C.Types                ( CInt )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromJust )
import           Control.Monad                  ( unless )
import           Codec.Picture                  ( PixelRGBA8(..) )
import           Data.Vector.Generic            ( thaw )
import           Graphics.Text.TrueType         ( loadFontFile )
import           System.FilePath                ( (</>) )

windowSize' = V2 800 600
frameInterval = 6944 -- this is smoother than 16667. Why is that? Since the
                     -- monitor refresh rate is 60 hz, 1/60 * 1000000 micro
                     -- seconds should be enough

main :: IO ()
main = do
    initialize [InitJoystick, InitVideo]
    window <- createWindow "twinpin"
                           defaultWindow { windowMode = FullscreenDesktop }
    renderer <- createRenderer window (-1) defaultRenderer
    showWindow window
    joysticks <- availableJoysticks
    mapM_ openJoystick joysticks
    font <- either fail return
        =<< loadFontFile ("fonts" </> "Aller" </> "Aller_Rg.ttf")
    winSize    <- get $ windowSize window
    textureMap <- mapM (toTexture renderer)
                       (Map.fromList $ getStaticImages font winSize)
    gameLoop renderer textureMap winSize createGame
    mapM_ destroyTexture textureMap

gameLoop :: Renderer -> Map.Map String Texture -> V2 CInt -> Game -> IO ()
gameLoop renderer textureMap winSize game = do
    currentTime <- fromIntegral <$> ticks
    events      <- pollEvents
    -- unless (null events) $ print events
    let newGame = updateGame events currentTime game
    rendererDrawColor renderer $= backgroundColorSDL
    clear renderer
    mapM_ (draw renderer textureMap) $ drawGame winSize newGame
    present renderer
    timeSpent <- fmap (flip (-) currentTime . fromIntegral) ticks
    threadDelay $ frameInterval - timeSpent
    unless (isFinished newGame) (gameLoop renderer textureMap winSize newGame)

draw
    :: Renderer
    -> Map.Map String Texture
    -> (Rectangle CInt, Either (Image PixelRGBA8) String)
    -> IO ()
draw renderer textureMap (destination, generatedOrStatic) = either
    (\image -> do
        texture <- toTexture renderer image
        drawInWindow texture
        destroyTexture texture
    )
    (\imageId -> drawInWindow $ fromJust $ Map.lookup imageId textureMap)
    generatedOrStatic
  where
    drawInWindow tex =
        copyEx renderer tex Nothing (Just destination) 0 Nothing
            $ V2 False False

-- the texture should be destroyed by the caller
toTexture :: Renderer -> Image PixelRGBA8 -> IO Texture
toTexture renderer image = do
    let rawImageData = imageData image
        width        = fromIntegral $ imageWidth image
        height       = fromIntegral $ imageHeight image
        size         = V2 width height
        pitch        = 4 * width
    mutableVector <- thaw rawImageData
    surface       <- createRGBSurfaceFrom mutableVector size pitch ABGR8888
    texture       <- createTextureFromSurface renderer surface
    freeSurface surface
    return texture
