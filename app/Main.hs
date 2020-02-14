{-# LANGUAGE OverloadedStrings #-}

module Main where

import           SDL
import           SDL.Raw.Types                  ( JoystickID )
import           Codec.Picture.Types
import           Control.Concurrent             ( threadDelay )
import           Game
import           Visual                         ( ImageId
                                                , backgroundColorSDL
                                                )
import           Space                          ( Time
                                                , DeltaTime
                                                )
import           Foreign.C.Types                ( CInt )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromJust
                                                , mapMaybe
                                                )
import           Control.Monad                  ( unless
                                                , (<=<)
                                                )
import           Codec.Picture                  ( PixelRGBA8(..) )
import           Data.Vector.Generic            ( thaw )
import           Graphics.Text.TrueType         ( loadFontFile )
import           System.FilePath                ( (</>) )
import           Data.List                      ( nub )

maxFps = 60
frameInterval = round $ 1000000 / maxFps -- microseconds

main :: IO ()
main = do
    initialize [InitJoystick, InitVideo]
    window <- createWindow "twinpin"
                        -- for screenshots
                        -- defaultWindow { windowInitialSize = V2 882 496 }
                           defaultWindow { windowMode = FullscreenDesktop }
    renderer <- createRenderer window (-1) defaultRenderer
    showWindow window
    font <- either fail return
        =<< loadFontFile ("fonts" </> "Aller" </> "Aller_Rg.ttf")
    winSize             <- get $ windowSize window
    preRenderedTextures <- mapM
        (toTexture renderer)
        (Map.fromList $ getStaticImages font winSize)
    gameLoop renderer preRenderedTextures winSize createGame
    mapM_ destroyTexture preRenderedTextures

gameLoop :: Renderer -> Map.Map ImageId Texture -> V2 CInt -> Game -> IO ()
gameLoop renderer preRenderedTextures winSize game = do
    updateTime <- currentTime
    events     <- pollEvents
    -- unless (null events) $ print events
    -- printFps lastTime updateTime
    let updatedGame = updateGame events updateTime game
    showFrame renderer preRenderedTextures $ drawGame winSize updatedGame
    addedJoysticks <- openJoysticks $ getAddedDevices events
    let newGame = assignJoysticks addedJoysticks updatedGame
    timeSpent <- currentTime `timeDifference` updateTime
    threadDelay $ frameInterval - (timeSpent * 1000)
    unless (isFinished newGame)
        $ gameLoop renderer preRenderedTextures winSize newGame

showFrame
    :: Renderer
    -> Map.Map String Texture
    -> [(Rectangle CInt, Either (Image PixelRGBA8) ImageId)]
    -> IO ()
showFrame renderer preRenderedTextures images = do
    rendererDrawColor renderer $= backgroundColorSDL
    clear renderer
    mapM_ (showImage renderer preRenderedTextures) images
    present renderer

-- time since game start in milliseconds
currentTime :: IO Time
currentTime = fromIntegral <$> ticks

timeDifference :: IO Time -> Time -> IO DeltaTime
timeDifference current previous = flip (-) previous <$> current

showImage
    :: Renderer
    -> Map.Map String Texture
    -> (Rectangle CInt, Either (Image PixelRGBA8) ImageId)
    -> IO ()
showImage renderer preRenderedTextures (destination, generatedOrStatic) =
    either
        (\image -> do
            texture <- toTexture renderer image
            drawInWindow texture
            destroyTexture texture
        )
        (\imageId ->
            drawInWindow $ fromJust $ Map.lookup imageId preRenderedTextures
        )
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

getAddedDevices :: [Event] -> [JoystickDevice]
getAddedDevices events = nub $ mapMaybe addedEventToJoystickDevice events
  where
    addedEventToJoystickDevice (Event _ payload) = case payload of
        JoyDeviceEvent (JoyDeviceEventData JoyDeviceAdded deviceId) ->
            Just $ JoystickDevice "" (fromIntegral deviceId)
        _ -> Nothing

-- TODO: do this in a different thread since openJoystick takes a lot of time
openJoysticks :: [JoystickDevice] -> IO [JoystickID]
openJoysticks = mapM $ getJoystickID <=< openJoystick

printFps :: Time -> Time -> IO ()
printFps prev curr =
    let diff = curr - prev
        fps  = round $ 1000 / fromIntegral diff
    in  putStrLn
            $  "time: "
            ++ show curr
            ++ " ms\t\tinterval: "
            ++ show diff
            ++ " ms\t\tfps: "
            ++ show fps
            ++ " Hz"
