import           Distribution.Simple
import           Codec.Picture                  ( writeBitmap )
import           Graphics.Svg                   ( loadSvgFile )
import           Graphics.Rasterific.Svg        ( loadCreateFontCache
                                                , renderSvgDocument
                                                )
import           System.Directory               ( createDirectoryIfMissing )

loadRender :: FilePath -> FilePath -> (Int, Int) -> IO ()
loadRender svgPath bmpPath bmpSize = do
    f <- loadSvgFile svgPath
    case f of
        Nothing  -> putStrLn "Error while loading SVG"
        Just doc -> do
            cache           <- loadCreateFontCache "gen/fonty-texture-cache"
            (finalImage, _) <- renderSvgDocument cache (Just bmpSize) 96 doc
            writeBitmap bmpPath finalImage

main :: IO ()
main = do
    createDirectoryIfMissing False "gen"
    loadRender "images/player.svg" "gen/player.bmp" (128, 128)
    defaultMain

