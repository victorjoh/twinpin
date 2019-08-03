import           Distribution.Simple
import           Codec.Picture                  ( writeBitmap )
import           Graphics.Svg                   ( loadSvgFile )
import           Graphics.Rasterific.Svg        ( loadCreateFontCache
                                                , renderSvgDocument
                                                )
import           System.Directory               ( createDirectoryIfMissing )

loadRender :: FilePath -> FilePath -> IO ()
loadRender svgfilename bmpfilename = do
    f <- loadSvgFile svgfilename
    case f of
        Nothing  -> putStrLn "Error while loading SVG"
        Just doc -> do
            cache           <- loadCreateFontCache "gen/fonty-texture-cache"
            (finalImage, _) <- renderSvgDocument cache Nothing 96 doc
            writeBitmap bmpfilename finalImage

main :: IO ()
main = do
    createDirectoryIfMissing False "gen"
    loadRender "images/player.svg" "gen/player.bmp"
    defaultMain

