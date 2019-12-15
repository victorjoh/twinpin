import           Distribution.Simple
import           Codec.Picture                  ( writeBitmap )
import           Graphics.Svg                   ( loadSvgFile )
import           Graphics.Rasterific.Svg        ( loadCreateFontCache
                                                , renderSvgDocument
                                                )
import           Graphics.Text.TrueType         ( FontCache )
import           System.Directory               ( createDirectoryIfMissing )
import           System.Process                 ( readProcess )
import           System.FilePath                ( (</>) )
import           Data.String.Utils              ( strip )

loadRender :: FontCache -> FilePath -> FilePath -> (Int, Int) -> IO ()
loadRender fontCache svgPath bmpPath bmpSize = do
    f <- loadSvgFile svgPath
    case f of
        Nothing  -> putStrLn "Error while loading SVG"
        Just doc -> do
            (finalImage, _) <- renderSvgDocument fontCache (Just bmpSize) 96 doc
            writeBitmap bmpPath finalImage

main :: IO ()
main = do
    distDir   <- strip <$> readProcess "stack" ["path", "--dist-dir"] ""
    createDirectoryIfMissing True distDir
    fontCache <- loadCreateFontCache $ distDir </> "fonty-texture-cache"
    createDirectoryIfMissing False "textures"
    loadRender fontCache "images/player.svg" "textures/player.bmp" (128, 128)
    loadRender fontCache "images/shot.svg"     "textures/shot.bmp"     (11 , 11)
    loadRender fontCache "images/shot-hit.svg" "textures/shot-hit.bmp" (11 , 11)
    loadRender fontCache "images/pillar.svg"   "textures/pillar.bmp"   (96 , 96)
    defaultMain

