import Codec.Archive.Tar.Entry
  ( getDirectoryContentsRecursive,
  )
import Codec.Archive.Zip
import Control.Monad (mapM_)
import Distribution.Simple
import Network.HTTP.Conduit (simpleHttp)
import System.Directory
import System.FilePath ((</>))

allerSource = "https://www.fontsquirrel.com/fonts/download/Aller"

allerFontsDir = "fonts" </> "Aller"

allerFiles = ["Aller_Rg.ttf", "Aller Font License.txt"]

main = do
  fontsExist <- doesDirectoryExist allerFontsDir
  currentContent <-
    if fontsExist
      then getDirectoryContentsRecursive allerFontsDir
      else return []
  if any (not . flip elem currentContent) allerFiles
    then do
      putStrLn $
        "Downloading fonts from "
          ++ allerSource
          ++ " to "
          ++ allerFontsDir
      allerZip <- toArchive <$> simpleHttp allerSource
      mapM_ (extractEntry allerZip allerFontsDir) allerFiles
    else
      putStrLn $
        "Skipped downloading Aller font, "
          ++ "all required files already exist in "
          ++ allerFontsDir
  defaultMain

extractEntry :: Archive -> FilePath -> FilePath -> IO ()
extractEntry archive destDir source = do
  entry <- maybe (fail failMsg) return $ findEntryByPath source archive
  writeEntry [OptDestination destDir] entry
  where
    failMsg =
      "Failed to find " ++ source ++ " in " ++ show (filesInArchive archive)
