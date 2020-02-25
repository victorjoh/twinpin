{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumDecimals #-}

module DeployTwinpin where

import           System.Process                 ( callProcess
                                                , readProcess
                                                )
import qualified Codec.Archive.Tar             as Tar
import qualified Codec.Archive.Tar.Entry       as Tar
import qualified Codec.Compression.GZip        as GZip
import qualified Codec.Archive.Zip             as Zip
import           Data.ByteString.Lazy          as BS
                                                ( ByteString
                                                , writeFile
                                                , readFile
                                                )
import           System.FilePath                ( (</>)
                                                , (<.>)
                                                , hasTrailingPathSeparator
                                                )
import           System.Directory
import           System.Info                   as A
import           Data.String.Utils              ( strip )
import           Data.Foldable                  ( foldrM )
import           Data.Time.Clock
import           Data.Time.Calendar
import           Unsafe.Coerce
import           Data.List.Split                ( splitOn )
import           Data.List                      ( find )

twinpinVersion = "0.3.0"
licenseFile = "LICENSE"
buildExeName = "twinpin-exe"
deployExeName = "twinpin"

fontsDir = "fonts"
linuxReadmeSrc = "doc" </> "README-linux"
linuxReadmeTarget = "README"

type UnixTime = Integer
unixEpoch = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)

data Distribution = LinuxDistribution   [Tar.Entry]
                  | WindowsDistribution [Zip.Entry]

deployTwinpin :: IO ()
deployTwinpin = do
    dist <- case os of
        "linux"   -> return $ LinuxDistribution []
        "mingw32" -> return $ WindowsDistribution []
        _ ->
            fail
                $  os
                ++ " is unsupported. Only linux and mingw32 are supported."
    callProcess "stack" ["build"]
    distDir     <- strip <$> readProcess "stack" ["path", "--dist-dir"] ""
    currentTime <- toSeconds . (`diffUTCTime` unixEpoch) <$> getCurrentTime
    let exePath =
            distDir
                </> "build"
                </> buildExeName
                </> buildExeName
                ++  exePostfix dist
    dist <- addEntry currentTime exePath (deployExeName ++ exePostfix dist) dist
    dist <- addEntry currentTime
                     licenseFile
                     (licenseFile ++ txtPostfix dist)
                     dist
    fonts <-
        map (fontsDir </>)
        .   filter (not . hasTrailingPathSeparator)
        <$> Tar.getDirectoryContentsRecursive fontsDir
    dist <- foldrM (addEntrySamePath currentTime) dist fonts
    dist <- addSdlLibrary currentTime dist

    let deployDir = distDir </> "deploy"
    createDirectoryIfMissing True deployDir
    let targetPath =
            deployDir
                </> deployExeName
                <-> twinpinVersion
                <-> osName dist
                <-> arch
                <.> fileType dist
    BS.writeFile targetPath $ compress dist
    putStrLn "deployed twinpin to: "
    putStrLn targetPath

(<->) :: String -> String -> String
(<->) s1 s2 = s1 ++ "-" ++ s2

toSeconds :: NominalDiffTime -> Integer
toSeconds diffTime = div (unsafeCoerce diffTime) 1e12

exePostfix :: Distribution -> FilePath
exePostfix (LinuxDistribution   _) = ""
exePostfix (WindowsDistribution _) = ".exe"

txtPostfix :: Distribution -> FilePath
txtPostfix (LinuxDistribution   _) = ""
txtPostfix (WindowsDistribution _) = ".txt"

osName :: Distribution -> String
osName (LinuxDistribution   _) = "linux"
osName (WindowsDistribution _) = "windows"

addEntry :: UnixTime -> FilePath -> FilePath -> Distribution -> IO Distribution
addEntry modifiedTime srcPath entryPath (LinuxDistribution entries) = do
    entry <-
        either
                fail
                ( fmap (setModifiedTime (fromInteger modifiedTime))
                . Tar.packFileEntry srcPath
                )
            $ Tar.toTarPath False entryPath
    return $ LinuxDistribution $ entry : entries
    where setModifiedTime t e = e { Tar.entryTime = t }

addEntry currentTime srcPath entryPath (WindowsDistribution entries) = do
    contents <- BS.readFile srcPath
    let entry = Zip.toEntry entryPath currentTime contents
    return $ WindowsDistribution $ entry : entries

addEntrySamePath :: UnixTime -> FilePath -> Distribution -> IO Distribution
addEntrySamePath modifiedTime path = addEntry modifiedTime path path

addSdlLibrary :: UnixTime -> Distribution -> IO Distribution
-- for linux let the user install the sdl2 dependency
addSdlLibrary modifiedTime (LinuxDistribution entries) = addEntry
    modifiedTime
    linuxReadmeSrc
    linuxReadmeTarget
    (LinuxDistribution entries)
addSdlLibrary modifiedTime (WindowsDistribution entries) = do
    libDirs       <- readProcess "stack" ["path", "--extra-library-dirs"] ""
    extraLibFiles <-
        fmap concat $ mapM (traverseToSnd' listDirectory . strip) $ splitOn
            ","
            libDirs
    sdl2Src <-
        liftMaybe ("Failed to find SDL2.dll, looked in: " ++ show libDirs)
                  (return . uncurry (</>))
            $ find (("SDL2.dll" ==) . snd) extraLibFiles
    putStrLn $ "Distributing with SDL2 found at: " ++ sdl2Src
    addEntry modifiedTime sdl2Src "SDL2.dll" (WindowsDistribution entries)

liftMaybe :: String -> (t -> IO a) -> Maybe t -> IO a
liftMaybe message = maybe (fail message)

-- Same as Relude.Extra.Tuple traverseToSnd but one functor deeper
traverseToSnd' :: (Functor s, Functor t) => (a -> s (t b)) -> a -> s (t (a, b))
traverseToSnd' f a = fmap (a, ) <$> f a

fileType :: Distribution -> FilePath
fileType (LinuxDistribution   _) = "tar.gz"
fileType (WindowsDistribution _) = "zip"

compress :: Distribution -> ByteString
compress (LinuxDistribution entries) = GZip.compress $ Tar.write entries
compress (WindowsDistribution entries) =
    Zip.fromArchive $ foldr Zip.addEntryToArchive Zip.emptyArchive entries
