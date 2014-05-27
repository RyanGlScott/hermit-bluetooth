{-# LANGUAGE CPP #-}
import Distribution.Simple

#if defined(mingw32_HOST_OS)
import Control.Monad

import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup

import System.Directory
import System.FilePath

dllFileName :: FilePath
dllFileName = "wsa_utils.dll"

dllSourceDir :: IO FilePath
dllSourceDir = do
    curDir <- getCurrentDirectory
    return $ curDir </> "lib"

dllSourcePath :: IO FilePath
dllSourcePath = do
    sourceDir <- dllSourceDir
    return $ sourceDir </> dllFileName

dllDestPath :: IO FilePath
dllDestPath = do
    cabalAppData <- getAppUserDataDirectory "cabal"
    return $ cabalAppData </> "bin" </> dllFileName

copyDll :: String -> FilePath -> FilePath -> IO ()
copyDll message source dest = do
    putStrLn message
    putStr "Copying... "
    copyFile source dest
    putStrLn "Done."

-- TODO: Make this process more composable with lenses
patchDesc :: FilePath -> PackageDescription -> PackageDescription
patchDesc source desc = let Just lib = library desc
                            lbi = libBuildInfo lib
                            newlbi = lbi { extraLibDirs = source : extraLibDirs lbi }
                        in desc { library = Just $ lib { libBuildInfo = newlbi } }
    
customBuild :: FilePath -> PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
customBuild source desc = buildHook simpleUserHooks $ patchDesc source desc

customInstall :: FilePath -> PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO ()
customInstall source desc = instHook simpleUserHooks $ patchDesc source desc

customPostConf :: FilePath -> Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
customPostConf source args conf desc linfo = postConf simpleUserHooks args conf (patchDesc source desc) linfo
#endif

main :: IO ()
main = do
#if defined(mingw32_HOST_OS)
    dest <- dllDestPath
    source <- dllSourcePath
    dllExists <- doesFileExist dest
    
    -- hermit-bluetooth.exe needs the DLL file to be in the PATH, so copy it to %APPDATA%\cabal\bin
    when (not dllExists) $ copyDll (dllFileName ++ " is not in application data.") source dest
    
    destTime <- getModificationTime dest
    sourceTime <- getModificationTime source
    
    -- Make sure the DLL is up-to-date
    when (destTime < sourceTime) $ copyDll (dllFileName ++ " is out-of-date.") source dest
    
    sourceDir <- dllSourceDir
    defaultMainWithHooks $ simpleUserHooks
        { buildHook = customBuild sourceDir
        , instHook = customInstall sourceDir
        , postConf = customPostConf sourceDir
        }
#elif defined(linux_HOST_OS)
    defaultMain
#endif