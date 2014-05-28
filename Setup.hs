{-# LANGUAGE CPP #-}
import Distribution.Simple

#if defined(mingw32_HOST_OS)
import Control.Monad

import Debug.Trace
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup

import System.Directory
import System.FilePath

dllFileName :: FilePath
dllFileName = "wsa_utils" <.> "dll"

dllSourceDir :: IO FilePath
dllSourceDir = do
    curDir <- getCurrentDirectory
    return $ curDir </> "lib"

dllSourcePath :: IO FilePath
dllSourcePath = do
    sourceDir <- dllSourceDir
    return $ sourceDir </> dllFileName

copyDll :: String -> FilePath -> FilePath -> IO ()
copyDll message sourcePath destPath = do
    putStrLn message
    putStr "Copying... "
    copyFile sourcePath destPath
    putStrLn "Done."

-- TODO: Make this process more composable with lenses
patchDesc :: FilePath -> PackageDescription -> PackageDescription
patchDesc sourceDir desc = let Just lib = library desc
                               lbi = libBuildInfo lib
                               newlbi = lbi { extraLibDirs = sourceDir : extraLibDirs lbi }
                           in desc { library = Just $ lib { libBuildInfo = newlbi } }
    
customBuild :: FilePath -> PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
customBuild sourceDir desc linfo hooks flags = do
    let installDir = bindir $ absoluteInstallDirs desc linfo NoCopyDest
        destPath = installDir </> dllFileName
    sourcePath <- dllSourcePath
    dllExists <- doesFileExist destPath
    
    -- hermit-bluetooth.exe needs the DLL file to be in the PATH, so copy it to %APPDATA%\cabal\bin (at least, that is probably where it will be)
    when (not dllExists) $ copyDll (dllFileName ++ " is not in application data.") sourcePath destPath
    
    destTime <- getModificationTime destPath
    sourceTime <- getModificationTime sourcePath
    
    -- Make sure the DLL is up-to-date
    when (destTime < sourceTime) $ copyDll (dllFileName ++ " is out-of-date.") sourcePath destPath
    
    buildHook simpleUserHooks (patchDesc sourceDir desc) linfo hooks flags

customInstall :: FilePath -> PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO ()
customInstall sourceDir desc = instHook simpleUserHooks $ patchDesc sourceDir desc

customPostConf :: FilePath -> Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
customPostConf sourceDir args conf desc linfo = postConf simpleUserHooks args conf (patchDesc sourceDir desc) linfo
#endif

main :: IO ()
main = do
#if defined(mingw32_HOST_OS)
    sourceDir <- dllSourceDir
    defaultMainWithHooks $ simpleUserHooks
        { buildHook = customBuild sourceDir
        , instHook = customInstall sourceDir
        , postConf = customPostConf sourceDir
        }
#elif defined(linux_HOST_OS)
    defaultMain
#endif