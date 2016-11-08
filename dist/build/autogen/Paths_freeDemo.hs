module Paths_freeDemo (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/matt/.cabal/bin"
libdir     = "/Users/matt/.cabal/lib/x86_64-osx-ghc-7.10.2/freeDemo-0.1.0.0-G5jSDknexjWFVzRHmfeO3Y"
datadir    = "/Users/matt/.cabal/share/x86_64-osx-ghc-7.10.2/freeDemo-0.1.0.0"
libexecdir = "/Users/matt/.cabal/libexec"
sysconfdir = "/Users/matt/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "freeDemo_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "freeDemo_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "freeDemo_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "freeDemo_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "freeDemo_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
