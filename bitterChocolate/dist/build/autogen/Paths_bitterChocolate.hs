module Paths_bitterChocolate (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/anthony/Documents/programming/hackerRank/bitterChocolate/.cabal-sandbox/bin"
libdir     = "/Users/anthony/Documents/programming/hackerRank/bitterChocolate/.cabal-sandbox/lib/x86_64-osx-ghc-7.8.3/bitterChocolate-0.1.0.0"
datadir    = "/Users/anthony/Documents/programming/hackerRank/bitterChocolate/.cabal-sandbox/share/x86_64-osx-ghc-7.8.3/bitterChocolate-0.1.0.0"
libexecdir = "/Users/anthony/Documents/programming/hackerRank/bitterChocolate/.cabal-sandbox/libexec"
sysconfdir = "/Users/anthony/Documents/programming/hackerRank/bitterChocolate/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "bitterChocolate_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bitterChocolate_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "bitterChocolate_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bitterChocolate_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "bitterChocolate_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
