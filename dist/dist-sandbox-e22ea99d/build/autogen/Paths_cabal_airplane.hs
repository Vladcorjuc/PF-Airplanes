{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_cabal_airplane (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/vlad/cabal-airplane/.cabal-sandbox/bin"
libdir     = "/home/vlad/cabal-airplane/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.2/cabal-airplane-0.1.0.0-5XzkWDfN5tQLqdCa21iwfE"
dynlibdir  = "/home/vlad/cabal-airplane/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/vlad/cabal-airplane/.cabal-sandbox/share/x86_64-linux-ghc-8.0.2/cabal-airplane-0.1.0.0"
libexecdir = "/home/vlad/cabal-airplane/.cabal-sandbox/libexec"
sysconfdir = "/home/vlad/cabal-airplane/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cabal_airplane_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cabal_airplane_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "cabal_airplane_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "cabal_airplane_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cabal_airplane_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cabal_airplane_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
