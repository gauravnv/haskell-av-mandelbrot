{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_fractal_hs (
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
version = Version [0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/vgaurav/.cabal/bin"
libdir     = "/home/vgaurav/.cabal/lib/x86_64-linux-ghc-8.0.2/fractal-hs-0.1-Amxu1j2MKDt3vHuy1ZKVBQ"
dynlibdir  = "/home/vgaurav/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/vgaurav/.cabal/share/x86_64-linux-ghc-8.0.2/fractal-hs-0.1"
libexecdir = "/home/vgaurav/.cabal/libexec"
sysconfdir = "/home/vgaurav/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "fractal_hs_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "fractal_hs_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "fractal_hs_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "fractal_hs_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "fractal_hs_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "fractal_hs_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
