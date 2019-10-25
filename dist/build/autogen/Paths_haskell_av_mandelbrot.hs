{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_haskell_av_mandelbrot (
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

bindir     = "/home/vgaurav/.cabal/bin"
libdir     = "/home/vgaurav/.cabal/lib/x86_64-linux-ghc-8.0.2/haskell-av-mandelbrot-0.1.0.0-E50eNoUsP4KIY1HlqP0bno"
dynlibdir  = "/home/vgaurav/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/vgaurav/.cabal/share/x86_64-linux-ghc-8.0.2/haskell-av-mandelbrot-0.1.0.0"
libexecdir = "/home/vgaurav/.cabal/libexec"
sysconfdir = "/home/vgaurav/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_av_mandelbrot_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_av_mandelbrot_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "haskell_av_mandelbrot_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "haskell_av_mandelbrot_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_av_mandelbrot_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_av_mandelbrot_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
