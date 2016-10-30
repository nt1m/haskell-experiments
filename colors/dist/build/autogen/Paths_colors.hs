{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_colors (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/admin/Library/Haskell/bin"
libdir     = "/Users/admin/Library/Haskell/ghc-8.0.1-x86_64/lib/colors-0.1.0.0"
datadir    = "/Users/admin/Library/Haskell/share/ghc-8.0.1-x86_64/colors-0.1.0.0"
libexecdir = "/Users/admin/Library/Haskell/libexec"
sysconfdir = "/Users/admin/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "colors_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "colors_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "colors_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "colors_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "colors_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)