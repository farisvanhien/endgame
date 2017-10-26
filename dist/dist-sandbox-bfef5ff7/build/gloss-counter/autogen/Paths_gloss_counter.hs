{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_gloss_counter (
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

bindir     = "D:\\School\\fp\\8.2.1\\endgame\\.cabal-sandbox\\bin"
libdir     = "D:\\School\\fp\\8.2.1\\endgame\\.cabal-sandbox\\x86_64-windows-ghc-8.2.1\\gloss-counter-0.1.0.0-8CACllsMzzC4pB1ddLt1JX"
dynlibdir  = "D:\\School\\fp\\8.2.1\\endgame\\.cabal-sandbox\\x86_64-windows-ghc-8.2.1"
datadir    = "D:\\School\\fp\\8.2.1\\endgame\\.cabal-sandbox\\x86_64-windows-ghc-8.2.1\\gloss-counter-0.1.0.0"
libexecdir = "D:\\School\\fp\\8.2.1\\endgame\\.cabal-sandbox\\gloss-counter-0.1.0.0-8CACllsMzzC4pB1ddLt1JX\\x86_64-windows-ghc-8.2.1\\gloss-counter-0.1.0.0"
sysconfdir = "D:\\School\\fp\\8.2.1\\endgame\\.cabal-sandbox\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "gloss_counter_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "gloss_counter_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "gloss_counter_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "gloss_counter_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "gloss_counter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "gloss_counter_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
