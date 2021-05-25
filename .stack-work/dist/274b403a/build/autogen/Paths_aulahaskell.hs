{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_aulahaskell (
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
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\josepetri\\projetos\\petri\\.stack-work\\install\\685340ec\\bin"
libdir     = "C:\\Users\\josepetri\\projetos\\petri\\.stack-work\\install\\685340ec\\lib\\x86_64-windows-ghc-8.10.4\\aulahaskell-0.0.0-8dUJhVgNUruFRoL1UyicE3"
dynlibdir  = "C:\\Users\\josepetri\\projetos\\petri\\.stack-work\\install\\685340ec\\lib\\x86_64-windows-ghc-8.10.4"
datadir    = "C:\\Users\\josepetri\\projetos\\petri\\.stack-work\\install\\685340ec\\share\\x86_64-windows-ghc-8.10.4\\aulahaskell-0.0.0"
libexecdir = "C:\\Users\\josepetri\\projetos\\petri\\.stack-work\\install\\685340ec\\libexec\\x86_64-windows-ghc-8.10.4\\aulahaskell-0.0.0"
sysconfdir = "C:\\Users\\josepetri\\projetos\\petri\\.stack-work\\install\\685340ec\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "aulahaskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "aulahaskell_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "aulahaskell_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "aulahaskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "aulahaskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "aulahaskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)