{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_HasCacBDD (
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

bindir     = "/home/javier/Dropbox/Carrera/Tesis/Codigos/Prueba Stack/HasCacBDD-0.1.0.0/.stack-work/install/x86_64-linux/lts-8.0/8.0.2/bin"
libdir     = "/home/javier/Dropbox/Carrera/Tesis/Codigos/Prueba Stack/HasCacBDD-0.1.0.0/.stack-work/install/x86_64-linux/lts-8.0/8.0.2/lib/x86_64-linux-ghc-8.0.2/HasCacBDD-0.1.0.0-9yTDjY7ApZf41Py31f2KZZ"
dynlibdir  = "/home/javier/Dropbox/Carrera/Tesis/Codigos/Prueba Stack/HasCacBDD-0.1.0.0/.stack-work/install/x86_64-linux/lts-8.0/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/javier/Dropbox/Carrera/Tesis/Codigos/Prueba Stack/HasCacBDD-0.1.0.0/.stack-work/install/x86_64-linux/lts-8.0/8.0.2/share/x86_64-linux-ghc-8.0.2/HasCacBDD-0.1.0.0"
libexecdir = "/home/javier/Dropbox/Carrera/Tesis/Codigos/Prueba Stack/HasCacBDD-0.1.0.0/.stack-work/install/x86_64-linux/lts-8.0/8.0.2/libexec"
sysconfdir = "/home/javier/Dropbox/Carrera/Tesis/Codigos/Prueba Stack/HasCacBDD-0.1.0.0/.stack-work/install/x86_64-linux/lts-8.0/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "HasCacBDD_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HasCacBDD_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "HasCacBDD_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "HasCacBDD_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HasCacBDD_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HasCacBDD_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
