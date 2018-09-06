module Main where

import Data.Maybe
import Distribution.PackageDescription    hiding (Flag)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import System.Directory

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  {
    preConf = makeExtLib
  , confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraLibDirs
  , postCopy = copyExtLib
  , postClean = cleanExtLib
  }

makeExtLib :: Args -> ConfigFlags -> IO HookedBuildInfo
makeExtLib _ flags = do
    let verbosity = fromFlag $ configVerbosity flags
    rawSystemExit verbosity "env" ["make", "--directory=src/cpp"]
    rawSystemExit verbosity "env" ["make", "--directory=src/c"]
    print $ "emptyHookedBuildInfo: " ++ (show emptyHookedBuildInfo)
    return emptyHookedBuildInfo

updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs localBuildInfo = do
    let myPackageDescription = localPkgDescr localBuildInfo
        lib = fromJust $ library myPackageDescription
        libBuild = libBuildInfo lib
    dir <- getCurrentDirectory
    print (dir ++ "/src/c")
    print $ "libBuild: " ++ show(libBuild)
    print $ "packageDescription: " ++ show(myPackageDescription)
    return localBuildInfo {
        localPkgDescr = myPackageDescription {
            library = Just $ lib {
                libBuildInfo = libBuild {
                    extraLibDirs = (dir ++ "/src/c") :
                        extraLibDirs libBuild
                }
            }
        }
    }

copyExtLib :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyExtLib _ flags pkg_descr lbi = do
    let libPref = libdir . absoluteInstallDirs pkg_descr lbi
                . fromFlag . copyDest
                $ flags
    let verbosity = fromFlag $ copyVerbosity flags
    print $ "prefijo libPref: " ++ (show libPref)
    rawSystemExit verbosity "cp" ["src/c/libCacBDD.a", libPref]

cleanExtLib :: Args -> CleanFlags -> PackageDescription -> () -> IO ()
cleanExtLib _ flags _ _ =
    let verbosity = fromFlag $ cleanVerbosity flags
    in do
      rawSystemExit verbosity "env" ["make", "--directory=src/c", "clean"]
      rawSystemExit verbosity "env" ["make", "--directory=src/cpp", "clean"]
