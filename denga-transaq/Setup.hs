import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import System.Directory


updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs localBuildInfo = do
    let packageDescription = localPkgDescr localBuildInfo
        Just lib = library packageDescription
        libBuild = libBuildInfo lib
    dir <- getCurrentDirectory
    return localBuildInfo {
        localPkgDescr = packageDescription {
            library = Just $ lib {
                libBuildInfo = libBuild {
                    extraLibDirs = (dir ++ "\\lib") : extraLibDirs libBuild,
                    extraLibs = ("txmlconnector64") : extraLibs libBuild
                }
            }
        }
    }

main = defaultMainWithHooks simpleUserHooks { confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraLibDirs}
