import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import System.Directory

{-}
updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs localBuildInfo = do
    let packageDescription = localPkgDescr localBuildInfo
        lib = fromJust $ library packageDescription
        libBuild = libBuildInfo lib
    dir <- getCurrentDirectory
    return localBuildInfo {
        localPkgDescr = packageDescription {
            library = Just $ lib {
                libBuildInfo = libBuild {
                    extraLibDirs = (dir ++ "\\lib") : extraLibDirs libBuild,
                    extraLibs = ("txmlconnector64.dll") : extraLibs libBuild
                }
            }
        }
    }
-}

updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs localBuildInfo = do
    let packageDescription = localPkgDescr localBuildInfo
        test = head $ testSuites packageDescription
        testBuild = testBuildInfo  test
    dir <- getCurrentDirectory
    return localBuildInfo {
        localPkgDescr = packageDescription {
            testSuites = [ test {
                testBuildInfo = testBuild {
                    extraLibDirs = (dir ++ "\\lib") : extraLibDirs testBuild,
                    extraLibs = ("txmlconnector64") : extraLibs testBuild
                }
            } ]
        }
    }


main = defaultMainWithHooks simpleUserHooks { confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraLibDirs}

--main = defaultMain