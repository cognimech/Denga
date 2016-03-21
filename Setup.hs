import Distribution.Simple


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

main = defaultMainWithHooks simpleUserHooks { confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraLibDirs }
