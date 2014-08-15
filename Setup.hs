import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Distribution.System
import Distribution.Simple
import Distribution.Simple.Utils
import Distribution.Simple.Setup
import Distribution.Simple.Command
import Distribution.Simple.Program
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess           hiding (ppC2hs)

import Control.Exception
import Control.Monad
import System.Exit
import System.FilePath
import System.Directory
import System.Environment
import System.IO.Error                          hiding (catch)
import Prelude                                  hiding (catch)


-- Replicate the invocation of the postConf script, so that we can insert the
-- arguments of --extra-include-dirs and --extra-lib-dirs as paths in CPPFLAGS
-- and LDFLAGS into the environment
--
main :: IO ()
main = defaultMainWithHooks customHooks
  where
    preprocessors = hookedPreProcessors autoconfUserHooks
    customHooks   = autoconfUserHooks {
      preConf             = preConfHook,
      postConf            = postConfHook,
      hookedPreProcessors = ("chs",ppC2hs) : filter (\x -> fst x /= "chs") preprocessors
    }

    preConfHook :: Args -> ConfigFlags -> IO HookedBuildInfo
    preConfHook args flags = do
      let verbosity = fromFlag (configVerbosity flags)

      confExists <- doesFileExist "configure"
      unless confExists $ do
        code <- rawSystemExitCode verbosity "autoconf" []
        case code of
          ExitSuccess   -> return ()
          ExitFailure c -> die $ "autoconf exited with code " ++ show c

      preConf autoconfUserHooks args flags

    postConfHook :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
    postConfHook args flags pkg_descr lbi
      = let verbosity = fromFlag (configVerbosity flags)
        in do
          noExtraFlags args
          confExists <- doesFileExist "configure"
          if confExists
             then runConfigureScript verbosity False flags lbi
             else die "configure script not found."

          pbi <- getHookedBuildInfo verbosity
          let pkg_descr' = updatePackageDescription pbi pkg_descr
          postConf simpleUserHooks args flags pkg_descr' lbi


runConfigureScript :: Verbosity -> Bool -> ConfigFlags -> LocalBuildInfo -> IO ()
runConfigureScript verbosity backwardsCompatHack flags lbi = do
  env               <- getEnvironment
  (ccProg, ccFlags) <- configureCCompiler verbosity (withPrograms lbi)

  let env' = foldr appendToEnvironment env
               [("CC",       ccProg)
               ,("CFLAGS",   unwords ccFlags)
               ,("CPPFLAGS", unwords $ map ("-I"++) (configExtraIncludeDirs flags))
               ,("LDFLAGS",  unwords $ map ("-L"++) (configExtraLibDirs flags))
               ]

  handleNoWindowsSH $ rawSystemExitWithEnv verbosity "sh" args env'

  where
    args = "configure" : configureArgs backwardsCompatHack flags

    appendToEnvironment (key, val) [] = [(key, val)]
    appendToEnvironment (key, val) (kv@(k, v) : rest)
     | key == k  = (key, v ++ " " ++ val) : rest
     | otherwise = kv : appendToEnvironment (key, val) rest

    handleNoWindowsSH action
      | buildOS /= Windows
      = action

      | otherwise
      = action
          `catch` \ioe -> if isDoesNotExistError ioe
                              then die notFoundMsg
                              else throwIO ioe

    notFoundMsg = "The package has a './configure' script. This requires a "
               ++ "Unix compatibility toolchain such as MinGW+MSYS or Cygwin."


getHookedBuildInfo :: Verbosity -> IO HookedBuildInfo
getHookedBuildInfo verbosity = do
  maybe_infoFile <- defaultHookedPackageDesc
  case maybe_infoFile of
    Nothing       -> return emptyHookedBuildInfo
    Just infoFile -> do
      info verbosity $ "Reading parameters from " ++ infoFile
      readHookedBuildInfo verbosity infoFile


-- Replicate the default C2HS preprocessor hook here, and inject a value for
-- extra-c2hs-options, if it was present in the buildinfo file
--
-- Everything below copied from Distribution.Simple.PreProcess
--
ppC2hs :: BuildInfo -> LocalBuildInfo -> PreProcessor
ppC2hs bi lbi
    = PreProcessor {
        platformIndependent = False,
        runPreProcessor     = \(inBaseDir, inRelativeFile)
                               (outBaseDir, outRelativeFile) verbosity ->
          rawSystemProgramConf verbosity c2hsProgram (withPrograms lbi) . filter (not . null) $
            maybe [] words (lookup "x-extra-c2hs-options" (customFieldsBI bi))
            ++ ["--include=" ++ outBaseDir]
            ++ ["--cppopts=" ++ opt | opt <- getCppOptions bi lbi]
            ++ ["--output-dir=" ++ outBaseDir,
                "--output=" ++ outRelativeFile,
                inBaseDir </> inRelativeFile]
      }

getCppOptions :: BuildInfo -> LocalBuildInfo -> [String]
getCppOptions bi lbi
   =  ["-I" ++ dir | dir <- includeDirs bi]
   ++ [opt | opt@('-':c:_) <- ccOptions bi, c `elem` "DIU"]

