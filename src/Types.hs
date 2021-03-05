{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import qualified Data.ByteString.Lazy          as B
import           RIO
import           RIO.Process

type ReadProcessResult = (ExitCode, B.ByteString, B.ByteString)

-- TODO will eventually be extended?..
data GitOpResult = Updated | UpToDate | GeneralSuccess
data GitOpError =
  GitOpError
    { errorCode    :: !Int
    , errorMessage :: !B.ByteString
    }

data Options =
  Options
    { optionsDirectory      :: !FilePath
    , optionsRecursive      :: !Bool
    , optionsRecursiveDepth :: !Int
    , optionsMain           :: !Bool
    , optionsForce          :: !Bool
    , optionsExclude        :: !FilePath
    , optionsVerbose        :: !Bool
    }

data App =
  App
    { appLogFunc        :: !LogFunc
    , appProcessContext :: !ProcessContext
    , appOptions        :: !Options
    }

class HasDirectory env where
  directoryL :: Lens' env FilePath

class HasRecursive env where
  recursiveL :: Lens' env Bool

class HasRecursiveDepth env where
  recursiveDepthL :: Lens' env Int

class HasMain env where
  mainL :: Lens' env Bool

class HasForce env where
  forceL :: Lens' env Bool

class HasExclude env where
  excludeL :: Lens' env FilePath

instance HasDirectory App where
  directoryL = appOptionsL . optionsDirectoryL
   where
    appOptionsL = lens appOptions (\x y -> x { appOptions = y })
    optionsDirectoryL =
      lens optionsDirectory (\x y -> x { optionsDirectory = y })

instance HasRecursive App where
  recursiveL = appOptionsL . optionsRecursiveL
   where
    appOptionsL = lens appOptions (\x y -> x { appOptions = y })
    optionsRecursiveL =
      lens optionsRecursive (\x y -> x { optionsRecursive = y })

instance HasRecursiveDepth App where
  recursiveDepthL = appOptionsL . optionsRecursiveDepthL
   where
    appOptionsL = lens appOptions (\x y -> x { appOptions = y })
    optionsRecursiveDepthL =
      lens optionsRecursiveDepth (\x y -> x { optionsRecursiveDepth = y })

instance HasMain App where
  mainL = appOptionsL . optionsMainL
   where
    appOptionsL  = lens appOptions (\x y -> x { appOptions = y })
    optionsMainL = lens optionsMain (\x y -> x { optionsMain = y })

instance HasForce App where
  forceL = appOptionsL . optionsForceL
   where
    appOptionsL   = lens appOptions (\x y -> x { appOptions = y })
    optionsForceL = lens optionsForce (\x y -> x { optionsForce = y })

instance HasExclude App where
  excludeL = appOptionsL . optionsExcludeL
   where
    appOptionsL     = lens appOptions (\x y -> x { appOptions = y })
    optionsExcludeL = lens optionsExclude (\x y -> x { optionsExclude = y })

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasProcessContext App where
  processContextL =
    lens appProcessContext (\x y -> x { appProcessContext = y })
