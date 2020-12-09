{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import qualified Data.ByteString.Lazy          as B
import           RIO
import           RIO.Process

type ReadProcessResult = (ExitCode, B.ByteString, B.ByteString)

data Options =
  Options
    { optionsDirectory      :: !FilePath
    , optionsRecursive      :: !Bool
    , optionsRecursiveDepth :: !Int
    , optionsMaster         :: !Bool
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

class HasMaster env where
  masterL :: Lens' env Bool

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

instance HasMaster App where
  masterL = appOptionsL . optionsMasterL
   where
    appOptionsL    = lens appOptions (\x y -> x { appOptions = y })
    optionsMasterL = lens optionsMaster (\x y -> x { optionsMaster = y })

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
