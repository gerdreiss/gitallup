{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import           RIO
import           RIO.Process

-- | Command line arguments
data Options =
  Options
    { optionsDirectory :: !FilePath
    , optionsMaster    :: !Bool
    , optionsVerbose   :: !Bool
    }

data App =
  App
    { appLogFunc        :: !LogFunc
    , appProcessContext :: !ProcessContext
    , appOptions        :: !Options
    }

class HasDirectory env where
  directoryL :: Lens' env FilePath

class HasMaster env where
  masterL :: Lens' env Bool

instance HasDirectory App where
  directoryL = appOptionsL . optionsDirectoryL
    where
      appOptionsL = lens appOptions (\x y -> x {appOptions = y})
      optionsDirectoryL =
        lens optionsDirectory (\x y -> x {optionsDirectory = y})

instance HasMaster App where
  masterL = appOptionsL . optionsMasterL
    where
      appOptionsL = lens appOptions (\x y -> x {appOptions = y})
      optionsMasterL = lens optionsMaster (\x y -> x {optionsMaster = y})

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})
