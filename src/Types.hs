{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import qualified Data.ByteString.Lazy          as B
import           RIO
import           RIO.Process

type ReadProcessResult = (ExitCode, B.ByteString, B.ByteString)

data Options =
  Options
    { optionsDirectory :: !FilePath
    , optionsRecursive :: !Bool
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

class HasRecursive env where
  recursiveL :: Lens' env Bool

class HasMaster env where
  masterL :: Lens' env Bool

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

instance HasMaster App where
  masterL = appOptionsL . optionsMasterL
   where
    appOptionsL    = lens appOptions (\x y -> x { appOptions = y })
    optionsMasterL = lens optionsMaster (\x y -> x { optionsMaster = y })

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasProcessContext App where
  processContextL =
    lens appProcessContext (\x y -> x { appProcessContext = y })
