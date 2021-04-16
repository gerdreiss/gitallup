module Types where

import qualified Data.ByteString.Lazy.Char8    as C8     -- TODO replace this with RIO's package or function
import qualified RIO.ByteString.Lazy           as B

import           RIO
import           RIO.Process                    ( HasProcessContext(..)
                                                , ProcessContext
                                                )


--
--
-- Types and data structures
--

type ReadProcessResult = (ExitCode, B.ByteString, B.ByteString)

data GitOpResultType
  = Clean
  | Dirty
  | UpToDate
  | Updated
  | Reset
  | GeneralSuccess
  deriving (Eq, Generic)

data GitOpResult = GitOpResult
  { resultType :: !GitOpResultType
  , resultText :: !B.ByteString
  }
  deriving (Eq, Generic)

data GitOpError = GitOpError
  { errorCode    :: !Int
  , errorMessage :: !B.ByteString
  }
  deriving (Eq, Generic)

data RepoUpdateResult = RepoUpdateResult
  { updateResultRepo     :: !FilePath
  , updateResultBranch   :: !(Maybe B.ByteString)
  , updateErrorOrSuccess :: !(Either GitOpError GitOpResult)
  }
  deriving (Eq, Generic)

data Options = Options
  { optionsDirectory      :: !FilePath
  , optionsRecursive      :: !Bool
  , optionsRecursiveDepth :: !Int
  , optionsStatus         :: !Bool
  , optionsMain           :: !Bool
  , optionsForce          :: !Bool
  , optionsExclude        :: !FilePath
  , optionsActions        :: Maybe FilePath
  , optionsVerbose        :: !Bool
  }

data App = App
  { appLogFunc        :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions        :: !Options
  }

--
--
-- Type classes for lenses
--

class HasDirectory env where
  directoryL :: Lens' env FilePath

class HasRecursive env where
  recursiveL :: Lens' env Bool

class HasRecursiveDepth env where
  recursiveDepthL :: Lens' env Int

class HasStatus env where
  statusL :: Lens' env Bool

class HasMain env where
  mainL :: Lens' env Bool

class HasForce env where
  forceL :: Lens' env Bool

class HasExclude env where
  excludeL :: Lens' env FilePath

class HasActions env where
  actionsL :: Lens' env (Maybe FilePath)

class HasUserHome env where
  userHomeL :: Lens' env FilePath

-- 
-- 
-- Instances
--
instance NFData RepoUpdateResult
instance NFData GitOpResultType
instance NFData GitOpResult
instance NFData GitOpError

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

instance HasStatus App where
  statusL = appOptionsL . optionsStatusL
   where
    appOptionsL    = lens appOptions (\x y -> x { appOptions = y })
    optionsStatusL = lens optionsStatus (\x y -> x { optionsStatus = y })

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

instance HasActions App where
  actionsL = appOptionsL . optionsActionsL
   where
    appOptionsL     = lens appOptions (\x y -> x { appOptions = y })
    optionsActionsL = lens optionsActions (\x y -> x { optionsActions = y })

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasProcessContext App where
  processContextL =
    lens appProcessContext (\x y -> x { appProcessContext = y })

instance Show GitOpResultType where
  show Clean          = " has nothing to commit."
  show Dirty          = " has uncommitted changes."
  show Updated        = " updated successfully."
  show Reset          = " reset succesfully."
  show UpToDate       = " already up to date."
  show GeneralSuccess = " operation successful, whatever it was ¯\\_(ツ)_/¯"

instance Show GitOpResult where
  show res = concat
    [ show (resultType res)
    , "\nResult text:\n"
    , C8.unpack . C8.intercalate "\n" . take 8 . C8.lines . resultText $ res
    , "\n\n"
    ]

instance Show GitOpError where
  show err = concat
    [ "Update failed with "
    , show (errorCode err)
    , " - "
    , C8.unpack (errorMessage err)
    ]

instance Show RepoUpdateResult where
  show res = concat
    [ "\n================\nRepo "
    , updateResultRepo res
    , maybe " " ((\r -> ":(" ++ r ++ ") ") . C8.unpack) (updateResultBranch res)
    , either show show (updateErrorOrSuccess res)
    , "\n\n"
    ]
