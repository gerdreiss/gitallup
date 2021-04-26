module Actions
    ( checkAndExecuteActions
    ) where


import qualified Logging                       as Log
import qualified RIO.Text                      as T
import qualified RIO.Text.Partial              as TP

import           Control.Monad.Extra            ( ifM )
import           Data.Configurator              ( load
                                                , lookup
                                                )
import           Data.Configurator.Types        ( Config
                                                , Worth(Required)
                                                )
import           RIO                     hiding ( display
                                                , force
                                                , lookup
                                                )
import           RIO.Directory                  ( doesFileExist
                                                , setCurrentDirectory
                                                )
import           RIO.FilePath                   ( takeFileName )
import           RIO.Process                    ( proc
                                                , readProcess
                                                )
import           Types

--
--
checkAndExecuteActions :: [RepoUpdateResult] -> RIO App [RepoUpdateResult]
checkAndExecuteActions results =
    view actionsL >>= maybe (return results) executeOrReturn
  where
    executeOrReturn actions = ifM (liftIO $ doesFileExist actions)
                                  (executeActions results actions)
                                  (warnNotFound results actions)

--
--
executeActions :: [RepoUpdateResult] -> FilePath -> RIO App [RepoUpdateResult]
executeActions results actionFile = do
    config  <- liftIO $ load [Required actionFile]
    actions <- liftIO $ getActions results config
    mapM (uncurry executeActionIfDefined) actions

--
--
getActions :: [RepoUpdateResult] -> Config -> IO [(RepoUpdateResult, Text)]
getActions results config = mapM (getAction config) results

--
--
getAction :: Config -> RepoUpdateResult -> IO (RepoUpdateResult, Text)
getAction config result
    | withResultType Updated result = fmap
        (maybe (result, T.empty) (result, ))
        (lookup config . T.pack . takeFileName . updateResultRepo $ result)
    | otherwise = return (result, T.empty)

--
--
executeActionIfDefined :: RepoUpdateResult -> Text -> RIO App RepoUpdateResult
executeActionIfDefined result action
    | T.null action
    = return result
    | otherwise
    = Log.logAction action result
        >>  prepareCommandLine action result
        >>= executeAction result

--
--
prepareCommandLine :: Text -> RepoUpdateResult -> RIO App [String]
prepareCommandLine action result
    | "[[REPO-PATH]]" `T.isInfixOf` action = return
    . words
    . T.unpack
    $ TP.replace "[[REPO-PATH]]" (T.pack $ updateResultRepo result) action
    | otherwise = do
        setCurrentDirectory (updateResultRepo result)
        return . words . T.unpack $ action

--
--
executeAction :: RepoUpdateResult -> [FilePath] -> RIO App RepoUpdateResult
executeAction result [] = -- this should not happen at this point
    Log.warn ("There is no action defined for " <> updateResultRepo result)
        >> return result
executeAction result [command] =
    processActionResult result <$> proc command [] readProcess
executeAction result (command : args) =
    processActionResult result <$> proc command args readProcess

--
--
processActionResult
    :: RepoUpdateResult -> ReadProcessResult -> RepoUpdateResult
processActionResult result (ExitSuccess     , out, _  ) = result
    { updateErrorOrSuccess = Right $ GitOpResult
        { resultType = ActionExecuted
        , resultText = "Repo updated, and action executed: " <> out
        }
    }
processActionResult result (ExitFailure code, _  , err) = result
    { updateErrorOrSuccess = Left $ GitOpError
                                 { errorCode    = code
                                 , errorMessage =
                                     "Repo updated, but action failed: " <> err
                                 }
    }

--
--
warnNotFound :: [RepoUpdateResult] -> FilePath -> RIO App [RepoUpdateResult]
warnNotFound results actions =
    Log.warn ("File " ++ actions ++ " not found. No actions will be executed.")
        >> return results
