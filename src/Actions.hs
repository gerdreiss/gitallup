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
import           RIO                     hiding ( display
                                                , force
                                                , lookup
                                                )
import           RIO.Directory                  ( doesFileExist )

import           Data.Configurator.Types        ( Config
                                                , Worth(Required)
                                                )
import           Data.List                      ( last )
import           RIO.FilePath                   ( takeFileName )
import           RIO.List                       ( intercalate )
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
executeActionIfDefined result action | T.null action = return result
                                     | otherwise = executeAction result action

--
--
executeAction :: RepoUpdateResult -> Text -> RIO App RepoUpdateResult
executeAction result action =
    Log.logMsg ("Executing command:\n" <> commandLine)
        >>  last
        <$> mapM doExecute commands
  where
    commandLine = intercalate "\n" (unwords <$> commands)
    commands    = fmap T.unpack . T.words <$> prepareCommandLine result action

    doExecute [] = -- this should not happen at this point
        Log.warn ("There is no action defined for " <> updateResultRepo result)
            >> return result
    doExecute [command] =
        processActionResult result <$> proc command [] readProcess
    doExecute (command : args) =
        processActionResult result <$> proc command args readProcess

--
--
prepareCommandLine :: RepoUpdateResult -> Text -> [Text]
prepareCommandLine result action
    | "[[REPO-PATH]]" `T.isInfixOf` action
    = [TP.replace "[[REPO-PATH]]" (T.pack $ updateResultRepo result) action]
    | otherwise
    = ["cd " <> T.pack (updateResultRepo result), action]

--
--
processActionResult
    :: RepoUpdateResult -> ReadProcessResult -> RepoUpdateResult
processActionResult result (ExitSuccess     , out, _  ) = result
    { updateErrorOrSuccess = Right $ GitOpResult
        { resultType = ActionExd
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
