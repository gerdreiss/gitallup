module Actions
    ( checkAndExecuteActions
    ) where


import qualified Logging                       as Log
import qualified RIO.Text                      as T
import qualified RIO.Text.Partial              as TP

import           Control.Monad.Extra            ( ifM )
import           Control.Parallel.Strategies    ( parList
                                                , rpar
                                                , using
                                                )
import           Data.Configurator              ( load
                                                , lookup
                                                )
import           RIO                     hiding ( force
                                                , lookup
                                                )
import           RIO.Directory                  ( doesFileExist )

import           Data.Configurator.Types        ( Config
                                                , Worth(Required)
                                                )
import           RIO.FilePath                   ( takeFileName )
import           RIO.List.Partial               ( head
                                                , tail
                                                )
import           RIO.Process                    ( proc
                                                , readProcess
                                                )
import           Types


checkAndExecuteActions :: [RepoUpdateResult] -> RIO App [RepoUpdateResult]
checkAndExecuteActions results =
    view actionsL >>= maybe (return results) executeOrReturn
  where
    executeOrReturn actions = ifM (liftIO $ doesFileExist actions)
                                  (executeActions results actions)
                                  (warnNotFound results actions)

executeActions :: [RepoUpdateResult] -> FilePath -> RIO App [RepoUpdateResult]
executeActions results actions = do
    config <- liftIO $ load [Required actions]
    acts   <- liftIO $ getActions config results
    sequence (map (uncurry executeAction) acts `using` parList rpar)

executeAction :: FilePath -> Text -> RIO App RepoUpdateResult
executeAction repo action = do
    let command =
            fmap T.unpack . T.words $ if "[[REPO-PATH]]" `T.isInfixOf` action
                then TP.replace "[[REPO-PATH]]" (T.pack repo) action
                else action
    processActionResult repo <$> proc (head command) (tail command) readProcess

getActions :: Config -> [RepoUpdateResult] -> IO [(FilePath, Text)]
getActions config = mapM (getAction config . updateResultRepo)

getAction :: Config -> FilePath -> IO (FilePath, Text)
getAction config repo = fmap (maybe (repo, T.empty) (repo, ))
                             (lookup config . T.pack . takeFileName $ repo)

processActionResult :: FilePath -> ReadProcessResult -> RepoUpdateResult
processActionResult repo (ExitSuccess     , out, _  ) = undefined
processActionResult repo (ExitFailure code, _  , err) = undefined

warnNotFound :: [RepoUpdateResult] -> FilePath -> RIO App [RepoUpdateResult]
warnNotFound results actions =
    Log.warn ("File " ++ actions ++ " not found. No actions will be executed.")
        >> return results
