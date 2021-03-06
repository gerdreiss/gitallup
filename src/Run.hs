{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run
  ( run
  )
where

import qualified RIO.ByteString.Lazy           as B
import qualified Data.ByteString.Lazy.Char8    as C8
import qualified Git
import qualified Logging                       as Log

import           Control.Monad.Extra            ( ifM
                                                , partitionM
                                                )
import           Data.List.Split                ( splitOn )
import           RIO                     hiding ( force )
import           System.Directory               ( doesDirectoryExist
                                                , listDirectory
                                                , makeAbsolute
                                                )
import           System.FilePath                ( (</>) )
import           Types
import           Data.List                      ( partition
                                                , intercalate
                                                )

run :: RIO App ()
run = do
  root      <- view directoryL
  recursive <- view recursiveL
  depth     <- view recursiveDepthL
  main      <- view mainL
  force     <- view forceL
  exclude   <- view excludeL
  Log.logInput recursive depth main force exclude root
  listRepos recursive depth (splitOn "," exclude) root
    >>= updateRepos main force
    >>  printSummary

listRepos :: Bool -> Int -> [FilePath] -> FilePath -> RIO App [FilePath]
listRepos recursive depth excluded root = do
  (repos, rest) <- listReposAndRest excluded root
  nested        <- listNestedRepos recursive depth excluded rest
  return (repos <> nested)

listReposAndRest :: [FilePath] -> FilePath -> RIO App ([FilePath], [FilePath])
listReposAndRest excluded root =
  liftIO
    $   makeAbsolute root
    >>= listDirectories excluded
    >>= partitionM Git.isGitRepo

listDirectories :: [FilePath] -> FilePath -> IO [FilePath]
listDirectories excluded path = ifM
  (doesDirectoryExist path)
  (fmap (path </>) . filter (`notElem` excluded) <$> listDirectory path)
  (return [])

listNestedRepos :: Bool -> Int -> [FilePath] -> [FilePath] -> RIO App [FilePath]
listNestedRepos recursive depth excluded subdirs
  | recursive && depth /= 0 && (not . null $ subdirs)
  = concat <$> mapM (listRepos True (depth - 1) excluded) subdirs
  | otherwise
  = return []

updateRepos :: Bool -> Bool -> [FilePath] -> RIO App ()
updateRepos main force = mapM_ (updateRepo main force)

updateRepo :: Bool -> Bool -> FilePath -> RIO App ()
updateRepo main force repo = Log.logRepo repo >> do
  -- force updating the current branch discarding any changes
  when force (hardResetCurrentBranch repo)
  -- update the current branch         
  res <- Git.updateBranch repo
  -- TODO write result to app
  either Log.logErrE (Log.logResS "Update result:") res
  -- switch to main branch and update it  
  when main (checkCurrentBranchSwitchUpdate repo)

hardResetCurrentBranch :: FilePath -> RIO App ()
hardResetCurrentBranch repo =
  whenM (isCurrentBranchDirty repo)
    $   Log.debugMsgS "Trying to extract the current branch..."
    >>  Git.currentBranch repo
    >>= either Log.logErrE processCurrentBranch
 where
  processCurrentBranch maybeBranch = maybe
    -- TODO instead of logging, add error to app
    (Log.errorMsgS $ concat ["Repo", repo, "has no current branch (WTF?)"])
    (hardResultBranch repo)
    maybeBranch

isCurrentBranchDirty :: FilePath -> RIO App Bool
isCurrentBranchDirty repo =
  Log.debugMsgS "Checking branch status..."
    >>  Git.isDirty repo
    -- TODO write result to app
    >>= either (return False <$ Log.logErrE) return

hardResultBranch :: FilePath -> B.ByteString -> RIO App ()
hardResultBranch repo branch =
  Log.debugMsgS "Trying to hard reset the current branch..."
    >>  Git.resetHard repo branch
    -- TODO write result to app
    >>= either Log.logErrE (Log.logResS "Hard reset")

checkCurrentBranchSwitchUpdate :: FilePath -> RIO App ()
checkCurrentBranchSwitchUpdate repo =
  Log.debugMsgS "Trying to extract the current branch..."
    >>  Git.currentBranch repo
    >>= either Log.logErrE processCurrentBranch
 where
  processCurrentBranch = maybe
    (Log.errorMsgS $ concat ["Repo", repo, "has no current branch (WTF?)"])
    (checkBranchIsMainSwitchUpdate repo)

checkBranchIsMainSwitchUpdate :: FilePath -> B.ByteString -> RIO App ()
checkBranchIsMainSwitchUpdate repo branch =
  unless (Git.isMainBranch branch) (getMainBranchSwitchUpdate repo)

getMainBranchSwitchUpdate :: FilePath -> RIO App ()
getMainBranchSwitchUpdate repo =
  Log.logMsgS "Trying to switch to main branch..."
    >>  Git.mainBranch repo
    >>= either Log.logErrE processMainBranch
 where
  processMainBranch branch = maybe
    (Log.errorMsgS $ concat ["Repo ", repo, " has no main branch (WTF?)"])
    (switchBranchUpdate repo)
    branch

switchBranchUpdate :: FilePath -> B.ByteString -> RIO App ()
switchBranchUpdate repo branch =
  Git.switchBranch repo branch >>= either Log.logErrE logSwitchUpdateBranch
 where
  logSwitchUpdateBranch _ =
    Log.logResS message GeneralSuccess >> updateCurrentBranch repo
  message = "Switch to branch " <> C8.unpack branch

updateCurrentBranch :: FilePath -> RIO App ()
updateCurrentBranch repo =
  Log.debugMsgS ("Trying to update repo" <> repo <> "...")
    >>  Git.updateBranch repo
    >>= either Log.logErrE (Log.logMsgS . show)

--
-- TODO fix this
-- write result to app result list
--

addNewResult :: RepoUpdateResult -> RIO App () -> RIO App ()
addNewResult newResult inner = do
  app        <- ask
  oldResults <- view resultsL
  Log.logMsgS
    ("DEBUGGING OLD RESULTS: " <> (intercalate "\n" . fmap show $ oldResults))
  runRIO app $ do
    local (set resultsL (newResult : oldResults)) inner

--
--
-- prints the update summary
--
printSummary :: RIO App ()
printSummary = do
  results <- view resultsL
  let (errors, successes) = partition (isLeft . updateErrorOrSuccess) results
  mapM_ printResult errors
  mapM_ printResult successes

printResult :: RepoUpdateResult -> RIO App ()
printResult res = Log.logMsgS (updateResultRepo res)
