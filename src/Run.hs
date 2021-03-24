module Run
  ( run
  ) where

import qualified Data.ByteString.Lazy.Char8    as C8
import qualified Git
import qualified Logging                       as Log
import qualified RIO.ByteString.Lazy           as B


import           Control.Monad.Extra            ( ifM
                                                , partitionM
                                                )
import           Control.Parallel.Strategies    ( parList
                                                , rpar
                                                , using
                                                )
import           Data.List.Split                ( splitOn )
import           RIO                     hiding ( force )
import           RIO.List                       ( partition )
import           System.Directory               ( doesDirectoryExist
                                                , listDirectory
                                                , makeAbsolute
                                                )
import           System.FilePath                ( (</>) )
import           Text.Pretty.Simple             ( pPrint )
import           Types

--
--
run :: RIO App ()
run = do
  Log.logInput
  recursive <- view recursiveL
  depth     <- view recursiveDepthL
  exclude   <- view excludeL
  root      <- view directoryL
  liftIO (listRepos recursive depth (splitOn "," exclude) root)
    >>= checkStatusOrUpdateRepos
    >>= printSummary

--
-- Listing GIT repos recursively up to a given depth
--
listRepos :: Bool -> Int -> [FilePath] -> FilePath -> IO [FilePath]
listRepos recursive depth excluded root = do
  (repos, rest) <- listReposAndRest excluded root
  nested        <- listNestedRepos recursive depth excluded rest
  return (repos ++ nested)

listReposAndRest :: [FilePath] -> FilePath -> IO ([FilePath], [FilePath])
listReposAndRest excluded root =
  makeAbsolute root >>= listDirectories excluded >>= partitionM Git.isGitRepo

listDirectories :: [FilePath] -> FilePath -> IO [FilePath]
listDirectories excluded path = ifM
  (doesDirectoryExist path)
  (fmap (path </>) . filter (`notElem` excluded) <$> listDirectory path)
  (return [])

listNestedRepos :: Bool -> Int -> [FilePath] -> [FilePath] -> IO [FilePath]
listNestedRepos recursive depth excluded subdirs
  | recursive && depth /= 0 && (not . null $ subdirs) = concat <$> sequence
    (map (listRepos True (depth - 1) excluded) subdirs `using` parList rpar)
  | otherwise = return []

--
-- Check git status of or update the repositories
--
-- Check status
--
checkStatusOrUpdateRepos :: [FilePath] -> RIO App [RepoUpdateResult]
checkStatusOrUpdateRepos repos = do
  status <- view statusL
  if status then checkStatusRepos repos else updateRepos repos

checkStatusRepos :: [FilePath] -> RIO App [RepoUpdateResult]
checkStatusRepos repos =
  sequence (map checkStatusRepo repos `using` parList rpar)

checkStatusRepo :: FilePath -> RIO App RepoUpdateResult
checkStatusRepo repo =
  Log.logRepo repo >> RepoUpdateResult repo Nothing <$> Git.branchStatus repo

--
-- Update
--
updateRepos :: [FilePath] -> RIO App [RepoUpdateResult]
updateRepos repos =
  concat <$> sequence (map updateRepo repos `using` parList rpar)

updateRepo :: FilePath -> RIO App [RepoUpdateResult]
updateRepo repo = Log.logRepo repo >> do
  main        <- view mainL
  force       <- view forceL
  forceResult <- if force
    then checkIsDirtyAndHardResetCurrentBranch repo
    else RepoUpdateResult repo Nothing <$> Git.updateBranch repo
  mainResult  <- if main
    then checkCurrentBranchSwitchUpdate repo
    else generalSuccessResult repo Nothing "Done."
  return [forceResult, mainResult]

checkIsDirtyAndHardResetCurrentBranch :: FilePath -> RIO App RepoUpdateResult
checkIsDirtyAndHardResetCurrentBranch repo = ifM
  (isCurrentBranchDirty repo)
  (hardResetCurrentBranch repo)
  (RepoUpdateResult repo Nothing <$> Git.updateBranch repo)

isCurrentBranchDirty :: FilePath -> RIO App Bool
isCurrentBranchDirty repo =
  Log.debug "Checking current branch status..."
    >>  Git.isDirty repo
    >>= either (return False <$ Log.logErr) return

hardResetCurrentBranch :: FilePath -> RIO App RepoUpdateResult
hardResetCurrentBranch repo =
  Log.debug "Retrieving the current branch..."
    >>  Git.currentBranch repo
    >>= either
          (errorResult repo)
          (maybe (noCurrentBranchErrorResult repo) (hardResetBranch repo))

hardResetBranch :: FilePath -> B.ByteString -> RIO App RepoUpdateResult
hardResetBranch repo branch =
  RepoUpdateResult repo (Just branch)
    <$> (  Log.debug ("Hard reset the branch " ++ C8.unpack branch)
        >> Git.resetHard repo branch
        )

checkCurrentBranchSwitchUpdate :: FilePath -> RIO App RepoUpdateResult
checkCurrentBranchSwitchUpdate repo =
  Log.debug "Retrieving the current branch..."
    >>  Git.currentBranch repo
    >>= either
          (errorResult repo)
          (maybe (noCurrentBranchErrorResult repo)
                 (checkBranchNotMainSwitchUpdate repo)
          )

checkBranchNotMainSwitchUpdate
  :: FilePath -> B.ByteString -> RIO App RepoUpdateResult
checkBranchNotMainSwitchUpdate repo branch =
  Log.debug "Checking if current branch is the main branch..."
    >>  Git.isMainBranch repo branch
    >>= either
          (errorResult repo)
          (\isMainBranch -> if not isMainBranch
            then retrieveMainBranchSwitchUpdate repo
            else generalSuccessResult repo (Just branch) "It's the main branch."
          )

retrieveMainBranchSwitchUpdate :: FilePath -> RIO App RepoUpdateResult
retrieveMainBranchSwitchUpdate repo =
  Log.debug "Retrieving the main/master branch..."
    >>  Git.mainBranch repo
    >>= either
          (errorResult repo)
          (maybe (noMainBranchErrorResult repo) (switchBranchUpdate repo))

switchBranchUpdate :: FilePath -> B.ByteString -> RIO App RepoUpdateResult
switchBranchUpdate repo branch =
  RepoUpdateResult repo (Just branch)
    <$> (  Log.debug ("Switching to " ++ show branch ++ "...")
        >> Git.switchBranch repo branch
        >> Log.debug "Updating..."
        >> Git.updateBranch repo
        )

--
-- functions to create different update results
--
errorResult :: FilePath -> GitOpError -> RIO App RepoUpdateResult
errorResult repo = return . RepoUpdateResult repo Nothing . Left

generalSuccessResult
  :: FilePath -> Maybe B.ByteString -> B.ByteString -> RIO App RepoUpdateResult
generalSuccessResult repo maybeBranch text = return $ RepoUpdateResult
  repo
  maybeBranch
  (Right $ GitOpResult GeneralSuccess text)

noCurrentBranchErrorResult :: FilePath -> RIO App RepoUpdateResult
noCurrentBranchErrorResult repo =
  return $ RepoUpdateResult repo Nothing (noCurrentBranchError repo)

noCurrentBranchError :: FilePath -> Either GitOpError GitOpResult
noCurrentBranchError repo = Left
  $ GitOpError 0 (C8.pack $ "Repo" ++ repo ++ "has no current branch (WTF?)")

noMainBranchErrorResult :: FilePath -> RIO App RepoUpdateResult
noMainBranchErrorResult repo =
  return $ RepoUpdateResult repo Nothing (noMainBranchError repo)

noMainBranchError :: FilePath -> Either GitOpError GitOpResult
noMainBranchError repo =
  Left $ GitOpError 0 (C8.pack $ "Repo" ++ repo ++ "has no main branch (WTF?)")

--
--
-- prints the status or update summary
--
printSummary :: [RepoUpdateResult] -> RIO App ()
printSummary results = do

  status <- view statusL

  Log.logMsg "\n\n============================================================"

  let newResults = filter (not . isGeneralSuccess) results
      errors     = filter (isLeft . updateErrorOrSuccess) newResults

  Log.logMsg $ "Repos processed  : " ++ show (length newResults)
  Log.logMsg $ "Errors occurred  : " ++ show (length errors)

  if status
    then printStatusSummary newResults
    else printUpdateSummary newResults

 where
  isGeneralSuccess res = either (const False)
                                ((== GeneralSuccess) . resultType)
                                (updateErrorOrSuccess res)

--
--
printStatusSummary :: [RepoUpdateResult] -> RIO App ()
printStatusSummary results = do
  let (errors, successes) = partition (isLeft . updateErrorOrSuccess) results
      dirty               = filter isDirty successes

  Log.logMsg $ "Repos dirty      : " ++ show (length dirty)
  Log.logMsg "\n"

  mapM_ pPrint errors
  mapM_ pPrint dirty

 where
  isDirty res =
    either (const False) ((== Dirty) . resultType) (updateErrorOrSuccess res)

--
--
printUpdateSummary :: [RepoUpdateResult] -> RIO App ()
printUpdateSummary results = do
  let (errors, successes) = partition (isLeft . updateErrorOrSuccess) results
      upToDate            = filter isUpToDate successes
      updated             = filter isUpdated successes

  Log.logMsg $ "Repos up to date : " ++ show (length upToDate)
  Log.logMsg $ "Repos updated    : " ++ show (length updated)
  Log.logMsg "\n"

  mapM_ pPrint errors
  mapM_ pPrint updated

 where
  isUpToDate res =
    either (const False) ((== UpToDate) . resultType) (updateErrorOrSuccess res)
  isUpdated res = either (const False)
                         ((`elem` [Updated, Reset]) . resultType)
                         (updateErrorOrSuccess res)
