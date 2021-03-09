{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run
  ( run
  )
where

import qualified Data.ByteString.Lazy.Char8    as C8
import qualified Git
import qualified Logging                       as Log
import qualified RIO.ByteString.Lazy           as B

import           Control.Monad.Extra            ( ifM
                                                , partitionM
                                                )
import           Data.List                      ( partition )
import           Data.List.Split                ( splitOn )
import           RIO                     hiding ( force )
import           System.Directory               ( doesDirectoryExist
                                                , listDirectory
                                                , makeAbsolute
                                                )
import           System.FilePath                ( (</>) )
import           Text.Pretty.Simple             ( pPrint )
import           Types

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
    >>= printSummary

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

updateRepos :: Bool -> Bool -> [FilePath] -> RIO App [RepoUpdateResult]
updateRepos main force = mapM (updateRepo main force)

updateRepo :: Bool -> Bool -> FilePath -> RIO App RepoUpdateResult
updateRepo main force repo =
  Log.logRepo repo
    >> (if force
         then hardResetCurrentBranch repo
         else RepoUpdateResult repo Nothing <$> Git.updateBranch repo
       )
    >> (if main
         then checkCurrentBranchSwitchUpdate repo
         else generalSuccessResult repo Nothing
       )

hardResetCurrentBranch :: FilePath -> RIO App RepoUpdateResult
hardResetCurrentBranch repo = ifM
  (isCurrentBranchDirty repo)
  (Git.currentBranch repo >>= either
    (errorResult repo)
    (maybe (noCurrentBranchErrorResult repo) (hardResetBranch repo))
  )
  (RepoUpdateResult repo Nothing <$> Git.updateBranch repo)

isCurrentBranchDirty :: FilePath -> RIO App Bool
isCurrentBranchDirty repo =
  Log.logMsg "Checking current branch status..."
    >>  Git.isDirty repo
    >>= either (return False <$ Log.logErr) return

hardResetBranch :: FilePath -> B.ByteString -> RIO App RepoUpdateResult
hardResetBranch repo branch =
  RepoUpdateResult repo (Just branch)
    <$> (  Log.logMsg "Hard reset the current branch..."
        >> Git.resetHard repo branch
        )

checkCurrentBranchSwitchUpdate :: FilePath -> RIO App RepoUpdateResult
checkCurrentBranchSwitchUpdate repo =
  Git.currentBranch repo >>= processCurrentBranch
 where
  processCurrentBranch = either
    (errorResult repo)
    (maybe (noCurrentBranchErrorResult repo)
           (checkBranchNotMainSwitchUpdate repo)
    )

checkBranchNotMainSwitchUpdate
  :: FilePath -> B.ByteString -> RIO App RepoUpdateResult
checkBranchNotMainSwitchUpdate repo branch =
  Log.logMsg "Checking if current branch is not the main branch..."
    >> if not (Git.isMainBranch branch)
         then retrieveMainBranchSwitchUpdate repo
         else generalSuccessResult repo (Just branch)

retrieveMainBranchSwitchUpdate :: FilePath -> RIO App RepoUpdateResult
retrieveMainBranchSwitchUpdate repo =
  Log.logMsg "Retrieving main branch..."
  -- inserting this comment just to force line break
    >>  Git.mainBranch repo
    >>= either
          (errorResult repo)
          (maybe (noMainBranchErrorResult repo) (switchBranchUpdate repo))

switchBranchUpdate :: FilePath -> B.ByteString -> RIO App RepoUpdateResult
switchBranchUpdate repo branch =
  RepoUpdateResult repo (Just branch)
    <$> (  Log.logMsg ("Switching to " <> show branch <> "...")
        >> Git.switchBranch repo branch
        >> Log.logMsg "Updating..."
        >> Git.updateBranch repo
        )

errorResult :: FilePath -> GitOpError -> RIO App RepoUpdateResult
errorResult repo = return . RepoUpdateResult repo Nothing . Left

generalSuccessResult
  :: FilePath -> Maybe B.ByteString -> RIO App RepoUpdateResult
generalSuccessResult repo maybeBranch =
  return $ RepoUpdateResult repo maybeBranch (Right GeneralSuccess)

noCurrentBranchErrorResult :: FilePath -> RIO App RepoUpdateResult
noCurrentBranchErrorResult repo =
  return $ RepoUpdateResult repo Nothing (noCurrentBranchError repo)

noCurrentBranchError :: FilePath -> Either GitOpError GitOpSuccess
noCurrentBranchError repo = Left
  $ GitOpError 0 (C8.pack $ "Repo" <> repo <> "has no current branch (WTF?)")

noMainBranchErrorResult :: FilePath -> RIO App RepoUpdateResult
noMainBranchErrorResult repo =
  return $ RepoUpdateResult repo Nothing (noMainBranchError repo)

noMainBranchError :: FilePath -> Either GitOpError GitOpSuccess
noMainBranchError repo =
  Left $ GitOpError 0 (C8.pack $ "Repo" <> repo <> "has no main branch (WTF?)")

--
--
-- prints the update summary
--
printSummary :: [RepoUpdateResult] -> RIO App ()
printSummary results = do
  let (errors, successes) = partition (isLeft . updateErrorOrSuccess) results
  let updated             = filter isUpdated successes
  Log.logMsg "=============================================================="
  Log.logMsg $ "Errors occurred: " ++ show (length errors)
  Log.logMsg $ "Successfully updated: " ++ show (length updated)
  mapM_ pPrint errors
  mapM_ pPrint updated
  where isUpdated res = Right Updated == updateErrorOrSuccess res
