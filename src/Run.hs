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
import           RIO.Directory                  ( doesDirectoryExist
                                                , listDirectory
                                                , makeAbsolute
                                                )
import           RIO.FilePath                   ( (</>) )

import           Actions
import           Summary
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
    >>= checkAndExecuteActions
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
checkStatusOrUpdateRepos :: [FilePath] -> RIO App [RepoUpdateResult]
checkStatusOrUpdateRepos repos =
  ifM (view statusL) (checkStatusRepos repos) (updateRepos repos)

--
-- Check status
--
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
  forceResult <- ifM (view forceL)
                     (checkIsDirtyAndHardResetCurrentBranch repo)
                     (RepoUpdateResult repo Nothing <$> Git.updateBranch repo)
  mainResult  <- ifM (view mainL)
                     (checkCurrentBranchSwitchUpdate repo)
                     (generalSuccessResult repo Nothing "Done.")
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
-- functions to create different update or status results
--

errorResult :: FilePath -> GitOpError -> RIO App RepoUpdateResult
errorResult repo = return . RepoUpdateResult repo Nothing . Left

generalSuccessResult
  :: FilePath -> Maybe B.ByteString -> B.ByteString -> RIO App RepoUpdateResult
generalSuccessResult repo branch text = return $ RepoUpdateResult
  { updateResultRepo     = repo
  , updateResultBranch   = branch
  , updateErrorOrSuccess = Right GitOpResult { resultType = GeneralSuccess
                                             , resultText = text
                                             }
  }

noCurrentBranchErrorResult :: FilePath -> RIO App RepoUpdateResult
noCurrentBranchErrorResult repo = return $ RepoUpdateResult
  { updateResultRepo     = repo
  , updateResultBranch   = Nothing
  , updateErrorOrSuccess = Left GitOpError { errorCode    = -1
                                           , errorMessage = message
                                           }
  }
  where message = C8.pack $ "Repo" ++ repo ++ "has no current branch (WTF?)"

noMainBranchErrorResult :: FilePath -> RIO App RepoUpdateResult
noMainBranchErrorResult repo = return $ RepoUpdateResult
  { updateResultRepo     = repo
  , updateResultBranch   = Nothing
  , updateErrorOrSuccess = Left GitOpError { errorCode    = -1
                                           , errorMessage = message
                                           }
  }
  where message = C8.pack ("Repo" ++ repo ++ "has no main branch (WTF?)")

