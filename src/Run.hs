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
                                                , concatMapM
                                                , partitionM
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
import           Util                           ( toSet )

run :: RIO App ()
run = do
  root      <- view directoryL
  recursive <- view recursiveL
  depth     <- view recursiveDepthL
  main      <- view mainL
  force     <- view forceL
  exclude   <- view excludeL
  userHome  <- view userHomeL
  Log.logInput recursive depth main force exclude root
  listRepos recursive depth (splitOn "," exclude) root
    >>= updateRepos main force
    >>= printSummary userHome

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
updateRepos main force = concatMapM (updateRepo main force)

updateRepo :: Bool -> Bool -> FilePath -> RIO App [RepoUpdateResult]
updateRepo main force repo = Log.logRepo repo >> do
  forceResult <-
    (if force
      then hardResetCurrentBranch repo
      else RepoUpdateResult repo Nothing <$> Git.updateBranch repo
    )
  mainResult  <-
    (if main
      then checkCurrentBranchSwitchUpdate repo
      else generalSuccessResult repo Nothing "Done."
    )
  return [forceResult, mainResult]

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
  Log.debug "Checking current branch status..."
    >>  Git.isDirty repo
    >>= either (return False <$ Log.logErr) return

hardResetBranch :: FilePath -> B.ByteString -> RIO App RepoUpdateResult
hardResetBranch repo branch =
  RepoUpdateResult repo (Just branch)
    <$> (  Log.logMsg ("Hard reset the branch " ++ C8.unpack branch)
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
  Log.debug "Checking if current branch is the main branch..."
    >>  Git.isMainBranch repo branch
    >>= either
          (errorResult repo)
          (\isMainBranch -> if not isMainBranch
            then retrieveMainBranchSwitchUpdate repo
            else generalSuccessResult repo
                                      (Just branch)
                                      "It is the main branch."
          )

retrieveMainBranchSwitchUpdate :: FilePath -> RIO App RepoUpdateResult
retrieveMainBranchSwitchUpdate repo =
  Log.debug "Retrieving main branch..."
  -- inserting this comment just to force line break
    >>  Git.mainBranch repo
    >>= either
          (errorResult repo)
          (maybe (noMainBranchErrorResult repo) (switchBranchUpdate repo))

switchBranchUpdate :: FilePath -> B.ByteString -> RIO App RepoUpdateResult
switchBranchUpdate repo branch =
  RepoUpdateResult repo (Just branch)
    <$> (  Log.debug ("Switching to " <> show branch <> "...")
        >> Git.switchBranch repo branch
        >> Log.debug "Updating..."
        >> Git.updateBranch repo
        )

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
  $ GitOpError 0 (C8.pack $ "Repo" <> repo <> "has no current branch (WTF?)")

noMainBranchErrorResult :: FilePath -> RIO App RepoUpdateResult
noMainBranchErrorResult repo =
  return $ RepoUpdateResult repo Nothing (noMainBranchError repo)

noMainBranchError :: FilePath -> Either GitOpError GitOpResult
noMainBranchError repo =
  Left $ GitOpError 0 (C8.pack $ "Repo" <> repo <> "has no main branch (WTF?)")

--
--
-- prints the update summary
--
printSummary :: FilePath -> [RepoUpdateResult] -> RIO App ()
printSummary userHome results = do
  let
    newResults =
      fmap (\r -> r { updateResultRepo = dropUserHome . updateResultRepo $ r })
        . filter (not . isGeneralSuccess)
        $ results
  let (errors, successes) =
        partition (isLeft . updateErrorOrSuccess) newResults
  let upToDate = filter isUpToDate successes
  let updated  = filter isUpdated successes
  Log.logMsg "\n\n============================================================"
  Log.logMsg $ "Repos processed  : " ++ show (length $ rmdups newResults)
  Log.logMsg $ "Errors occurred  : " ++ show (length errors)
  Log.logMsg $ "Repos up to date : " ++ show (length . rmdups $ upToDate)
  Log.logMsg $ "Repos updated    : " ++ show (length . rmdups $ updated)
  Log.logMsg "\n"
  mapM_ pPrint errors
  mapM_ pPrint updated
 where
  dropUserHome = ("~" ++) . drop (length userHome)
  isGeneralSuccess res = either (const False)
                                ((== GeneralSuccess) . resultType)
                                (updateErrorOrSuccess res)
  isUpToDate res = either (const False)
                          ((== UpToDate) . resultType)
                          (updateErrorOrSuccess res)
  isUpdated res = either (const False)
                         ((`elem` [Updated, Reset]) . resultType)
                         (updateErrorOrSuccess res)
  rmdups = toSet updateResultRepo
