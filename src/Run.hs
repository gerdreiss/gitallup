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
  when force (_currentBranchHardReset repo)
  -- update the current branch         
  res <- Git.updateBranch repo
  -- TODO write result to app
  either Log.logErrE (Log.logResS "Update result:") res
  -- switch to main branch and update it  
  when main (_checkCurrentSwitchUpdate repo)

_currentBranchHardReset :: FilePath -> RIO App ()
_currentBranchHardReset repo =
  whenM (_isDirty repo)
    $  Log.debugMsgS "Trying to extract the current branch..."
    >> do
         res <- Git.currentBranch repo
         either Log.logErrE processCurrentBranch res
 where
  processCurrentBranch maybeBranch = maybe
    (Log.errorMsgS $ concat ["Repo", repo, "has no current branch (WTF?)"])
    (_hardReset repo)
    maybeBranch

_isDirty :: FilePath -> RIO App Bool
_isDirty repo = Log.debugMsgS "Checking branch status..." >> do
  dirty <- Git.isDirty repo
  -- TODO write result to app
  either (return False <$ Log.logErrE) return dirty

_hardReset :: FilePath -> C8.ByteString -> RIO App ()
_hardReset repo branch =
  Log.debugMsgS "Trying to hard reset the current branch..." >> do
    res <- Git.resetHard repo branch
    -- TODO write result to app
    either Log.logErrE (Log.logResS "Hard reset") res

_checkCurrentSwitchUpdate :: FilePath -> RIO App ()
_checkCurrentSwitchUpdate repo =
  Log.debugMsgS "Trying to extract the current branch..." >> do
    res <- Git.currentBranch repo
    -- TODO write result to app
    either Log.logErrE processCurrentBranch res
 where
  processCurrentBranch branch = maybe
    (Log.errorMsgS $ concat ["Repo", repo, "has no current branch (WTF?)"])
    (_checkIsMainSwitchUpdate repo)
    branch

_checkIsMainSwitchUpdate :: FilePath -> B.ByteString -> RIO App ()
_checkIsMainSwitchUpdate repo branch =
  unless (Git.isMainBranch branch) $ _checkMainSwitchUpdate repo

_checkMainSwitchUpdate :: FilePath -> RIO App ()
_checkMainSwitchUpdate repo =
  Log.logMsgS "Trying to switch to main branch..." >> do
    res <- Git.mainBranch repo
    either Log.logErrE processMainBranch res
 where
  processMainBranch branch = maybe
    (Log.errorMsgS $ concat ["Repo", repo, "has no main branch (WTF?)"])
    (_switchUpdateMain repo)
    branch

_switchUpdateMain :: FilePath -> B.ByteString -> RIO App ()
_switchUpdateMain repo branch = do
  res <- Git.switchBranch repo branch
  -- TODO write result to app
  either Log.logErrE logSwitchUpdateBranch res
 where
  logSwitchUpdateBranch _ =
    Log.logResS message GeneralSuccess >> _updateBranch repo
  message = "Switch to branch " <> C8.unpack branch

_updateBranch :: FilePath -> RIO App ()
_updateBranch repo = Log.debugMsgS "Trying to update branch..." >> do
  res <- Git.updateBranch repo
  -- TODO write result to app
  either Log.logErrE logResult res
  where logResult _ = Log.logResS "Update result" GeneralSuccess
