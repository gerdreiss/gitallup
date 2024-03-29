module Git
  ( listBranches
  , currentBranch
  , mainBranch
  , switchBranch
  , cleanRepo
  , resetHard
  , updateBranch
  , deleteBranch
  , branchStatus
  , isDirty
  , isGitRepo
  , isMainBranch
  , isRemoteBranch
  ) where

import qualified Data.ByteString.Lazy.Char8    as C8     -- TODO replace this with RIO's package or function
import qualified RIO.ByteString.Lazy           as B

import           Data.List.Extra                ( trim )

import           RIO
import           RIO.Directory                  ( doesDirectoryExist )
import           RIO.FilePath                   ( (</>) )
import           RIO.List                       ( find )
import           RIO.Process                    ( proc
                                                , readProcess
                                                )
import           Types

--
--
-- Main module functions
--
-- queries
--
listBranches :: FilePath -> RIO App (Either GitOpError [B.ByteString])
listBranches repo = _extractBranchList <$> proc "git" ["-C", repo, "branch"] readProcess

--
--
currentBranch :: FilePath -> RIO App (Either GitOpError (Maybe B.ByteString))
currentBranch repo = _extractCurrentBranch <$> proc "git" ["-C", repo, "branch"] readProcess

--
--
mainBranch :: FilePath -> RIO App (Either GitOpError (Maybe B.ByteString))
mainBranch repo = _extractMainBranch <$> proc "git" ["-C", repo, "branch", "--all"] readProcess

--
--
isMainBranch :: FilePath -> B.ByteString -> RIO App (Either GitOpError Bool)
isMainBranch repo branch = fmap (Just branch ==) <$> mainBranch repo

--
--
isRemoteBranch :: FilePath -> B.ByteString -> RIO App (Either GitOpError Bool)
isRemoteBranch repo branch =
  _extractBranchIsRemote <$> proc "git" ["-C", repo, "ls-remote", "--heads", "origin", "refs/heads/" ++ C8.unpack branch] readProcess

--
--
isDirty :: FilePath -> RIO App (Either GitOpError Bool)
isDirty repo = _extractBranchIsDirty <$> proc "git" ["-C", repo, "status"] readProcess

--
--
branchStatus :: FilePath -> RIO App (Either GitOpError GitOpResult)
branchStatus repo = _extractGitOpErrorOrResult <$> proc "git" ["-C", repo, "status"] readProcess

--
-- actions
--

switchBranch :: FilePath -> B.ByteString -> RIO App (Either GitOpError GitOpResult)
switchBranch repo branch = _extractGitOpErrorOrResult <$> proc "git" ["-C", repo, "switch", C8.unpack branch] readProcess

--
--
updateBranch :: FilePath -> RIO App (Either GitOpError GitOpResult)
updateBranch repo =
  proc "git" ["-C", repo, "fetch", "--all"] readProcess >> _extractGitOpErrorOrResult <$> proc "git" ["-C", repo, "pull"] readProcess

--
--
deleteBranch :: FilePath -> B.ByteString -> RIO App (Either GitOpError GitOpResult)
deleteBranch repo branch = _extractGitOpErrorOrResult <$> proc "git" ["-C", repo, "branch", "-D", C8.unpack branch] readProcess

--
--
resetHard :: FilePath -> B.ByteString -> RIO App (Either GitOpError GitOpResult)
resetHard repo branch =
  _extractGitOpErrorOrResult <$> proc "git" ["-C", repo, "reset", "--hard", "origin/" ++ C8.unpack branch] readProcess

--
--
cleanRepo :: FilePath -> RIO App (Either GitOpError GitOpResult)
cleanRepo repo = _extractGitOpErrorOrResult <$> proc "git" ["-C", repo, "clean", "-fdx"] readProcess

--
--
-- Utilities
--
--
isGitRepo :: FilePath -> IO Bool
isGitRepo dir = doesDirectoryExist (dir </> ".git")

--
--
-- Private functions
--
--
_extractBranchList :: ReadProcessResult -> Either GitOpError [B.ByteString]
_extractBranchList (ExitSuccess     , out, _  ) = Right (B.drop 2 <$> C8.lines out)
_extractBranchList (ExitFailure code, _  , err) = Left (GitOpError code err)

--
--
_extractCurrentBranch :: ReadProcessResult -> Either GitOpError (Maybe B.ByteString)
_extractCurrentBranch (ExitSuccess     , out, _  ) = Right . fmap (B.drop 2) . find (B.isPrefixOf "* ") . C8.lines $ out
_extractCurrentBranch (ExitFailure code, _  , err) = Left (GitOpError code err)

--
--
_extractMainBranch :: ReadProcessResult -> Either GitOpError (Maybe B.ByteString)
_extractMainBranch (ExitSuccess, out, _) =
  Right
    . fmap (B.drop 30) -- length of "remotes/origin/HEAD -> origin/"
    . find (B.isPrefixOf "remotes/origin/HEAD")
    . fmap (B.drop 2)
    . C8.lines
    $ out
_extractMainBranch (ExitFailure code, _, err) = Left $ GitOpError code err

--
--
_extractBranchIsDirty :: ReadProcessResult -> Either GitOpError Bool
_extractBranchIsDirty (ExitSuccess     , out, _  ) = Right . isNothing . find (B.isPrefixOf "nothing to commit") . C8.lines $ out
_extractBranchIsDirty (ExitFailure code, _  , err) = Left (GitOpError code err)

--
--
_extractBranchIsRemote :: ReadProcessResult -> Either GitOpError Bool
_extractBranchIsRemote (ExitSuccess     , out, _  ) = Right . not . null . trim $ C8.unpack out
_extractBranchIsRemote (ExitFailure code, _  , err) = Left (GitOpError code err)

--
--
_extractGitOpErrorOrResult :: ReadProcessResult -> Either GitOpError GitOpResult
_extractGitOpErrorOrResult (ExitSuccess     , out, _  ) = Right $ GitOpResult (_extractGitOpResultType out) out
_extractGitOpErrorOrResult (ExitFailure code, _  , err) = Left (GitOpError code err)

--
--
_extractGitOpResultType :: B.ByteString -> GitOpResultType
_extractGitOpResultType result | isUpToDate result    = UpToDate
                               | isUpdated result     = Updated
                               | isReset result       = Reset
                               | hasNoChanges result  = Clean
                               | hasChanges result    = Dirty
                               | cleanedUp result     = CleanedUp
                               | deletedBranch result = Deleted
                               | otherwise            = GeneralSuccess
 where
  isUpToDate res = any (B.isPrefixOf "Already up to date") (C8.lines res)
  isUpdated res = any (B.isPrefixOf "Updating") (C8.lines res)
  isReset res = any (B.isPrefixOf "HEAD is now at") (C8.lines res)
  cleanedUp res = any (B.isPrefixOf "Removing ") (C8.lines res)
  deletedBranch res = any (B.isPrefixOf "Deleted branch ") (C8.lines res)
  hasNoChanges res = any (B.isPrefixOf "nothing to commit") (C8.lines res)
  hasChanges res = any isChange (C8.lines res)
  isChange line = any
    ($ line)
    [ B.isPrefixOf "Your branch is ahead of"
    , B.isPrefixOf "Changes not staged for commit"
    , B.isPrefixOf "Changes to be committed"
    , B.isPrefixOf "Untracked files"
    ]
