{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Git
  ( listBranches
  , currentBranch
  , mainBranch
  , switchBranch
  , resetHard
  , updateBranch
  , isDirty
  , isGitRepo
  , isMainBranch
  )
where

import qualified RIO.ByteString.Lazy           as B
import qualified Data.ByteString.Lazy.Char8    as C8 -- TODO replace this with RIO's package or function

import           RIO
import           RIO.Directory                  ( doesDirectoryExist )
import           RIO.FilePath                   ( (</>) )
import           RIO.List                       ( find
                                                , isPrefixOf
                                                )
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
listBranches repo =
  _extractBranchList <$> proc "git" ["-C", repo, "branch"] readProcess

--
--
currentBranch :: FilePath -> RIO App (Either GitOpError (Maybe B.ByteString))
currentBranch repo =
  _extractCurrentBranch <$> proc "git" ["-C", repo, "branch"] readProcess

--
--
mainBranch :: FilePath -> RIO App (Either GitOpError (Maybe B.ByteString))
mainBranch repo =
  _extractMainBranch <$> proc "git" ["-C", repo, "branch"] readProcess

--
--
isDirty :: FilePath -> RIO App (Either GitOpError Bool)
isDirty repo =
  _extractBranchIsDirty <$> proc "git" ["-C", repo, "status"] readProcess

--
-- actions
--
switchBranch
  :: FilePath -> B.ByteString -> RIO App (Either GitOpError GitOpResult)
switchBranch repo branch =
  _extractGitOpErrorOrResult
    <$> proc "git" ["-C", repo, "switch", C8.unpack branch] readProcess

--
--
updateBranch :: FilePath -> RIO App (Either GitOpError GitOpResult)
updateBranch repo =
  proc "git" ["-C", repo, "fetch", "--all"] readProcess
    >>  _extractGitOpErrorOrResult
    <$> proc "git" ["-C", repo, "pull"] readProcess

--
--
resetHard :: FilePath -> B.ByteString -> RIO App (Either GitOpError GitOpResult)
resetHard repo branch =
  _extractGitOpErrorOrResult
    <$> proc "git"
             ["-C", repo, "reset", "--hard", "origin/" ++ C8.unpack branch]
             readProcess


--
--
-- Utilities
--
--
isGitRepo :: FilePath -> IO Bool
isGitRepo dir = doesDirectoryExist (dir </> ".git")

isMainBranch :: B.ByteString -> Bool
isMainBranch branch = branch `elem` _mainBranches

-- 
-- 
-- Private functions
--
--
_extractBranchList :: ReadProcessResult -> Either GitOpError [B.ByteString]
_extractBranchList (ExitSuccess, out, _) =
  Right . fmap (B.drop 2) . C8.lines $ out
_extractBranchList (ExitFailure code, _, err) = Left $ GitOpError code err

-- 
--
_extractCurrentBranch
  :: ReadProcessResult -> Either GitOpError (Maybe B.ByteString)
_extractCurrentBranch (ExitSuccess, out, _) =
  Right . fmap (B.drop 2) . find (B.isPrefixOf "* ") . C8.lines $ out
_extractCurrentBranch (ExitFailure code, _, err) = Left $ GitOpError code err

--
--
_extractMainBranch
  :: ReadProcessResult -> Either GitOpError (Maybe B.ByteString)
_extractMainBranch (ExitSuccess, out, _) =
  Right . find (`elem` _mainBranches) . fmap (B.drop 2) . C8.lines $ out
_extractMainBranch (ExitFailure code, _, err) = Left $ GitOpError code err

--
--
_extractBranchIsDirty :: ReadProcessResult -> Either GitOpError Bool
_extractBranchIsDirty (ExitSuccess, out, _) =
  Right . not $ B.isSuffixOf "working tree clean" out
_extractBranchIsDirty (ExitFailure code, _, err) = Left $ GitOpError code err

--
--
_extractGitOpErrorOrResult :: ReadProcessResult -> Either GitOpError GitOpResult
_extractGitOpErrorOrResult (ExitSuccess, out, _) =
  Right $ GitOpResult (_extractGitOpResultType out) out
_extractGitOpErrorOrResult (ExitFailure code, _, err) =
  Left $ GitOpError code err

--
--
_extractGitOpResultType :: B.ByteString -> GitOpResultType
_extractGitOpResultType result | isUpdated result  = Updated
                               | isReset result    = Reset
                               | isUpToDate result = UpToDate
                               | otherwise         = GeneralSuccess
 where
  isUpToDate = isPrefixOf "Already up to date" . C8.unpack
  isUpdated  = isPrefixOf "Updating" . C8.unpack
  isReset    = isPrefixOf "HEAD is now at" . C8.unpack

-- 
-- 
-- quasi constants
--
_mainBranches :: [B.ByteString]
_mainBranches = ["master", "main", "develop"]

--
-- TODO include remotes/origin/master etc...
--
_mainRemoteBranches :: [B.ByteString]
_mainRemoteBranches = ("remotes/origin/" <>) <$> _mainBranches
