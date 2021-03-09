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
import qualified Data.ByteString.Lazy.Char8    as C8

import           Data.List                      ( find )
import           RIO
import           RIO.Process                    ( proc
                                                , readProcess
                                                )
import           System.Directory               ( doesDirectoryExist )
import           System.FilePath                ( (</>) )
import           Types
import           Data.List.Extra                ( isPrefixOf )

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
  :: FilePath -> B.ByteString -> RIO App (Either GitOpError GitOpSuccess)
switchBranch repo branch =
  _extractGitOpErrorOrResult
    <$> proc "git" ["-C", repo, "checkout", C8.unpack branch] readProcess

--
--
updateBranch :: FilePath -> RIO App (Either GitOpError GitOpSuccess)
updateBranch repo =
  proc "git" ["-C", repo, "fetch", "--all"] readProcess
    >>  _extractGitOpErrorOrResult
    <$> proc "git" ["-C", repo, "pull"] readProcess

--
--
resetHard
  :: FilePath -> B.ByteString -> RIO App (Either GitOpError GitOpSuccess)
resetHard repo branch =
  _extractGitOpErrorOrResult
    <$> proc "git"
             ["-C", repo, "reset", "--hard", "origin/" <> C8.unpack branch]
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
_extractGitOpErrorOrResult
  :: ReadProcessResult -> Either GitOpError GitOpSuccess
_extractGitOpErrorOrResult (ExitSuccess, out, _) =
  Right $ _extractGitOpResult out
_extractGitOpErrorOrResult (ExitFailure code, _, err) =
  Left $ GitOpError code err

--
--
_extractGitOpResult :: B.ByteString -> GitOpSuccess
_extractGitOpResult result
  | "Already up to date" `isPrefixOf` C8.unpack result = UpToDate
  | "Updating" `isPrefixOf` C8.unpack result = Updated
  | "HEAD is now at" `isPrefixOf` C8.unpack result = Updated
  | otherwise = GeneralSuccess

-- 
-- 
-- quasi constants
--
-- TODO include remotes/origin/master etc...
_mainBranches :: [B.ByteString]
_mainBranches = ["master", "main", "develop"]

_mainRemoteBranches :: [B.ByteString]
_mainRemoteBranches = ("remotes/origin/" <>) <$> _mainBranches
