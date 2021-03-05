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

import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.Char8    as C8

import           RIO                            ( ExitCode
                                                  ( ExitFailure
                                                  , ExitSuccess
                                                  )
                                                , RIO
                                                )
import           RIO.Process                    ( proc
                                                , readProcess
                                                )
import           System.Directory               ( doesDirectoryExist )
import           System.FilePath                ( (</>) )
import           Types                          ( App
                                                , GitOpError(GitOpError)
                                                , GitOpResult(..)
                                                , ReadProcessResult
                                                )
import           Data.List                      ( find )

--
--
-- Main module functions
-- 

listBranches :: FilePath -> RIO App (Either GitOpError [B.ByteString])
listBranches repo =
  _extractBranchList <$> proc "git" ["-C", repo, "branch"] readProcess

currentBranch :: FilePath -> RIO App (Either GitOpError (Maybe B.ByteString))
currentBranch repo =
  _extractCurrentBranch <$> proc "git" ["-C", repo, "branch"] readProcess

mainBranch :: FilePath -> RIO App (Either GitOpError (Maybe B.ByteString))
mainBranch repo =
  _extractMainBranch <$> proc "git" ["-C", repo, "branch"] readProcess

switchBranch :: FilePath -> B.ByteString -> RIO App (Either GitOpError ())
switchBranch repo branch =
  _extractGitOpErrorOrUnit
    <$> proc "git" ["-C", repo, "checkout", C8.unpack branch] readProcess

updateBranch :: FilePath -> RIO App (Either GitOpError GitOpResult)
updateBranch repo =
  proc "git" ["-C", repo, "fetch", "--all"] readProcess
    >>  _extractGitOpErrorOrResult
    <$> proc "git" ["-C", repo, "pull"] readProcess

resetHard :: FilePath -> B.ByteString -> RIO App (Either GitOpError GitOpResult)
resetHard repo branch =
  _extractGitOpErrorOrResult
    <$> proc "git"
             ["-C", repo, "reset", "--hard", "origin/" ++ C8.unpack branch]
             readProcess

isDirty :: FilePath -> RIO App (Either GitOpError Bool)
isDirty repo =
  _extractBranchIsDirty <$> proc "git" ["-C", repo, "status"] readProcess

--
--
-- Utilities
--

isGitRepo :: FilePath -> IO Bool
isGitRepo dir = doesDirectoryExist (dir </> ".git")

isMainBranch :: B.ByteString -> Bool
isMainBranch s = any (`elem` _mainBranches) branches
  where branches = C8.lines s

-- 
-- 
-- Private functions
--

_extractBranchList :: ReadProcessResult -> Either GitOpError [B.ByteString]
_extractBranchList (ExitSuccess, out, _) =
  Right . fmap (C8.drop 2) . C8.lines $ out
_extractBranchList (ExitFailure code, _, err) = Left $ GitOpError code err

_extractCurrentBranch
  :: ReadProcessResult -> Either GitOpError (Maybe B.ByteString)
_extractCurrentBranch (ExitSuccess, out, _) =
  Right
    . fmap (C8.drop 2)
    . find (C8.isPrefixOf . C8.pack $ "* ")
    . C8.lines
    $ out
_extractCurrentBranch (ExitFailure code, _, err) = Left $ GitOpError code err

_extractMainBranch
  :: ReadProcessResult -> Either GitOpError (Maybe B.ByteString)
_extractMainBranch (ExitSuccess, out, _) =
  Right . find (`elem` _mainBranches) . fmap (C8.drop 2) . C8.lines $ out
_extractMainBranch (ExitFailure code, _, err) = Left $ GitOpError code err

_extractBranchIsDirty :: ReadProcessResult -> Either GitOpError Bool
_extractBranchIsDirty (ExitSuccess, out, _) =
  Right . not $ C8.isSuffixOf (C8.pack "working tree clean") out
_extractBranchIsDirty (ExitFailure code, _, err) = Left $ GitOpError code err

_extractGitOpErrorOrUnit :: ReadProcessResult -> Either GitOpError ()
_extractGitOpErrorOrUnit (ExitSuccess, _, _) = Right ()
_extractGitOpErrorOrUnit (ExitFailure code, _, err) =
  Left $ GitOpError code err

_extractGitOpErrorOrResult :: ReadProcessResult -> Either GitOpError GitOpResult
_extractGitOpErrorOrResult (ExitSuccess, out, _) =
  Right $ _extractGitOpResult out
_extractGitOpErrorOrResult (ExitFailure code, _, err) =
  Left $ GitOpError code err

_extractGitOpResult :: B.ByteString -> GitOpResult
_extractGitOpResult result
  | C8.isPrefixOf (C8.pack "Already up to date") result = UpToDate
  | C8.isPrefixOf (C8.pack "Updating") result = Updated
  | otherwise = GeneralSuccess

-- 
-- 
-- quasi constants
--

_mainBranches :: [B.ByteString]
_mainBranches = C8.pack <$> ["master", "main", "develop"]
