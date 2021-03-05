module Git
  ( listBranches
  , currentBranch
  , mainBranch
  , switchBranch
  , updateBranch
  -- Utilities
  , isGitRepo
  , isMainBranch
  -- Basic functions
  , gitBranch
  -- Deprecated functions
  , gitPull
  , gitFetchAll
  , gitCheckoutBranch
  , gitResetHard
  , extractMainBranch
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
import           Types
import           Logging
import           Data.List                      ( find )

--
--
-- Main module functions
-- 

listBranches :: RIO App (Either GitOpError [B.ByteString])
listBranches = _extractBranchList <$> gitBranch

currentBranch :: RIO App (Either GitOpError (Maybe B.ByteString))
currentBranch = _extractCurrentBranch <$> gitBranch

mainBranch :: RIO App (Either GitOpError (Maybe B.ByteString))
mainBranch = _extractMainBranch <$> gitBranch

switchBranch :: B.ByteString -> RIO App (Either GitOpError ())
switchBranch b = _extractGitOpErrorOrUnit <$> gitSwitchBranch b

updateBranch :: RIO App (Either GitOpError GitOpResult)
updateBranch = gitFetchAll0 >> _extractGitOpErrorOrResult <$> gitPull0

--
--
-- Utilities
--

isGitRepo :: FilePath -> IO Bool
isGitRepo dir = doesDirectoryExist (dir </> ".git")

isMainBranch :: B.ByteString -> Bool
isMainBranch s = any (`elem` _mainBranches True) branches
  where branches = C8.lines s

--
--
-- GIT basic ops
--

gitBranch :: RIO App ReadProcessResult
gitBranch = proc "git" ["branch"] readProcess

gitSwitchBranch :: B.ByteString -> RIO App ReadProcessResult
gitSwitchBranch b = proc "git" ["switch", C8.unpack b] readProcess

gitPull0 :: RIO App ReadProcessResult
gitPull0 = proc "git" ["pull"] readProcess

gitFetchAll0 :: RIO App ReadProcessResult
gitFetchAll0 = proc "git" ["fetch", "--all"] readProcess

--
-- 
-- Deprecated functions
--

gitPull :: RIO App ()
gitPull = proc "git" ["pull"] readProcess >>= _processResult

gitFetchAll :: RIO App ()
gitFetchAll = proc "git" ["fetch", "--all"] readProcess >>= _processResult

gitCheckoutBranch :: B.ByteString -> RIO App ()
gitCheckoutBranch branch =
  proc "git" ["checkout", C8.unpack branch] readProcess >>= _processResult

gitResetHard :: B.ByteString -> RIO App ()
gitResetHard branch =
  proc "git" ["reset", "--hard", "origin/" ++ C8.unpack branch] readProcess
    >>= _processResult

extractMainBranch :: B.ByteString -> Maybe B.ByteString
extractMainBranch s = C8.drop 2 <$> find (`elem` _mainBranches False) branches
  where branches = C8.lines s

-- 
-- 
-- Private functions
--

_extractBranchList :: ReadProcessResult -> Either GitOpError [B.ByteString]
_extractBranchList (ExitSuccess     , out, _  ) = Right (C8.lines out)
_extractBranchList (ExitFailure code, _  , err) = Left (GitOpError code err)

_extractCurrentBranch
  :: ReadProcessResult -> Either GitOpError (Maybe B.ByteString)
_extractCurrentBranch (ExitSuccess, out, _) =
  Right $ find (C8.isPrefixOf (C8.pack "* ")) (C8.lines out)
_extractCurrentBranch (ExitFailure code, _, err) = Left (GitOpError code err)

_extractMainBranch
  :: ReadProcessResult -> Either GitOpError (Maybe B.ByteString)
_extractMainBranch (ExitSuccess, out, _) =
  Right $ find (`elem` _mainBranches True) (C8.lines out)
_extractMainBranch (ExitFailure code, _, err) = Left (GitOpError code err)

_mainBranches :: Bool -> [B.ByteString]
_mainBranches selected = C8.pack <$> map prefix ["main", "master"]
  where prefix = if selected then ("* " ++) else ("  " ++)

_extractGitOpErrorOrUnit :: ReadProcessResult -> Either GitOpError ()
_extractGitOpErrorOrUnit (ExitSuccess     , _, _  ) = Right ()
_extractGitOpErrorOrUnit (ExitFailure code, _, err) = Left opError
  where opError = GitOpError code err

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

_processResult :: ReadProcessResult -> RIO App ()
_processResult (ExitSuccess     , out, _  ) = logSuc (C8.unpack out)
_processResult (ExitFailure code, _  , err) = logErr code (C8.unpack err)

