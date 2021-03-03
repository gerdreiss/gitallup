module Git
  ( isGitRepo
  , gitBranch
  , gitCheckoutMain
  , gitPull
  , gitFetchAll
  , gitResetHard
  , isMainBranch
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

isGitRepo :: FilePath -> IO Bool
isGitRepo dir = doesDirectoryExist (dir </> ".git")

gitBranch :: RIO App ReadProcessResult
gitBranch = proc "git" ["branch"] readProcess

gitCheckoutMain :: B.ByteString -> RIO App ()
gitCheckoutMain branch =
  proc "git" ["checkout", C8.unpack branch] readProcess >>= _processResult

gitPull :: RIO App ()
gitPull = proc "git" ["pull"] readProcess >>= _processResult

gitFetchAll :: RIO App ()
gitFetchAll = proc "git" ["fetch", "--all"] readProcess >>= _processResult

gitResetHard :: B.ByteString -> RIO App ()
gitResetHard branch =
  proc "git" ["reset", "--hard", "origin/" ++ C8.unpack branch] readProcess
    >>= _processResult

isMainBranch :: B.ByteString -> Bool
isMainBranch s = any (`elem` _mainBranches True) branches
  where branches = C8.lines s

extractMainBranch :: B.ByteString -> Maybe B.ByteString
extractMainBranch s = C8.drop 2 <$> find (`elem` _mainBranches False) branches
  where branches = C8.lines s

_processResult :: ReadProcessResult -> RIO App ()
_processResult (ExitSuccess     , out, _  ) = logSuc (C8.unpack out)
_processResult (ExitFailure code, _  , err) = logErr code (C8.unpack err)

_mainBranches :: Bool -> [B.ByteString]
_mainBranches prefixed = C8.pack <$> map prefix ["main", "master"]
  where prefix = if prefixed then ("* " ++) else ("  " ++)
