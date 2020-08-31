module Git
  ( isGitRepo
  , gitBranch
  , gitCheckoutMaster
  , gitPull
  , gitFetchAll
  , isMasterBranch
  )
where

import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.Char8    as C8
import           RIO
import           RIO.Process                    ( proc
                                                , readProcess
                                                )
import           System.Directory               ( doesDirectoryExist )
import           System.FilePath                ( (</>) )
import           Types
import           Logging

isGitRepo :: FilePath -> IO Bool
isGitRepo dir = doesDirectoryExist (dir </> ".git")

gitBranch :: RIO App ReadProcessResult
gitBranch = proc "git" ["branch"] readProcess

gitCheckoutMaster :: RIO App ()
gitCheckoutMaster =
  proc "git" ["checkout", "master"] readProcess >>= _processResult

gitPull :: RIO App ()
gitPull = proc "git" ["pull"] readProcess >>= _processResult

gitFetchAll :: RIO App ()
gitFetchAll = proc "git" ["fetch", "--all"] readProcess >>= _processResult

isMasterBranch :: B.ByteString -> Bool
isMasterBranch s = "* master" `elem` branches
  where branches = lines . C8.unpack $ s

_processResult :: ReadProcessResult -> RIO App ()
_processResult (ExitSuccess     , out, _  ) = logSuc (C8.unpack out)
_processResult (ExitFailure code, _  , err) = logErr code (C8.unpack err)
