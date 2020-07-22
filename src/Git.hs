module Git where

import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.Char8    as C8
import           RIO
import           RIO.Process                    ( proc
                                                , readProcess
                                                , runProcess_
                                                )
import           System.Directory               ( doesDirectoryExist )
import           System.FilePath                ( (</>) )
import           Types

isGitRepo :: FilePath -> IO Bool
isGitRepo dir = doesDirectoryExist (dir </> ".git")

gitBranch :: RIO App ReadProcessResult
gitBranch = proc "git" ["branch"] readProcess

gitCheckoutMaster :: RIO App ()
gitCheckoutMaster = proc "git" ["checkout", "master"] runProcess_

gitPull :: RIO App ()
gitPull = proc "git" ["pull"] runProcess_

isMasterBranch :: B.ByteString -> Bool
isMasterBranch s = "* master" `elem` branches
  where branches = lines . C8.unpack $ s
