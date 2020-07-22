module Git
  ( isGitRepo
  , gitBranch
  , gitCheckoutMaster
  , gitPull
  )
where

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
