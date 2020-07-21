{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run
  ( run
  )
where

import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.Char8    as C8
import           RIO
import           RIO.Process
import           System.Directory               ( doesDirectoryExist
                                                , listDirectory
                                                , makeAbsolute
                                                )
import           System.FilePath                ( (</>) )
import           Types

run :: RIO App ()
run = do
  root   <- view directoryL
  master <- view masterL
  _      <- logInput master root
  subs   <- listRepos
  updateRepos master subs

logInput :: Bool -> FilePath -> RIO App ()
logInput master root =
  logInfo
    . fromString
    . concat
    $ [ "Updating "
      , if master then "master branches of the " else " "
      , "GIT repos in "
      , resolveRoot root
      , "..."
      ]

resolveRoot :: String -> String
resolveRoot root | root == "."  = "current directory"
                 | root == ".." = "parent directory"
                 | root == "~"  = "home directory"
                 | otherwise    = root

listRepos :: RIO App [FilePath]
listRepos = do
  root <- view directoryL
  liftIO $ do
    path <- makeAbsolute root
    dirs <- map (root </>) <$> listDirectory path
    filterM isGitRepo dirs

isGitRepo :: FilePath -> IO Bool
isGitRepo dir = doesDirectoryExist $ dir </> ".git"

updateRepos :: Bool -> [FilePath] -> RIO App ()
updateRepos master = mapM_ (updateRepo master)

updateRepo :: Bool -> FilePath -> RIO App ()
updateRepo master repo = do
  logRepo repo
  proc "cd"  [repo]   runProcess_ -- the "cd" does not work - we're staying in the working directory all the time
  proc "git" ["pull"] runProcess_
  when master (updateMasterBranch repo)

updateMasterBranch :: FilePath -> RIO App ()
updateMasterBranch repo = do
  _      <- proc "cd" [repo] runProcess_
  pwd    <- proc "pwd" [] readProcess
  _      <- logInfo . fromString . show $ pwd
  result <- proc "git" ["branch"] readProcess
  processBranch result

processBranch :: ReadProcessResult -> RIO App ()
processBranch (ExitSuccess, out, _) = do
  logInfo . fromString . C8.unpack $ out
  unless (isMasterBranch out) $ do
    logInfo . fromString $ "Checkout and update master branch"
    proc "git" ["checkout", "master"] runProcess_
    proc "git" ["pull"]               runProcess_
processBranch (ExitFailure code, _, err) =
  logError
    . fromString
    . concat
    $ ["Failed listing branches with code", show code, " and error ", show err]

isMasterBranch :: B.ByteString -> Bool
isMasterBranch s = elem "* master" (lines . C8.unpack $ s)

logRepo :: FilePath -> RIO App ()
logRepo repo = logInfo . fromString $ "updating repo: " <> repo
