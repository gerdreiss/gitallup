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
                                                , setCurrentDirectory
                                                )
import           System.FilePath                ( (</>) )
import           Types

run :: RIO App ()
run = do
  root   <- view directoryL
  master <- view masterL
  _      <- logInput master root
  listRepos >>= updateRepos master

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
    dirs <- map (path </>) <$> listDirectory path
    filterM isGitRepo dirs

isGitRepo :: FilePath -> IO Bool
isGitRepo dir = doesDirectoryExist $ dir </> ".git"

updateRepos :: Bool -> [FilePath] -> RIO App ()
updateRepos master = mapM_ (updateRepo master)

updateRepo :: Bool -> FilePath -> RIO App ()
updateRepo master repo = do
  logRepo repo
  liftIO $ setCurrentDirectory repo
  gitPull
  when master updateMasterBranch

updateMasterBranch :: RIO App ()
updateMasterBranch = gitBranch >>= processBranch

processBranch :: ReadProcessResult -> RIO App ()
processBranch (ExitSuccess, out, _) = do
  unless (isMasterBranch out) $ do
    logInfo . fromString $ "Checkout and update master branch"
    gitCheckoutMaster
    gitPull
processBranch (ExitFailure code, _, err) =
  logError
    . fromString
    . concat
    $ ["Failed listing branches with code", show code, " and error ", show err]

gitBranch :: RIO App ReadProcessResult
gitBranch = proc "git" ["branch"] readProcess

gitPull :: RIO App ()
gitPull = proc "git" ["pull"] runProcess_

gitCheckoutMaster :: RIO App ()
gitCheckoutMaster = proc "git" ["checkout", "master"] runProcess_

isMasterBranch :: B.ByteString -> Bool
isMasterBranch s = "* master" `elem` branches
  where branches = lines . C8.unpack $ s

logRepo :: FilePath -> RIO App ()
logRepo repo = logInfo . fromString $ "updating repo: " <> repo
