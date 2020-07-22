{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run
  ( run
  )
where

import           Control.Monad.Extra            ( ifM )
import           Git
import           Logging
import           RIO
import           System.Directory               ( doesDirectoryExist
                                                , listDirectory
                                                , makeAbsolute
                                                , setCurrentDirectory
                                                )
import           System.FilePath                ( (</>) )
import           Types
import           Util                           ( spanM )

run :: RIO App ()
run = do
  root      <- view directoryL
  recursive <- view recursiveL
  master    <- view masterL
  logInput recursive master root
  listRepos recursive root >>= updateRepos master

listRepos :: Bool -> FilePath -> RIO App [FilePath]
listRepos recursive root = do
  (repos, subs) <- liftIO $ do
    path <- makeAbsolute root
    dirs <- ifM (doesDirectoryExist path) (listDirectories path) (return [])
    spanM isGitRepo dirs
  nested <- if recursive && not (null subs)
    then mapM (listRepos recursive) subs
    else return []
  return (repos ++ concat nested)

listDirectories :: FilePath -> IO [FilePath]
listDirectories path = map (path </>) <$> listDirectory path

updateRepos :: Bool -> [FilePath] -> RIO App ()
updateRepos master = mapM_ (updateRepo master)

updateRepo :: Bool -> FilePath -> RIO App ()
updateRepo master repo = do
  logRepo repo
  liftIO (setCurrentDirectory repo)
  gitPull
  when master (gitBranch >>= processBranch)

processBranch :: ReadProcessResult -> RIO App ()
processBranch (ExitSuccess, out, _) = unless (isMasterBranch out) $ do
  logInfo . fromString $ "Checkout and update master branch"
  gitCheckoutMaster
  gitPull
processBranch (ExitFailure code, _, err) =
  logError
    . fromString
    . concat
    $ ["Failed listing branches with code ", show code, " and error ", show err]

