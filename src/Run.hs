{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run
  ( run
  )
where

import           Control.Monad.Extra            ( ifM
                                                , partitionM
                                                )
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

run :: RIO App ()
run = do
  root      <- view directoryL
  recursive <- view recursiveL
  depth     <- view recursiveDepthL
  master    <- view masterL
  logInput recursive depth master root
  listRepos recursive depth root >>= updateRepos master

listRepos :: Bool -> Int -> FilePath -> RIO App [FilePath]
listRepos recursive depth root = do
  (repos, rest) <- listReposAndRest root
  nested        <- listNestedRepos recursive depth rest
  return (repos ++ nested)

listReposAndRest :: FilePath -> RIO App ([FilePath], [FilePath])
listReposAndRest root =
  liftIO $ makeAbsolute root >>= listDirectories >>= partitionM isGitRepo

listDirectories :: FilePath -> IO [FilePath]
listDirectories path = ifM (doesDirectoryExist path)
                           (fmap (path </>) <$> listDirectory path)
                           (return [])

listNestedRepos :: Bool -> Int -> [FilePath] -> RIO App [FilePath]
listNestedRepos recursive depth subdirs
  | recursive && depth > 0 && (not . null $ subdirs)
  = concat <$> mapM (listRepos True (depth - 1)) subdirs
  | otherwise
  = return []

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

