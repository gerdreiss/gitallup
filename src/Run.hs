{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run
  ( run
  )
where

import           Control.Monad.Extra            ( ifM
                                                , partitionM
                                                )
import           Data.List.Split
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
  exclude   <- view excludeL
  logInput recursive depth master exclude root
  listRepos recursive depth (splitOn "," exclude) root >>= updateRepos master

listRepos :: Bool -> Int -> [String] -> FilePath -> RIO App [FilePath]
listRepos recursive depth excluded root = do
  (repos, rest) <- listReposAndRest excluded root
  nested        <- listNestedRepos recursive depth excluded rest
  return (repos ++ nested)

listReposAndRest :: [String] -> FilePath -> RIO App ([FilePath], [FilePath])
listReposAndRest excluded root =
  liftIO
    $   makeAbsolute root
    >>= listDirectories excluded
    >>= partitionM isGitRepo

listDirectories :: [String] -> FilePath -> IO [FilePath]
listDirectories excluded path = ifM
  (doesDirectoryExist path)
  (fmap (path </>) . filter (`notElem` excluded) <$> listDirectory path)
  (return [])

listNestedRepos :: Bool -> Int -> [String] -> [FilePath] -> RIO App [FilePath]
listNestedRepos recursive depth excluded subdirs
  | recursive && depth /= 0 && (not . null $ subdirs)
  = concat <$> mapM (listRepos True (depth - 1) excluded) subdirs
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
    $ ["Failed listing branches: ", show code, " - ", show err]
