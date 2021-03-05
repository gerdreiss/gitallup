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
import           RIO                     hiding ( force )
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
  main      <- view mainL
  force     <- view forceL
  exclude   <- view excludeL
  logInput recursive depth main force exclude root
  listRepos recursive depth (splitOn "," exclude) root
    >>= updateRepos main force

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

updateRepos :: Bool -> Bool -> [FilePath] -> RIO App ()
updateRepos main force = mapM_ (updateRepo main force)

updateRepo :: Bool -> Bool -> FilePath -> RIO App ()
updateRepo main force repo = do
  logRepo repo
  liftIO (setCurrentDirectory repo)
  when force gitResetHard -- TODO when force rollbackCurrentBranch
  gitPull
  when main (gitBranch >>= updateMainBranch)

updateMainBranch :: ReadProcessResult -> RIO App ()
updateMainBranch (ExitSuccess, out, _) = unless (isMainBranch out) $ do
  logInfo . fromString $ "Checkout and update main/master branch"
  maybe (logWrn "Main branch not found. Proceeding with current branch...")
        gitCheckoutBranch
        (extractMainBranch out)
  gitPull
updateMainBranch (ExitFailure code, _, err) =
  logError
    . fromString
    . concat
    $ ["Failed listing branches: ", show code, " - ", show err]
