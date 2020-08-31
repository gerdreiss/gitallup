{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run
  ( run
  )
where

import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.Char8    as C8
import           Control.Monad.Extra            ( ifM
                                                , partitionM
                                                )
import           Data.List                      ( head
                                                , isPrefixOf
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
  master    <- view masterL
  force     <- view forceL
  exclude   <- view excludeL
  logInput recursive depth master force exclude root
  listRepos recursive depth (splitOn "," exclude) root
    >>= updateRepos master force

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
updateRepos master force = mapM_ (updateRepo master force)

updateRepo :: Bool -> Bool -> FilePath -> RIO App ()
updateRepo master force repo = do
  logRepo repo
  liftIO (setCurrentDirectory repo)
  when force (gitBranch >>= extractBranch >>= gitResetHard)
  gitPull
  when master (gitBranch >>= processBranch)

extractBranch :: ReadProcessResult -> RIO App B.ByteString
extractBranch (ExitSuccess     , out, _  ) = return
  ( C8.pack
  . drop 2 -- remove "* " from branch name
  . head -- GIT always returns the active branch prefixed with "* "
  . filter ("* " `isPrefixOf`)
  . lines
  . C8.unpack
  $ out
  )
extractBranch (ExitFailure code, _  , err) = do
  logWarn
    . fromString
    . concat
    $ [ "Failed extracting branch: "
      , show code
      , " - "
      , show err
      , "\nReturning 'master'..."
      ]
  return "master"

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
