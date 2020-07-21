{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run
  ( run
  )
where

import           Import
import           System.Directory               ( doesDirectoryExist
                                                , listDirectory
                                                , makeAbsolute
                                                )
import           System.FilePath                ( (</>) )

run :: RIO App ()
run = do
  root   <- view directoryL
  master <- view masterL
  _      <- logInput root master
  subs   <- listRepos
  logRepos subs

logInput :: FilePath -> Bool -> RIO App ()
logInput root master =
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

logRepos :: [FilePath] -> RIO App ()
logRepos subs =
  logInfo . fromString . concat $ "updating repos:" : map ("\n--> " ++) subs

listRepos :: RIO App [FilePath]
listRepos = do
  root <- view directoryL
  liftIO $ do
    path <- makeAbsolute root
    dirs <- map (root </>) <$> listDirectory path
    filterM isGitRepo dirs

isGitRepo :: FilePath -> IO Bool
isGitRepo dir = doesDirectoryExist $ dir </> ".git"
