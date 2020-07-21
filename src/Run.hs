{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run
  ( run
  )
where

import           Import
import           System.Directory               ( listDirectory
                                                , makeAbsolute
                                                )
import           System.FilePath                ( (</>) )

run :: RIO App ()
run = do
  root   <- view directoryL
  master <- view masterL
  logInput root master
  subs <- listSubdirectories
  logInfo . fromString . concat $ "updating repos:" : map ("\n--> " ++) subs

logInput :: FilePath -> Bool -> RIO App ()
logInput root master =
  logInfo
    . fromString
    . concat
    $ [ "Updating "
      , if master then "master branches of the" else ""
      , " GIT repos in "
      , if root == "." then "current root" else root
      , "..."
      ]

listSubdirectories :: RIO App [FilePath]
listSubdirectories = do
  root <- view directoryL
  liftIO $ do
    a <- makeAbsolute root
    s <- listDirectory a
    return $ map (root </>) s
