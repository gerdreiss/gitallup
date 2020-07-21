{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run
  ( run
  )
where

import           Import

run :: RIO App ()
run = do
  directory <- view directoryL
  master    <- view masterL
  logInfo
    . fromString
    . concat
    $ [ "Updating "
      , if master then "master branches of the" else ""
      , " GIT repos in "
      , if directory == "." then "current directory" else directory
      , "..."
      ]
