module Logging
  ( logInput
  , logRepo
  )
where

import           RIO
import           Types

logInput :: Bool -> Bool -> FilePath -> RIO App ()
logInput recursive master root =
  logInfo
    . fromString
    . concat
    $ [ "Updating "
      , if recursive then "recursively " else " "
      , if master then "master branches of the " else " "
      , "GIT repos in "
      , resolveRoot root
      ]

resolveRoot :: String -> String
resolveRoot root | root == "."  = "current directory"
                 | root == ".." = "parent directory"
                 | root == "~"  = "home directory"
                 | otherwise    = root

logRepo :: FilePath -> RIO App ()
logRepo repo = logInfo . fromString $ "updating repo: " <> repo
