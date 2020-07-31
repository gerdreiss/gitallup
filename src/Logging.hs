module Logging where

import           RIO
import           Types

logInput :: Bool -> Int -> Bool -> FilePath -> RIO App ()
logInput recursive depth master root =
  logInfo
    . fromString
    . concat
    $ [ "Updating "
      , recf recursive depth
      , masterf master
      , "GIT repos in "
      , resolveRoot root
      ]
 where
  recf r d = if r then "recursively " ++ recd d else " "
  recd d = if d > -1 then "up to a depth of " ++ show d else ""
  masterf m = if m then "master branches of the " else " "

resolveRoot :: String -> String
resolveRoot root | root == "."  = "current directory"
                 | root == ".." = "parent directory"
                 | root == "~"  = "home directory"
                 | otherwise    = root

logRepo :: FilePath -> RIO App ()
logRepo repo = logInfo . fromString $ "updating repo: " <> repo

logSuc :: String -> RIO App ()
logSuc suc = logInfo . fromString $ "Success: " ++ suc

logErr :: Int -> String -> RIO App ()
logErr code err =
  logError . fromString . concat $ ["Failed: ", show code, ": ", err]
