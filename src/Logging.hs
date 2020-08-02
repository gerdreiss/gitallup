module Logging
  ( logInput
  , logRepo
  , logSuc
  , logErr
  )
where

import           RIO
import           Types

logInput :: Bool -> Int -> Bool -> FilePath -> RIO App ()
logInput recursive depth master path =
  logInfo
    . fromString
    . concat
    $ [ "Updating "
      , recf recursive depth
      , masterf master
      , "GIT repos in "
      , _resolvePath path
      ]
 where
  recf r d = if r then "recursively " ++ depf d else " "
  depf d = if d > -1 then "up to a depth of " ++ show d else ""
  masterf m = if m then "master branches of the " else " "

logRepo :: FilePath -> RIO App ()
logRepo repo = logInfo . fromString $ "updating repo: " <> repo

logSuc :: String -> RIO App ()
logSuc suc = logInfo . fromString $ "Success: " ++ suc

logErr :: Int -> String -> RIO App ()
logErr code err =
  logError . fromString . concat $ ["Failed: ", show code, ": ", err]

_resolvePath :: String -> String
_resolvePath path | path == "."  = "current directory"
                  | path == ".." = "parent directory"
                  | path == "~"  = "home directory"
                  | otherwise    = path

