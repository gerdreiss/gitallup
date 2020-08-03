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
      , mkStrRecursive recursive depth
      , mkStrMaster master
      , "GIT repos in "
      , _resolvePath path
      ]
 where
  mkStrRecursive r d = if r then "recursively " ++ mkStrDepth d else " "
  mkStrDepth d = if d > -1 then "up to a depth of " ++ show d else ""
  mkStrMaster m = if m then "master branches of the " else " "

logRepo :: FilePath -> RIO App ()
logRepo repo = logInfo . fromString $ "updating repo: " <> repo

logSuc :: String -> RIO App ()
logSuc msg = logInfo . fromString $ "Success: " ++ msg

logErr :: Int -> String -> RIO App ()
logErr code msg =
  logError . fromString . concat $ ["Failed: ", show code, ": ", msg]

_resolvePath :: String -> String
_resolvePath path | path == "."  = "current directory"
                  | path == ".." = "parent directory"
                  | path == "~"  = "home directory"
                  | otherwise    = path

