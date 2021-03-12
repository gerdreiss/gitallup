module Logging
  ( logInput
  , logRepo
  , logMsg
  , logRes
  , logErr
  , debug
  , error
  )
where

import qualified Data.ByteString.Lazy.Char8    as C8 -- TODO replace this with RIO's package or function

import           RIO                     hiding ( force
                                                , error
                                                )
import           Types

logInput :: Bool -> Int -> Bool -> Bool -> FilePath -> FilePath -> RIO App ()
logInput recursive depth main force exclude path =
  logInfo
    . fromString
    . concat
    $ [ mkStrUpdate
      , mkStrRecursive
      , mkStrMain
      , "GIT repos in "
      , _resolvePath path
      , mkStrExclude
      ]
 where
  mkStrUpdate    = if force then "Force updating " else "Updating "
  mkStrRecursive = if recursive then "recursively " ++ mkStrDepth else " "
  mkStrDepth     = if depth > -1 then "up to a depth of " ++ show depth else " "
  mkStrMain      = if main then "main branches of the " else " "
  mkStrExclude   = if null exclude then " " else "excluding " ++ exclude

logRepo :: FilePath -> RIO App ()
logRepo repo = logInfo . fromString $ "updating repo: " ++ repo

logMsg :: String -> RIO App ()
logMsg = logInfo . fromString

logRes :: String -> GitOpResult -> RIO App ()
logRes msg res =
  logInfo
    . fromString
    . concat
    $ ["Success => ", msg, " ", show (resultType res)]

logErr :: GitOpError -> RIO App ()
logErr err =
  logError
    . fromString
    . concat
    $ ["Failed => ", show (errorCode err), " - ", C8.unpack (errorMessage err)]

debug :: String -> RIO App ()
debug = logDebug . fromString

error :: String -> RIO App ()
error = logError . fromString

_resolvePath :: FilePath -> FilePath
_resolvePath path | path == "."  = "current directory "
                  | path == ".." = "parent directory "
                  | path == "~"  = "home directory "
                  | otherwise    = path ++ " "
