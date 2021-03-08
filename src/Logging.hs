{-# LANGUAGE NoImplicitPrelude #-}
module Logging
  ( logInput
  , logRepo
  , logMsg
  , logRes
  , logErr
  , teeLogErr
  , debug
  , error
  )
where

import           RIO                     hiding ( force
                                                , error
                                                )
import           Types

logInput :: Bool -> Int -> Bool -> Bool -> FilePath -> FilePath -> RIO App ()
logInput recursive depth main force exclude path =
  logInfo
    . fromString
    . concat
    $ [ mkStrUpdate force
      , mkStrRecursive recursive depth
      , mkStrMain main
      , "GIT repos in "
      , _resolvePath path
      , mkStrExclude exclude
      ]

 where
  mkStrUpdate f = if f then "Force updating " else "Updating "
  mkStrRecursive r d = if r then "recursively " <> mkStrDepth d else " "
  mkStrDepth d = if d > -1 then "up to a depth of " <> show d else " "
  mkStrMain m = if m then "main branches of the " else " "
  mkStrExclude x = if null x then " " else "excluding " <> exclude

logRepo :: FilePath -> RIO App ()
logRepo repo = logInfo . fromString $ "\nupdating repo: " <> repo

logMsg :: String -> RIO App ()
logMsg = logInfo . fromString

logRes :: String -> GitOpSuccess -> RIO App ()
logRes msg res =
  logInfo . fromString . concat $ ["Success => ", msg, " ", show res]

teeLogErr :: GitOpError -> RIO App GitOpError
teeLogErr err = logErr err >> return err

logErr :: GitOpError -> RIO App ()
logErr err =
  logError
    . fromString
    . concat
    $ ["Failed => ", show . errorCode $ err, " - ", show . errorMessage $ err]

debug :: String -> RIO App ()
debug = logDebug . fromString

error :: String -> RIO App ()
error = logError . fromString

_resolvePath :: FilePath -> FilePath
_resolvePath path | path == "."  = "current directory "
                  | path == ".." = "parent directory "
                  | path == "~"  = "home directory "
                  | otherwise    = path <> " "
