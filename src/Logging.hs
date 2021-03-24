module Logging
  ( logInput
  , logRepo
  , logMsg
  , logRes
  , logErr
  , debug
  , error
  ) where

import qualified Data.ByteString.Lazy.Char8    as C8                 -- TODO replace this with RIO's package or function

import           RIO                     hiding ( error
                                                , force
                                                )
import           Types

logInput :: RIO App ()
logInput = do
  status    <- view statusL
  force     <- view forceL
  recursive <- view recursiveL
  depth     <- view recursiveDepthL
  main      <- view mainL
  root      <- view directoryL
  exclude   <- view excludeL
  logInfo
    . fromString
    . concat
    $ [ if status then mkStrStatus else mkStrUpdate force
      , mkStrRecursive recursive depth
      , mkStrMain main
      , "GIT repos in "
      , resolvePath root
      , mkStrExclude exclude
      ]
 where
  mkStrStatus = "Checking status "

  mkStrUpdate True  = "Force updating "
  mkStrUpdate False = "Updating "

  mkStrRecursive True  d = "recursively " ++ mkStrDepth d
  mkStrRecursive False _ = " "

  mkStrDepth d | d > -1    = "up to a depth of " ++ show d
               | otherwise = " "

  mkStrMain True  = "main branches of the "
  mkStrMain False = " "

  mkStrExclude [] = " "
  mkStrExclude x  = "excluding " ++ x

  resolvePath "."  = "current directory "
  resolvePath ".." = "parent directory "
  resolvePath "~"  = "home directory "
  resolvePath path = path ++ " "

logRepo :: FilePath -> RIO App ()
logRepo repo = do
  status <- view statusL
  logInfo
    .  fromString
    $  (if status then "checking status for repo: " else "updating repo: ")
    ++ repo

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
