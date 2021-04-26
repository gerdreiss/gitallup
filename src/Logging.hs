module Logging
  ( logInput
  , logRepo
  , logAction
  , logMsg
  , logRes
  , logErr
  , debug
  , warn
  , error
  ) where

import qualified Data.ByteString.Lazy.Char8    as C8 -- TODO replace this with RIO's package or function

import           Control.Monad.Extra            ( ifM )
import           RIO                     hiding ( error
                                                , force
                                                )
import           RIO.Text                       ( unpack )
import           Types

--
--
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

--
--
logRepo :: FilePath -> RIO App ()
logRepo repo = ifM (view statusL)
                   (logMsg $ "checking status for repo: " <> repo)
                   (logMsg $ "updating repo: " <> repo)

--
--
logAction :: Text -> RepoUpdateResult -> RIO App ()
logAction action result = logMsg $ concat
  ["Executing command: ", unpack action, " in ", updateResultRepo result]

--
--
logRes :: String -> GitOpResult -> RIO App ()
logRes msg res =
  logMsg $ concat ["Success => ", msg, " ", show (resultType res)]

--
--
logMsg :: String -> RIO App ()
logMsg = logInfo . fromString

--
--
logErr :: GitOpError -> RIO App ()
logErr err = error $ concat
  ["Failed => ", show (errorCode err), " - ", C8.unpack (errorMessage err)]

--
--
debug :: String -> RIO App ()
debug = logDebug . fromString

--
--
error :: String -> RIO App ()
error = logError . fromString

--
--
warn :: String -> RIO App ()
warn = logWarn . fromString
