module Logging
  ( logInput
  , logRepo
  , logMsg
  , logRes
  , logErr
  , debug
  , warn
  , error
  ) where

import qualified Data.ByteString.Lazy.Char8    as C8    -- TODO replace this with RIO's package or function

import           Control.Monad.Extra            ( ifM )
import           Data.List                      ( intercalate )
import           Data.List.Split                ( splitOn )
import           RIO                     hiding ( error
                                                , force
                                                )
import           Types

--
--
logInput :: RIO App ()
logInput = do
  status         <- view statusL
  cleanup        <- view cleanupL
  deleteBranches <- view deleteBranchesL
  force          <- view forceL
  recursive      <- view recursiveL
  depth          <- view recursiveDepthL
  main           <- view mainL
  root           <- view directoryL
  only           <- intercalate ", " . filter (/= []) . splitOn "," <$> view onlyL
  exclude        <- intercalate ", " . filter (/= []) . splitOn "," <$> view excludeL
  logInfo
    . fromString
    . concat
    $ [ if status
        then mkStrStatus
        else if cleanup
          then mkStrCleanup --
          else if deleteBranches
            then mkStrDelete --
            else mkStrUpdate force
      , mkStrRecursive recursive depth
      , mkStrMain main
      , "GIT repos in "
      , resolvePath root
      , mkStrOnly only
      , mkStrExclude exclude
      ]
 where
  mkStrStatus  = "Checking status "
  mkStrCleanup = "Cleaning "
  mkStrDelete  = "Deleting branches in"

  mkStrUpdate True  = "Force updating "
  mkStrUpdate False = "Updating "

  mkStrRecursive True  d = "recursively " ++ mkStrDepth d
  mkStrRecursive False _ = " "

  mkStrDepth d | d > -1    = "up to a depth of " ++ show d
               | otherwise = " "

  mkStrMain True  = "main branches of the "
  mkStrMain False = " "

  mkStrOnly [] = " "
  mkStrOnly x  = "including only " ++ x ++ " "

  mkStrExclude [] = " "
  mkStrExclude x  = if null x then " " else "excluding " ++ x

  resolvePath "."  = "current directory "
  resolvePath ".." = "parent directory "
  resolvePath "~"  = "home directory "
  resolvePath path = path ++ " "

--
--
logRepo :: FilePath -> RIO App ()
logRepo repo = ifM
  (view statusL)
  (logMsg $ "checking status for repo: " <> repo)
  (ifM
    (view cleanupL)
    (logMsg $ "cleaning up repo: " <> repo)
    (ifM (view deleteBranchesL)
         (logMsg $ "deleting branches for repo: " <> repo) --
         (logMsg $ "updating repo: " <> repo)
    )
  )

--
--
logRes :: String -> GitOpResult -> RIO App ()
logRes msg res = logMsg $ concat ["Success => ", msg, " ", show (resultType res)]

--
--
logMsg :: String -> RIO App ()
logMsg = logInfo . fromString

--
--
logErr :: GitOpError -> RIO App ()
logErr err = error $ concat ["Failed => ", show (errorCode err), " - ", C8.unpack (errorMessage err)]

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
