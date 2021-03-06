{-# LANGUAGE NoImplicitPrelude #-}
module Logging
  ( logInput
  , logRepo
  , logMsg
  , logMsgS
  , logRes
  , logResS
  , logErr
  , logErrE
  , logWrn
  , logWrnS
  , debugMsg
  , debugMsgS
  , errorMsg
  , errorMsgS
  )
where

import qualified RIO.ByteString.Lazy           as B

import           RIO                     hiding ( force )
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

logMsg :: B.ByteString -> RIO App ()
logMsg = logMsgS . show

logMsgS :: String -> RIO App ()
logMsgS = logInfo . fromString

logRes :: B.ByteString -> GitOpResult -> RIO App ()
logRes msg = logResS (show msg)

logResS :: String -> GitOpResult -> RIO App ()
logResS msg res =
  logInfo . fromString . concat $ ["Success => ", msg, " ", show res]

logErrE :: GitOpError -> RIO App ()
logErrE e = logErr (errorCode e) (errorMessage e)

logErr :: Int -> B.ByteString -> RIO App ()
logErr code msg =
  logError . fromString . concat $ ["Failed => ", show code, " - ", show msg]

logWrn :: B.ByteString -> RIO App ()
logWrn = logWrnS . show

logWrnS :: String -> RIO App ()
logWrnS msg = logWarn . fromString $ "Warning => " <> msg

debugMsg :: B.ByteString -> RIO App ()
debugMsg = logMsgS . show

debugMsgS :: String -> RIO App ()
debugMsgS = logDebug . fromString

errorMsg :: B.ByteString -> RIO App ()
errorMsg = logMsgS . show

errorMsgS :: String -> RIO App ()
errorMsgS = logError . fromString

_resolvePath :: FilePath -> FilePath
_resolvePath path | path == "."  = "current directory "
                  | path == ".." = "parent directory "
                  | path == "~"  = "home directory "
                  | otherwise    = path <> " "
