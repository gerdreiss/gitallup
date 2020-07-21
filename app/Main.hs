{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main
  ( main
  ) where

import           Import
import           Options.Applicative
import           Options.Applicative.Simple
import qualified Paths_gitallup
import           RIO.Process
import           Run

main :: IO ()
main = do
  (opts, ()) <-
    simpleOptions
      $(simpleVersion Paths_gitallup.version)
      "Update all GIT repos in directory"
      "Walks through all subdirectories of the given or current directory, and performs git pull"
      options
      empty
  lo <- logOptionsHandle stderr (optionsVerbose opts)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App {appLogFunc = lf, appProcessContext = pc, appOptions = opts}
     in runRIO app run

options :: Parser Options
options =
  Options <$>
  strOption
    (long "directory" <>
     short 'd' <>
     help "Root directory where to update all existing GIT repos" <>
     showDefault <> value "." <> metavar "PATH") <*>
  switch (long "master" <> short 'm' <> help "Switch all to master branch?") <*>
  switch (long "verbose" <> short 'v' <> help "Verbose output?")
