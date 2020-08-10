{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Options.Applicative
import           Options.Applicative.Simple
import           RIO
import           RIO.Process
import           Run
import           Types

import qualified Paths_gitallup

main :: IO ()
main = do
  (opts, ()) <-
    simpleOptions
      $(simpleVersion Paths_gitallup.version)
      "Update all GIT repos in directory"
      "Walks through all subdirectories of the given or current directory, and performs git pull on all GIT repos"
      options
      empty
  lo <- setLogUseColor True <$> logOptionsHandle stderr (optionsVerbose opts)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App {appLogFunc = lf, appProcessContext = pc, appOptions = opts}
     in runRIO app run

options :: Parser Options
options =
  Options
    <$> directory
    <*> recursive
    <*> recursiveDepth
    <*> master
    <*> verbose

directory :: Parser String
directory =
  strOption
    ( long "path"
        <> short 'p'
        <> help "Path to directory where to update all existing GIT repos"
        <> showDefault
        <> value "."
        <> metavar "PATH"
    )

recursive :: Parser Bool
recursive =
  switch
    ( long "recursive"
        <> short 'r'
        <> help "Go recursively through subdirectories which are not GIT repos?"
    )

recursiveDepth :: Parser Int
recursiveDepth =
  option
    auto
      ( long "depth"
          <> short 'd'
          <> showDefault
          <> value (-1) -- -1 means the depth is not restricted
          <> metavar "NUMBER"
          <> help "The depth of directory recursion"
      )

master :: Parser Bool
master =
  switch
    ( long "master"
        <> short 'm'
        <> help "Switch all to master branch?"
    )

verbose :: Parser Bool
verbose =
  switch
    ( long "verbose"
        <> short 'v'
        <> help "Verbose output?"
    )
