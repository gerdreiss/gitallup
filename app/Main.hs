{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Options.Applicative
import           Options.Applicative.Simple
import           RIO                     hiding ( force )
import           RIO.Process                    ( mkDefaultProcessContext )
import           Run                            ( run )
import           Types

import qualified Paths_gitallup


main :: IO ()
main = do
  (opts, ()) <-
    simpleOptions
      $(simpleVersion Paths_gitallup.version)
      "Update all GIT repos in directory"
      "Walks through all subdirectories of the given or current directory, and checks the status or performs git pull on all GIT repos"
      options
      empty
  lo <- setLogUseColor True <$> logOptionsHandle stderr (optionsVerbose opts)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App { appLogFunc = lf, appProcessContext = pc, appOptions = opts }
     in runRIO app run

options :: Parser Options
options =
  Options
    <$> directory
    <*> recursive
    <*> recursiveDepth
    <*> status
    <*> mainBranch
    <*> force
    <*> exclude
    <*> actions
    <*> verbose   

directory :: Parser FilePath 
directory =
  strOption
    ( long "path"
        <> short 'p'
        <> metavar "PATH"
        <> showDefault
        <> value "."
        <> help "Path to directory where to update all existing GIT repos"
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
          <> metavar "NUMBER"
          <> showDefault
          <> value (-1) -- -1 means the depth is not restricted
          <> help "The depth of directory recursion"
      )

status :: Parser Bool
status =
  switch
    ( long "status"
        <> short 'S'
        <> help "Check status of the repositories."
    )

mainBranch :: Parser Bool
mainBranch =
  switch
    ( long "main-branch"
        <> short 'm'
        <> help "Switch all to main/default branch? Ignored when --status/-S is passed as parameter."
    )

force :: Parser Bool
force =
  switch
    ( long "force"
        <> short 'f'
        <> help "Force update overriding any local changes? Ignored when --status/-S is passed as parameter."
    )

exclude :: Parser FilePath
exclude =
  strOption
    ( long "exclude"
        <> short 'x'
        <> metavar "LIST"
        <> value ""
        <> help "List of directories/repositories to be excluded from updating, comma separated"
    )

actions :: Parser (Maybe FilePath)
actions =
  optional $ strOption
    ( long "actions"
        <> short 'a'
        <> metavar "FILE"
        <> help "Path to the file that contains actions for select repositories"
    )

verbose :: Parser Bool
verbose =
  switch
    ( long "verbose"
        <> short 'v'
        <> help "Verbose output?"
    )
