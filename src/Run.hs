{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run
  ( run
  )
where

import           Import

run :: RIO App ()
run = do
  App _ _ opts <- ask
  logInfo
    .  fromString
    $  "We're inside the application! Directory: "
    ++ (optionsDirectory $ opts)
