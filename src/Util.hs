{-# LANGUAGE NoImplicitPrelude #-}
module Util
  ( spanM
  )
where

import           Control.Monad.Extra            ( ifM )
import           RIO

spanM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
spanM _          []       = return ([], [])
spanM predicateM (x : xs) = ifM (predicateM x) caseTrue caseFalse
 where
  caseTrue  = do
    (with, without) <- spanM predicateM xs
    return (x : with, without)
  caseFalse = return ([], x : xs)
