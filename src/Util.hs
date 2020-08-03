{-# LANGUAGE NoImplicitPrelude #-}
module Util
  ( plus2
  , spanM
  )
where

import           Control.Monad.Extra            ( ifM )
import           RIO

plus2 :: Int -> Int
plus2 = (+ 2)

spanM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
spanM _          []       = return ([], [])
spanM predicateM (x : xs) = ifM (predicateM x) caseTrue caseFalse
 where
  caseTrue  = do
    (with, without) <- spanM predicateM xs
    return (x : with, without)
  caseFalse = return ([], x : xs)
