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
spanM _ []       = return ([], [])
spanM p (x : xs) = ifM (p x) f g
 where
  f = do
    (with, without) <- spanM p xs
    return (x : with, without)
  g = do
    return ([], x : xs)
