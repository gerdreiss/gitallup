module Util
  ( spanM
  , removeDuplicatesComparingBy
  ) where

import           Control.Monad.Extra            ( ifM )
import           Data.List.Extra                ( groupOn )
import           RIO                            ( (.)
                                                , Bool
                                                , Monad(return)
                                                , Ord
                                                , map
                                                )
import           RIO.List                       ( sortOn )
import           RIO.List.Partial               ( head )

spanM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
spanM _          []       = return ([], [])
spanM predicateM (x : xs) = ifM (predicateM x) caseTrue caseFalse
 where
  caseTrue  = do
    (with, without) <- spanM predicateM xs
    return (x : with, without)
  caseFalse = return ([], x : xs)

removeDuplicatesComparingBy :: Ord b => (a -> b) -> [a] -> [a]
removeDuplicatesComparingBy f = map head . groupOn f . sortOn f
