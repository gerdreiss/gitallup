module Summary
  ( printSummary
  ) where

import qualified Logging                       as Log

import           Control.Monad.Extra            ( ifM )
import           RIO                     hiding ( force )
import           RIO.List                       ( partition )
import           Text.Pretty.Simple             ( pPrint )
import           Types

--
--
-- prints the status or update summary
--
printSummary :: [RepoUpdateResult] -> RIO App ()
printSummary results = do
  Log.logMsg "\n\n============================================================"

  let newResults = filter (not . withResultType GeneralSuccess) results
      errors     = filter (isLeft . updateErrorOrSuccess) newResults

  Log.logMsg $ "Repos processed  : " ++ show (length newResults)
  Log.logMsg $ "Errors occurred  : " ++ show (length errors)

  ifM
    (view statusL)
    (printStatusSummary newResults)
    (ifM (view cleanupL)
         (printCleanupSummary newResults)
         (ifM (view deleteBranchesL) (printDeleteSummary newResults) (printUpdateSummary newResults))
    )

--
--
printStatusSummary :: [RepoUpdateResult] -> RIO App ()
printStatusSummary results = do
  let (errors, successes) = partition (isLeft . updateErrorOrSuccess) results
      dirty               = filter isDirty successes

  Log.logMsg $ "Repos dirty      : " ++ show (length dirty)
  Log.logMsg "\n"

  mapM_ pPrint errors
  mapM_ pPrint dirty
  where isDirty res = either (const False) ((== Dirty) . resultType) (updateErrorOrSuccess res)

--
--
printUpdateSummary :: [RepoUpdateResult] -> RIO App ()
printUpdateSummary results = do
  let (errors, successes) = partition (isLeft . updateErrorOrSuccess) results
      upToDate            = filter isUpToDate successes
      updated             = filter isUpdated successes

  Log.logMsg $ "Repos up to date : " ++ show (length upToDate)
  Log.logMsg $ "Repos updated    : " ++ show (length updated)
  Log.logMsg "\n"

  mapM_ pPrint errors
  mapM_ pPrint updated

 where
  isUpToDate res = either (const False) ((== UpToDate) . resultType) (updateErrorOrSuccess res)
  isUpdated res = either (const False) ((`elem` [Updated, Reset, ActionExecuted]) . resultType) (updateErrorOrSuccess res)

printCleanupSummary :: [RepoUpdateResult] -> RIO App ()
printCleanupSummary results = do
  let (errors, successes) = partition (isLeft . updateErrorOrSuccess) results
      cleanedUp           = filter isCleanedUp successes

  Log.logMsg $ "Repos cleaned up : " ++ show (length cleanedUp)

  mapM_ pPrint errors
  mapM_ pPrint cleanedUp
  where isCleanedUp res = either (const False) ((== CleanedUp) . resultType) (updateErrorOrSuccess res)


printDeleteSummary :: [RepoUpdateResult] -> RIO App ()
printDeleteSummary results = do
  let (errors, successes) = partition (isLeft . updateErrorOrSuccess) results
      deleted             = filter isDeleted successes

  Log.logMsg $ "Branches deleted : " ++ show (length deleted)

  mapM_ pPrint errors
  mapM_ pPrint deleted
  where isDeleted res = either (const False) ((== Deleted) . resultType) (updateErrorOrSuccess res)
