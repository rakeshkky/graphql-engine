module Hasura.RQL.DDL.Relationship.Rename
  (runRenameRel)
where

import           Hasura.Prelude
import           Hasura.EncJSON
import           Hasura.RQL.DDL.Relationship       (validateRelP1)
import           Hasura.RQL.DDL.Relationship.Types
import           Hasura.RQL.DDL.Schema.Rename      (renameRelInCatalog)
import           Hasura.RQL.DDL.Schema.Table       (buildSchemaCache)
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Data.HashMap.Strict               as HM

renameRelP2
  :: ( QErrM m
     , MonadTx m
     , CacheRWM m
     , MonadIO m
     , HasHttpManager m
     , HasServeOptsCtx m
     )
  => QualifiedTable -> RelName -> RelInfo -> m ()
renameRelP2 qt newRN relInfo = do
  tabInfo <- askTabInfo qt
  -- check for conflicts in fieldInfoMap
  case HM.lookup (fromRel newRN) $ tiFieldInfoMap tabInfo of
    Nothing -> return ()
    Just _  ->
      throw400 AlreadyExists $ "cannot rename relationship " <> oldRN
      <<> " to " <> newRN <<> " in table " <> qt <<>
      " as a column/relationship with the name already exists"
  -- update catalog
  renameRelInCatalog qt oldRN newRN
  -- update schema cache
  buildSchemaCache
  where
    oldRN = riName relInfo

runRenameRel
  :: ( QErrM m
     , CacheRWM m
     , MonadTx m
     , UserInfoM m
     , MonadIO m
     , HasHttpManager m
     , HasServeOptsCtx m
     )
  => RenameRel -> m EncJSON
runRenameRel defn = do
  ri <- validateRelP1 qt rn
  renameRelP2 qt newRN ri
  return successMsg
  where
    RenameRel qt rn newRN = defn
