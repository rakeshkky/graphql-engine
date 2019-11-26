{-# LANGUAGE Arrows #-}

-- | Description: Create/delete SQL tables to/from Hasura metadata.
module Hasura.RQL.DDL.Schema.Table
  ( TrackTable(..)
  , runTrackTableQ
  , trackExistingTableOrViewP2

  , TrackTableV2(..)
  , runTrackTableV2Q

  , UntrackTable(..)
  , runUntrackTableQ

  , SetTableIsEnum(..)
  , runSetExistingTableIsEnumQ

  , SetTableCustomFields(..)
  , runSetTableCustomFieldsQV2

  , buildTableCache
  , delTableAndDirectDeps
  , processTableChanges
  ) where

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.Deps
import {-# SOURCE #-} Hasura.RQL.DDL.Schema.Cache
import           Hasura.RQL.DDL.Schema.Catalog
import           Hasura.RQL.DDL.Schema.Diff
import           Hasura.RQL.DDL.Schema.Enum
import           Hasura.RQL.DDL.Schema.Rename
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Catalog
import           Hasura.SQL.Types

import qualified Database.PG.Query             as Q
import qualified Hasura.GraphQL.Context        as GC
import qualified Hasura.GraphQL.Schema         as GS
import qualified Hasura.Incremental            as Inc
import qualified Language.GraphQL.Draft.Syntax as G

import           Control.Arrow.Extended
import           Control.Lens.Extended         hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Instances.TH.Lift             ()
import           Language.Haskell.TH.Syntax    (Lift)
import           Network.URI.Extended          ()

import qualified Data.HashMap.Strict.Extended  as M
import qualified Data.Text                     as T

data TrackTable
  = TrackTable
  { tName   :: !QualifiedTable
  , tIsEnum :: !Bool
  } deriving (Show, Eq, Lift)

instance FromJSON TrackTable where
  parseJSON v = withOptions <|> withoutOptions
    where
      withOptions = flip (withObject "TrackTable") v $ \o -> TrackTable
        <$> o .: "table"
        <*> o .:? "is_enum" .!= False
      withoutOptions = TrackTable <$> parseJSON v <*> pure False

instance ToJSON TrackTable where
  toJSON (TrackTable name isEnum)
    | isEnum = object [ "table" .= name, "is_enum" .= isEnum ]
    | otherwise = toJSON name

data SetTableIsEnum
  = SetTableIsEnum
  { stieTable  :: !QualifiedTable
  , stieIsEnum :: !Bool
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 4 snakeCase) ''SetTableIsEnum)

data UntrackTable =
  UntrackTable
  { utTable   :: !QualifiedTable
  , utCascade :: !(Maybe Bool)
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''UntrackTable)

-- | Track table/view, Phase 1:
-- Validate table tracking operation. Fails if table is already being tracked,
-- or if a function with the same name is being tracked.
trackExistingTableOrViewP1 :: (QErrM m, CacheRWM m) => QualifiedTable -> m ()
trackExistingTableOrViewP1 qt = do
  rawSchemaCache <- askSchemaCache
  when (M.member qt $ scTables rawSchemaCache) $
    throw400 AlreadyTracked $ "view/table already tracked : " <>> qt
  let qf = fmap (FunctionName . getTableTxt) qt
  when (M.member qf $ scFunctions rawSchemaCache) $
    throw400 NotSupported $ "function with name " <> qt <<> " already exists"

trackExistingTableOrViewP2
  :: (MonadTx m, CacheRWM m)
  => QualifiedTable -> SystemDefined -> Bool -> TableConfig -> m EncJSON
trackExistingTableOrViewP2 tableName systemDefined isEnum config = do
  sc <- askSchemaCache
  let defGCtx = scDefaultRemoteGCtx sc
  GS.checkConflictingNode defGCtx $ GS.qualObjectToName tableName
  saveTableToCatalog tableName systemDefined isEnum config
  buildSchemaCacheFor (MOTable tableName)
  return successMsg

runTrackTableQ
  :: (MonadTx m, CacheRWM m, HasSystemDefined m) => TrackTable -> m EncJSON
runTrackTableQ (TrackTable qt isEnum) = do
  trackExistingTableOrViewP1 qt
  systemDefined <- askSystemDefined
  trackExistingTableOrViewP2 qt systemDefined isEnum emptyTableConfig

data TrackTableV2
  = TrackTableV2
  { ttv2Table         :: !TrackTable
  , ttv2Configuration :: !TableConfig
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 4 snakeCase) ''TrackTableV2)

runTrackTableV2Q
  :: (MonadTx m, CacheRWM m, HasSystemDefined m) => TrackTableV2 -> m EncJSON
runTrackTableV2Q (TrackTableV2 (TrackTable qt isEnum) config) = do
  trackExistingTableOrViewP1 qt
  systemDefined <- askSystemDefined
  trackExistingTableOrViewP2 qt systemDefined isEnum config

runSetExistingTableIsEnumQ :: (MonadTx m, CacheRWM m) => SetTableIsEnum -> m EncJSON
runSetExistingTableIsEnumQ (SetTableIsEnum tableName isEnum) = do
  void $ askTabInfo tableName -- assert that table is tracked
  updateTableIsEnumInCatalog tableName isEnum
  buildSchemaCacheFor (MOTable tableName)
  return successMsg

data SetTableCustomFields
  = SetTableCustomFields
  { _stcfTable             :: !QualifiedTable
  , _stcfCustomRootFields  :: !GC.TableCustomRootFields
  , _stcfCustomColumnNames :: !CustomColumnNames
  } deriving (Show, Eq, Lift)
$(deriveToJSON (aesonDrop 5 snakeCase) ''SetTableCustomFields)

instance FromJSON SetTableCustomFields where
  parseJSON = withObject "SetTableCustomFields" $ \o ->
    SetTableCustomFields
    <$> o .: "table"
    <*> o .:? "custom_root_fields" .!= GC.emptyCustomRootFields
    <*> o .:? "custom_column_names" .!= M.empty

runSetTableCustomFieldsQV2
  :: (MonadTx m, CacheRWM m) => SetTableCustomFields -> m EncJSON
runSetTableCustomFieldsQV2 (SetTableCustomFields tableName rootFields columnNames) = do
  let tableConfig = TableConfig rootFields columnNames
  updateTableConfig tableName tableConfig
  buildSchemaCacheFor (MOTable tableName)
  return successMsg

unTrackExistingTableOrViewP1
  :: (CacheRM m, QErrM m) => UntrackTable -> m ()
unTrackExistingTableOrViewP1 (UntrackTable vn _) = do
  rawSchemaCache <- askSchemaCache
  case M.lookup vn (scTables rawSchemaCache) of
    Just ti ->
      -- Check if table/view is system defined
      when (isSystemDefined $ _tciSystemDefined $ _tiCoreInfo ti) $ throw400 NotSupported $
        vn <<> " is system defined, cannot untrack"
    Nothing -> throw400 AlreadyUntracked $
      "view/table already untracked : " <>> vn

unTrackExistingTableOrViewP2
  :: (CacheRWM m, MonadTx m)
  => UntrackTable -> m EncJSON
unTrackExistingTableOrViewP2 (UntrackTable qtn cascade) = do
  sc <- askSchemaCache

  -- Get relational, query template and function dependants
  let allDeps = getDependentObjs sc (SOTable qtn)
      indirectDeps = filter (not . isDirectDep) allDeps

  -- Report bach with an error if cascade is not set
  when (indirectDeps /= [] && not (or cascade)) $ reportDepsExt indirectDeps []

  -- Purge all the dependents from state
  mapM_ purgeDependentObject indirectDeps

  -- delete the table and its direct dependencies
  delTableAndDirectDeps qtn

  return successMsg
  where
    isDirectDep = \case
      (SOTableObj dtn _) -> qtn == dtn
      _                  -> False

runUntrackTableQ
  :: (CacheRWM m, MonadTx m)
  => UntrackTable -> m EncJSON
runUntrackTableQ q = do
  unTrackExistingTableOrViewP1 q
  unTrackExistingTableOrViewP2 q

processTableChanges :: (MonadTx m, CacheRM m) => TableCoreInfo FieldInfo -> TableDiff -> m ()
processTableChanges ti tableDiff = do
  -- If table rename occurs then don't replace constraints and
  -- process dropped/added columns, because schema reload happens eventually
  sc <- askSchemaCache
  let tn = _tciName ti
      withOldTabName = do
        procAlteredCols sc tn

      withNewTabName newTN = do
        let tnGQL = GS.qualObjectToName newTN
            defGCtx = scDefaultRemoteGCtx sc
        -- check for GraphQL schema conflicts on new name
        GS.checkConflictingNode defGCtx tnGQL
        procAlteredCols sc tn
        -- update new table in catalog
        renameTableInCatalog newTN tn

  -- Process computed field diff
  processComputedFieldDiff tn
  -- Drop custom column names for dropped columns
  possiblyDropCustomColumnNames tn
  maybe withOldTabName withNewTabName mNewName
  where
    TableDiff mNewName droppedCols _ alteredCols _ computedFieldDiff _ _ = tableDiff

    possiblyDropCustomColumnNames tn = do
      let TableConfig customFields customColumnNames = _tciCustomConfig ti
          modifiedCustomColumnNames = foldl' (flip M.delete) customColumnNames droppedCols
      when (modifiedCustomColumnNames /= customColumnNames) $
        liftTx $ updateTableConfig tn $ TableConfig customFields modifiedCustomColumnNames

    procAlteredCols sc tn = for_ alteredCols $
      \( PGRawColumnInfo oldName oldType _ _ _
       , PGRawColumnInfo newName newType _ _ _ ) -> do
        if | oldName /= newName -> renameColInCatalog oldName newName tn (_tciFieldInfoMap ti)

           | oldType /= newType -> do
              let colId = SOTableObj tn $ TOCol oldName
                  typeDepObjs = getDependentObjsWith (== DROnType) sc colId

              unless (null typeDepObjs) $ throw400 DependencyError $
                "cannot change type of column " <> oldName <<> " in table "
                <> tn <<> " because of the following dependencies : " <>
                reportSchemaObjs typeDepObjs

           | otherwise -> pure ()

    processComputedFieldDiff table  = do
      let ComputedFieldDiff _ altered overloaded = computedFieldDiff
          getFunction = fmFunction . ccmFunctionMeta
      forM_ overloaded $ \(columnName, function) ->
        throw400 NotSupported $ "The function " <> function
        <<> " associated with computed field" <> columnName
        <<> " of table " <> table <<> " is being overloaded"
      forM_ altered $ \(old, new) ->
        if | (fmType . ccmFunctionMeta) new == FTVOLATILE ->
             throw400 NotSupported $ "The type of function " <> getFunction old
             <<> " associated with computed field " <> ccmName old
             <<> " of table " <> table <<> " is being altered to \"VOLATILE\""
           | otherwise -> pure ()

delTableAndDirectDeps :: (MonadTx m) => QualifiedTable -> m ()
delTableAndDirectDeps qtn@(QualifiedObject sn tn) = do
  liftTx $ Q.catchE defaultTxErrorHandler $ do
    Q.unitQ [Q.sql|
             DELETE FROM "hdb_catalog"."hdb_relationship"
             WHERE table_schema = $1 AND table_name = $2
              |] (sn, tn) False
    Q.unitQ [Q.sql|
             DELETE FROM "hdb_catalog"."hdb_permission"
             WHERE table_schema = $1 AND table_name = $2
              |] (sn, tn) False
    Q.unitQ [Q.sql|
             DELETE FROM "hdb_catalog"."event_triggers"
             WHERE schema_name = $1 AND table_name = $2
              |] (sn, tn) False
    Q.unitQ [Q.sql|
             DELETE FROM "hdb_catalog"."hdb_computed_field"
             WHERE table_schema = $1 AND table_name = $2
              |] (sn, tn) False
  deleteTableFromCatalog qtn

-- | Builds an initial @'TableCache' 'PGColumnInfo'@ from catalog information. Does not fill in
-- '_tiRolePermInfoMap' or '_tiEventTriggerInfoMap' at all, and '_tiFieldInfoMap' only contains
-- columns, not relationships; those pieces of information are filled in by later stages.
buildTableCache
  :: forall arr m
   . (Inc.ArrowDistribute arr, ArrowWriter (Seq CollectedInfo) arr, ArrowKleisli m arr, MonadTx m)
  => [CatalogTable] `arr` M.HashMap QualifiedTable (TableCoreInfo PGColumnInfo)
buildTableCache = proc catalogTables -> do
  rawTableInfos <-
    (| Inc.keyed (| withTable (\tables -> buildRawTableInfo <<< noDuplicates -< tables) |)
    |) (M.groupOnNE _ctName catalogTables)
  let rawTableCache = M.catMaybes rawTableInfos
      enumTables = M.mapMaybe _tciEnumValues rawTableCache
  tableInfos <-
    (| Inc.keyed (| withTable (\table -> processTableInfo -< (enumTables, table)) |)
    |) rawTableCache
  returnA -< M.catMaybes tableInfos
  where
    withTable :: ErrorA QErr arr (e, s) a -> arr (e, (QualifiedTable, s)) (Maybe a)
    withTable f = withRecordInconsistency f <<<
      second (first $ arr \name -> MetadataObject (MOTable name) (toJSON name))

    noDuplicates = proc tables -> case tables of
      table :| [] -> returnA -< table
      _           -> throwA -< err400 AlreadyExists "duplication definition for table"

    -- Step 1: Build the raw table cache from metadata information.
    buildRawTableInfo :: ErrorA QErr arr CatalogTable (TableCoreInfo PGRawColumnInfo)
    buildRawTableInfo = proc (CatalogTable name systemDefined isEnum config maybeInfo) -> do
        catalogInfo <-
          (| onNothingA (throwA -<
               err400 NotExists $ "no such table/view exists in postgres: " <>> name)
          |) maybeInfo

        let CatalogTableInfo columns constraints primaryKeyColumnNames viewInfo maybeDesc = catalogInfo
            primaryKeyColumns = flip filter columns $ \column ->
              prciName column `elem` primaryKeyColumnNames
        maybeEnumValues <- if isEnum
          then bindA -< Just <$> fetchAndValidateEnumValues name primaryKeyColumns columns
          else returnA -< Nothing

        -- validate tableConfig
        -- FIXME
        -- withPathK "configuration" $ validateTableConfig info config
        returnA -< TableCoreInfo
          { _tciName = name
          , _tciSystemDefined = systemDefined
          , _tciFieldInfoMap = mapFromL (fromPGCol . prciName) columns
          , _tciUniqueOrPrimaryKeyConstraints = constraints
          , _tciPrimaryKeyColumns = primaryKeyColumnNames
          , _tciViewInfo = viewInfo
          , _tciEnumValues = maybeEnumValues
          , _tciCustomConfig = config
          , _tciDescription = maybeDesc
          }

    -- validateTableConfig :: TableCoreInfo a -> TableConfig -> m ()
    -- validateTableConfig tableInfo (TableConfig rootFlds colFlds) = do
    --     withPathK "custom_root_fields" $ do
    --       sc <- askSchemaCache
    --       let defRemoteGCtx = scDefaultRemoteGCtx sc
    --       validateCustomRootFlds defRemoteGCtx rootFlds
    --     withPathK "custom_column_names" $
    --       forM_ (M.toList colFlds) $ \(col, customName) -> do
    --         void $ askPGColInfo (_tciFieldInfoMap tableInfo) col ""
    --         withPathK (getPGColTxt col) $
    --           _checkForFieldConflict tableInfo $ FieldName $ G.unName customName
    --         when (not $ null duplicateNames) $ throw400 NotSupported $
    --           "the following names are duplicated: " <> showNames duplicateNames
    --   where
    --     duplicateNames = duplicates $ M.elems colFlds

    -- Step 2: Process the raw table cache to replace Postgres column types with logical column
    -- types.
    processTableInfo
      :: ErrorA QErr arr
       ( M.HashMap QualifiedTable EnumValues
       , TableCoreInfo PGRawColumnInfo
       ) (TableCoreInfo PGColumnInfo)
    processTableInfo = (throwA ||| returnA) <<< arr \(enumTables, rawInfo) -> runExcept do
      let tableName = _tciName rawInfo
          customFields = _tcCustomColumnNames $ _tciCustomConfig rawInfo
          process = processColumnInfo enumTables customFields tableName
      traverseOf (tciFieldInfoMap.traverse) process rawInfo

    -- | “Processes” a 'PGRawColumnInfo' into a 'PGColumnInfo' by resolving its type using a map of
    -- known enum tables.
    processColumnInfo
      :: (QErrM n)
      => M.HashMap QualifiedTable EnumValues -- ^ known enum tables
      -> CustomColumnNames -- ^ customised graphql names
      -> QualifiedTable -- ^ the table this column belongs to
      -> PGRawColumnInfo -- ^ the column’s raw information
      -> n PGColumnInfo
    processColumnInfo enumTables customFields tableName rawInfo = do
      resolvedType <- resolveColumnType
      pure PGColumnInfo
        { pgiColumn = pgCol
        , pgiName = graphqlName
        , pgiType = resolvedType
        , pgiIsNullable = prciIsNullable rawInfo
        , pgiDescription = prciDescription rawInfo
        }
      where
        pgCol = prciName rawInfo
        graphqlName = fromMaybe (G.Name $ getPGColTxt pgCol) $
                      M.lookup pgCol customFields
        resolveColumnType =
          case prciReferences rawInfo of
            -- no referenced tables? definitely not an enum
            [] -> pure $ PGColumnScalar (prciType rawInfo)

            -- one referenced table? might be an enum, so check if the referenced table is an enum
            [referencedTableName] -> pure $ M.lookup referencedTableName enumTables & maybe
              (PGColumnScalar $ prciType rawInfo)
              (PGColumnEnumReference . EnumReference referencedTableName)

            -- multiple referenced tables? we could check if any of them are enums, but the schema
            -- is strange, so let’s just reject it
            referencedTables -> throw400 ConstraintViolation
              $ "cannot handle exotic schema: column " <> prciName rawInfo <<> " in table "
              <> tableName <<> " references multiple foreign tables ("
              <> T.intercalate ", " (map dquote referencedTables) <> ")?"
