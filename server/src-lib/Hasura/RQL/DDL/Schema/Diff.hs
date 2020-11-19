module Hasura.RQL.DDL.Schema.Diff
  ( TableMeta(..)
  , ComputedFieldMeta(..)

  , fetchMeta

  , getDifference

  , TableDiff(..)
  , getTableDiff
  , getTableChangeDeps
  , ComputedFieldDiff(..)

  , SchemaDiff(..)
  , getSchemaDiff
  , getSchemaChangeDeps

  , FunctionMeta(..)
  , FunctionDiff(..)
  , getFuncDiff
  , getOverloadedFuncs
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                as M
import qualified Data.HashSet                       as HS
import qualified Data.List.NonEmpty                 as NE

import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.List.Extended                 (duplicates)

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.RQL.DDL.Schema.Common
import           Hasura.RQL.Types                   hiding (ConstraintName, fmFunction,
                                                     tmComputedFields, tmTable)

data FunctionMeta
  = FunctionMeta
  { fmOid      :: !OID
  , fmFunction :: !QualifiedFunction
  , fmType     :: !FunctionVolatility
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 2 snakeCase) ''FunctionMeta)

data ComputedFieldMeta
  = ComputedFieldMeta
  { ccmName         :: !ComputedFieldName
  , ccmFunctionMeta :: !FunctionMeta
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''ComputedFieldMeta)

data TableMeta
  = TableMeta
  { tmTable          :: !QualifiedTable
  , tmInfo           :: !TableMetadataInfo
  , tmComputedFields :: ![ComputedFieldMeta]
  } deriving (Show, Eq)

fetchMeta
  :: (MonadTx m)
  => TableCache
  -> FunctionCache
  -> m ([TableMeta], [FunctionMeta])
fetchMeta tables functions = do
  tableMetaInfos <- fetchTableMetadata
  functionMetaInfos <- fetchFunctionMetadata

  let getFunctionMetas function =
        let mkFunctionMeta rawInfo =
              FunctionMeta (rfiOid rawInfo) function (rfiFunctionType rawInfo)
        in maybe [] (map mkFunctionMeta) $ M.lookup function functionMetaInfos

      mkComputedFieldMeta computedField =
        let function = _cffName $ _cfiFunction computedField
        in map (ComputedFieldMeta (_cfiName computedField)) $ getFunctionMetas function

      tableMetas = flip map (M.toList tableMetaInfos) $ \(table, tableMetaInfo) ->
                   TableMeta table tableMetaInfo $ fromMaybe [] $
                     M.lookup table tables <&> \tableInfo ->
                     let tableCoreInfo  = _tiCoreInfo tableInfo
                         computedFields = getComputedFieldInfos $ _tciFieldInfoMap tableCoreInfo
                     in  concatMap mkComputedFieldMeta computedFields

      functionMetas = concatMap getFunctionMetas $ M.keys functions

  pure (tableMetas, functionMetas)

getOverlap :: (Eq k, Hashable k) => (v -> k) -> [v] -> [v] -> [(v, v)]
getOverlap getKey left right =
  M.elems $ M.intersectionWith (,) (mkMap left) (mkMap right)
  where
    mkMap = M.fromList . map (\v -> (getKey v, v))

getDifference :: (Eq k, Hashable k) => (v -> k) -> [v] -> [v] -> [v]
getDifference getKey left right =
  M.elems $ M.difference (mkMap left) (mkMap right)
  where
    mkMap = M.fromList . map (\v -> (getKey v, v))

data ComputedFieldDiff
  = ComputedFieldDiff
  { _cfdDropped    :: [ComputedFieldName]
  , _cfdAltered    :: [(ComputedFieldMeta, ComputedFieldMeta)]
  , _cfdOverloaded :: [(ComputedFieldName, QualifiedFunction)]
  } deriving (Show, Eq)

data TableDiff (b :: BackendType)
  = TableDiff
  { _tdNewName         :: !(Maybe QualifiedTable)
  , _tdDroppedCols     :: ![Column b]
  , _tdAddedCols       :: ![RawColumnInfo b]
  , _tdAlteredCols     :: ![(RawColumnInfo b, RawColumnInfo b)]
  , _tdDroppedFKeyCons :: ![ConstraintName]
  , _tdComputedFields  :: !ComputedFieldDiff
  -- The final list of uniq/primary constraint names
  -- used for generating types on_conflict clauses
  -- TODO: this ideally should't be part of TableDiff
  , _tdUniqOrPriCons   :: ![ConstraintName]
  , _tdNewDescription  :: !(Maybe PGDescription)
  }

getTableDiff :: TableMeta -> TableMeta -> TableDiff 'Postgres
getTableDiff oldtm newtm =
  TableDiff mNewName droppedCols addedCols alteredCols
  droppedFKeyConstraints computedFieldDiff uniqueOrPrimaryCons mNewDesc
  where
    mNewName = bool (Just $ tmTable newtm) Nothing $ tmTable oldtm == tmTable newtm
    oldCols = _tmiColumns $ tmInfo oldtm
    newCols = _tmiColumns $ tmInfo newtm

    uniqueOrPrimaryCons = map _cName $
      maybeToList (_pkConstraint <$> _tmiPrimaryKey (tmInfo newtm))
        <> toList (_tmiUniqueConstraints $ tmInfo newtm)

    mNewDesc = _tmiDescription $ tmInfo newtm

    droppedCols = map prciName $ getDifference prciPosition oldCols newCols
    addedCols = getDifference prciPosition newCols oldCols
    existingCols = getOverlap prciPosition oldCols newCols
    alteredCols = filter (uncurry (/=)) existingCols

    -- foreign keys are considered dropped only if their oid
    -- and (ref-table, column mapping) are changed
    droppedFKeyConstraints = map (_cName . _fkConstraint) $ HS.toList $
      droppedFKeysWithOid `HS.intersection` droppedFKeysWithUniq
    tmForeignKeys = fmap unForeignKeyMetadata . toList . _tmiForeignKeys . tmInfo
    droppedFKeysWithOid = HS.fromList $
      (getDifference (_cOid . _fkConstraint) `on` tmForeignKeys) oldtm newtm
    droppedFKeysWithUniq = HS.fromList $
      (getDifference mkFKeyUniqId `on` tmForeignKeys) oldtm newtm
    mkFKeyUniqId (ForeignKey _ reftn colMap) = (reftn, colMap)

    -- calculate computed field diff
    oldComputedFieldMeta = tmComputedFields oldtm
    newComputedFieldMeta = tmComputedFields newtm

    droppedComputedFields = map ccmName $
      getDifference (fmOid . ccmFunctionMeta) oldComputedFieldMeta newComputedFieldMeta

    alteredComputedFields =
      getOverlap (fmOid . ccmFunctionMeta) oldComputedFieldMeta newComputedFieldMeta

    overloadedComputedFieldFunctions =
      let getFunction = fmFunction . ccmFunctionMeta
          getSecondElement (_ NE.:| list) = listToMaybe list
      in mapMaybe (fmap ((&&&) ccmName getFunction) . getSecondElement) $
         flip NE.groupBy newComputedFieldMeta $ \l r ->
         ccmName l == ccmName r && getFunction l == getFunction r

    computedFieldDiff = ComputedFieldDiff droppedComputedFields alteredComputedFields
                      overloadedComputedFieldFunctions

getTableChangeDeps
  :: (QErrM m, CacheRM m)
  => QualifiedTable -> TableDiff 'Postgres -> m [SchemaObjId]
getTableChangeDeps tn tableDiff = do
  sc <- askSchemaCache
  -- for all the dropped columns
  droppedColDeps <- fmap concat $ forM droppedCols $ \droppedCol -> do
    let objId = SOTableObj tn $ TOCol droppedCol
    return $ getDependentObjs sc objId
  -- for all dropped constraints
  droppedConsDeps <- fmap concat $ forM droppedFKeyConstraints $ \droppedCons -> do
    let objId = SOTableObj tn $ TOForeignKey droppedCons
    return $ getDependentObjs sc objId
  return $ droppedConsDeps <> droppedColDeps <> droppedComputedFieldDeps
  where
    TableDiff _ droppedCols _ _ droppedFKeyConstraints computedFieldDiff _ _ = tableDiff
    droppedComputedFieldDeps = map (SOTableObj tn . TOComputedField) $ _cfdDropped computedFieldDiff

data SchemaDiff (b :: BackendType)
  = SchemaDiff
  { _sdDroppedTables :: ![QualifiedTable]
  , _sdAlteredTables :: ![(QualifiedTable, TableDiff b)]
  }

getSchemaDiff :: [TableMeta] -> [TableMeta] -> SchemaDiff 'Postgres
getSchemaDiff oldMeta newMeta =
  SchemaDiff droppedTables survivingTables
  where
    droppedTables = map tmTable $ getDifference (_tmiOid . tmInfo) oldMeta newMeta
    survivingTables =
      flip map (getOverlap (_tmiOid . tmInfo) oldMeta newMeta) $ \(oldtm, newtm) ->
      (tmTable oldtm, getTableDiff oldtm newtm)

getSchemaChangeDeps
  :: (QErrM m, CacheRM m)
  => SchemaDiff 'Postgres -> m [SchemaObjId]
getSchemaChangeDeps schemaDiff = do
  -- Get schema cache
  sc <- askSchemaCache
  let tableIds = map SOTable droppedTables
  -- Get the dependent of the dropped tables
  let tableDropDeps = concatMap (getDependentObjs sc) tableIds
  tableModDeps <- concat <$> traverse (uncurry getTableChangeDeps) alteredTables
  return $ filter (not . isDirectDep) $
    HS.toList $ HS.fromList $ tableDropDeps <> tableModDeps
  where
    SchemaDiff droppedTables alteredTables = schemaDiff

    isDirectDep (SOTableObj tn _) = tn `HS.member` HS.fromList droppedTables
    isDirectDep _                 = False

data FunctionDiff
  = FunctionDiff
  { fdDropped :: ![QualifiedFunction]
  , fdAltered :: ![(QualifiedFunction, FunctionVolatility)]
  } deriving (Show, Eq)

getFuncDiff :: [FunctionMeta] -> [FunctionMeta] -> FunctionDiff
getFuncDiff oldMeta newMeta =
  FunctionDiff droppedFuncs alteredFuncs
  where
    droppedFuncs = map fmFunction $ getDifference fmOid oldMeta newMeta
    alteredFuncs = mapMaybe mkAltered $ getOverlap fmOid oldMeta newMeta
    mkAltered (oldfm, newfm) =
      let isTypeAltered = fmType oldfm /= fmType newfm
          alteredFunc = (fmFunction oldfm, fmType newfm)
      in bool Nothing (Just alteredFunc) $ isTypeAltered

getOverloadedFuncs
  :: [QualifiedFunction] -> [FunctionMeta] -> [QualifiedFunction]
getOverloadedFuncs trackedFuncs newFuncMeta =
  toList $ duplicates $ map fmFunction trackedMeta
  where
    trackedMeta = flip filter newFuncMeta $ \fm ->
      fmFunction fm `elem` trackedFuncs
