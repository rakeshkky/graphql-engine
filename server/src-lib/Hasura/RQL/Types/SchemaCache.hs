-- As of GHC 8.6, a use of DefaultSignatures in this module triggers a false positive for this
-- warning, so don’t treat it as an error even if -Werror is enabled.
{-# OPTIONS_GHC -Wwarn=redundant-constraints #-}

{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.SchemaCache
-- <<<<<<< HEAD
--        ( SchemaCache(..)
--        , SchemaCacheVer
--        , initSchemaCacheVer
--        , incSchemaCacheVer
--        , emptySchemaCache
--        , TableConfig(..)
--        , emptyTableConfig

--        , TableCache
--        , modTableCache
--        , addTableToCache
--        , modTableInCache
--        , delTableFromCache

--        , TableInfo(..)
--        , askTabInfoM
--        , tiName
--        , tiDescription
--        , tiSystemDefined
--        , tiFieldInfoMap
--        , tiRolePermInfoMap
--        , tiUniqOrPrimConstraints
--        , tiPrimaryKeyCols
--        , tiViewInfo
--        , tiEventTriggerInfoMap
--        , tiEnumValues
--        , tiCustomConfig

--        , TableConstraint(..)
--        , ConstraintType(..)
--        , ViewInfo(..)
--        , checkForFieldConflict
--        , isMutable
--        , mutableView
--        , isUniqueOrPrimary
--        , isForeignKey

--        , RemoteSchemaCtx(..)
--        , RemoteSchemaMap
--        , addRemoteSchemaToCache
--        , delRemoteSchemaFromCache

--        , WithDeps

--        , CacheRM(..)
--        , CacheRWM(..)

--        , FieldInfoMap
--        , FieldInfo(..)
--        , _FIColumn
--        , _FIRelationship
--        , getPGColumnInfoM
--        , getCols
--        , getRels
--        , getComputedFieldInfos
--        , possibleNonColumnGraphQLFields

--        , isPGColInfo
--        , RelInfo(..)
--        , addColToCache
--        , addRelToCache
--        , addComputedFieldToCache

--        , delColFromCache
--        , updColInCache
--        , delRelFromCache
--        , deleteComputedFieldFromCache

--        , RolePermInfo(..)
--        , permIns
--        , permSel
--        , permUpd
--        , permDel
--        , PermAccessor(..)
--        , permAccToLens
--        , permAccToType
--        , withPermType
--        , RolePermInfoMap

--        , InsPermInfo(..)
--        , SelPermInfo(..)
--        , getSelectPermissionInfoM
--        , UpdPermInfo(..)
--        , DelPermInfo(..)
--        , addPermToCache
--        , delPermFromCache
--        , PreSetColsPartial

--        , setCustomTypesInCache

--        , addEventTriggerToCache
--        , delEventTriggerFromCache
--        , EventTriggerInfo(..)
--        , EventTriggerInfoMap

--        , TableObjId(..)
--        , SchemaObjId(..)
--        , reportSchemaObj
--        , reportSchemaObjs
--        , DependencyReason(..)
--        , SchemaDependency(..)
--        , mkParentDep
--        , mkColDep
--        , mkComputedFieldDep
--        , getDependentObjs
--        , getDependentObjsWith

--        , FunctionType(..)
--        , FunctionArg(..)
--        , FunctionArgName(..)
--        , FunctionName(..)
--        , FunctionInfo(..)
--        , FunctionCache
--        , getFuncsOfTable
--        , addFunctionToCache
--        , askFunctionInfo
--        , delFunctionFromCache
--        , updateFunctionDescription

--        , replaceAllowlist
--        , ActionCache

--        , addActionToCache
--        , delActionFromCache
--        , addActionPermissionToCache
--        , delActionPermissionFromCache
--        ) where
-- =======
  ( SchemaCache(..)
  , SchemaCacheVer
  , initSchemaCacheVer
  , incSchemaCacheVer
  , TableConfig(..)
  , emptyTableConfig

  , TableCoreCache
  , TableCache

  , TableCoreInfoG(..)
  , TableRawInfo
  , TableCoreInfo
  , askTabInfoM
  , tciName
  , tciDescription
  , tciSystemDefined
  , tciFieldInfoMap
  , tciPrimaryKey
  , tciUniqueConstraints
  , tciForeignKeys
  , tciViewInfo
  , tciEnumValues
  , tciCustomConfig
  , tciUniqueOrPrimaryKeyConstraints

  , OutputFieldTypeInfo(..)
  , AnnotatedObjectType(..)
  , AnnotatedObjects
  , ObjectRelationship(..)
  , orName, orRemoteTable, orFieldMapping
  , NonObjectTypeMap(..)

  , TableInfo(..)
  , tiCoreInfo
  , tiRolePermInfoMap
  , tiEventTriggerInfoMap

  , ViewInfo(..)
  , checkForFieldConflict
  , isMutable
  , mutableView

  , RemoteSchemaCtx(..)
  , RemoteSchemaMap

  , DepMap
  , WithDeps

  , TableCoreInfoRM(..)
  , TableCoreCacheRT(..)
  , CacheRM(..)
  , CacheRT(..)

  , FieldInfoMap
  , FieldInfo(..)
  , _FIColumn
  , _FIRelationship
  , _FIComputedField
  , getPGColumnInfoM
  , fieldInfoName
  , fieldInfoGraphQLNames
  , getCols
  , getRels
  , getComputedFieldInfos

  , isPGColInfo
  , RelInfo(..)

  , RolePermInfo(..)
  , mkRolePermInfo
  -- , emptyRolePermInfo
  , permIns
  , permSel
  , permUpd
  , permDel
  , PermAccessor(..)
  , permAccToLens
  , permAccToType
  , withPermType
  , RolePermInfoMap

  , InsPermInfo(..)
  , SelPermInfo(..)
  , getSelectPermissionInfoM
  , UpdPermInfo(..)
  , DelPermInfo(..)
  , PreSetColsPartial

  , EventTriggerInfo(..)
  , EventTriggerInfoMap

  , TableObjId(..)
  , SchemaObjId(..)
  , reportSchemaObj
  , reportSchemaObjs
  , DependencyReason(..)
  , SchemaDependency(..)
  , mkParentDep
  , mkColDep
  , mkComputedFieldDep
  , getDependentObjs
  , getDependentObjsWith

  , FunctionType(..)
  , FunctionArg(..)
  , FunctionArgName(..)
  , FunctionName(..)
  , FunctionInfo(..)
  , FunctionCache
  , getFuncsOfTable
  , askFunctionInfo

  , ActionCache
  ) where
-- >>>>>>> 3354-faster-metadata-migrations

import qualified Hasura.GraphQL.Context            as GC

import           Hasura.Db
import           Hasura.Incremental                (Dependency, MonadDepend (..),
                                                    selectKeyD)
import           Hasura.Prelude
import           Hasura.RQL.Types.Action
import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.ComputedField
import           Hasura.RQL.Types.CustomTypes
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.Function
import           Hasura.RQL.Types.Metadata
import           Hasura.RQL.Types.QueryCollection
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.RQL.Types.SchemaCacheTypes
import           Hasura.RQL.Types.Table
import           Hasura.SQL.Types

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH

import qualified Data.HashMap.Strict               as M
import qualified Data.HashSet                      as HS
import qualified Data.Text                         as T

reportSchemaObjs :: [SchemaObjId] -> T.Text
reportSchemaObjs = T.intercalate ", " . sort . map reportSchemaObj

mkParentDep :: QualifiedTable -> SchemaDependency
mkParentDep tn = SchemaDependency (SOTable tn) DRTable

mkColDep :: DependencyReason -> QualifiedTable -> PGCol -> SchemaDependency
mkColDep reason tn col =
  flip SchemaDependency reason . SOTableObj tn $ TOCol col

mkComputedFieldDep
  :: DependencyReason -> QualifiedTable -> ComputedFieldName -> SchemaDependency
mkComputedFieldDep reason tn computedField =
  flip SchemaDependency reason . SOTableObj tn $ TOComputedField computedField

type WithDeps a = (a, [SchemaDependency])

-- <<<<<<< HEAD
-- data ConstraintType
--   = CTCHECK
--   | CTFOREIGNKEY
--   | CTPRIMARYKEY
--   | CTUNIQUE
--   deriving Eq

-- constraintTyToTxt :: ConstraintType -> T.Text
-- constraintTyToTxt ty = case ty of
--   CTCHECK      -> "CHECK"
--   CTFOREIGNKEY -> "FOREIGN KEY"
--   CTPRIMARYKEY -> "PRIMARY KEY"
--   CTUNIQUE     -> "UNIQUE"

-- instance Show ConstraintType where
--   show = T.unpack . constraintTyToTxt

-- instance FromJSON ConstraintType where
--   parseJSON = withText "ConstraintType" $ \case
--     "CHECK"       -> return CTCHECK
--     "FOREIGN KEY" -> return CTFOREIGNKEY
--     "PRIMARY KEY" -> return CTPRIMARYKEY
--     "UNIQUE"      -> return CTUNIQUE
--     c             -> fail $ "unexpected ConstraintType: " <> T.unpack c

-- instance ToJSON ConstraintType where
--   toJSON = String . constraintTyToTxt

-- isUniqueOrPrimary :: ConstraintType -> Bool
-- isUniqueOrPrimary = \case
--   CTPRIMARYKEY -> True
--   CTUNIQUE     -> True
--   _            -> False

-- isForeignKey :: ConstraintType -> Bool
-- isForeignKey = \case
--   CTFOREIGNKEY -> True
--   _            -> False

-- data TableConstraint
--   = TableConstraint
--   { tcType :: !ConstraintType
--   , tcName :: !ConstraintName
--   } deriving (Show, Eq)

-- $(deriveJSON (aesonDrop 2 snakeCase) ''TableConstraint)
-- =======

checkForFieldConflict
  :: (MonadError QErr m)
  => TableCoreInfoG a b
  -> FieldName
  -> m ()
checkForFieldConflict tableInfo f =
  case M.lookup f (_tciFieldInfoMap tableInfo) of
    Just _ -> throw400 AlreadyExists $ mconcat
      [ "column/relationship/computed field " <>> f
      , " of table " <>> _tciName tableInfo
      , " already exists"
      ]
    Nothing -> return ()

type FunctionCache = M.HashMap QualifiedFunction FunctionInfo -- info of all functions

data RemoteSchemaCtx
  = RemoteSchemaCtx
  { rscName :: !RemoteSchemaName
  , rscGCtx :: !GC.RemoteGCtx
  , rscInfo :: !RemoteSchemaInfo
  } deriving (Show, Eq)

instance ToJSON RemoteSchemaCtx where
  toJSON = toJSON . rscInfo

type RemoteSchemaMap = M.HashMap RemoteSchemaName RemoteSchemaCtx

type DepMap = M.HashMap SchemaObjId (HS.HashSet SchemaDependency)

newtype SchemaCacheVer
  = SchemaCacheVer { unSchemaCacheVer :: Word64 }
  deriving (Show, Eq, Ord, Hashable, ToJSON, FromJSON)

initSchemaCacheVer :: SchemaCacheVer
initSchemaCacheVer = SchemaCacheVer 0

incSchemaCacheVer :: SchemaCacheVer -> SchemaCacheVer
incSchemaCacheVer (SchemaCacheVer prev) =
  SchemaCacheVer $ prev + 1

-- data CustomTypesState
--   = CustomTypeState
--   { _ctsTypes         :: !RT.TypeMap
--   , _ctsRelationships :: !(M.HashMap G.NamedType )
--   }

type ActionCache =
  M.HashMap ActionName ActionInfo

data SchemaCache
  = SchemaCache
  { scTables            :: !TableCache
  , scActions           :: !ActionCache
  , scFunctions         :: !FunctionCache
  , scRemoteSchemas     :: !RemoteSchemaMap
  , scAllowlist         :: !(HS.HashSet GQLQuery)
  , scCustomTypes       :: !(NonObjectTypeMap, AnnotatedObjects)
  , scGCtxMap           :: !GC.GCtxMap
  , scDefaultRemoteGCtx :: !GC.GCtx
  , scDepMap            :: !DepMap
  , scInconsistentObjs  :: ![InconsistentMetadata]
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 2 snakeCase) ''SchemaCache)

-- <<<<<<< HEAD
-- class (Monad m) => CacheRM m where
--   askSchemaCache :: m SchemaCache

-- -- instance (CacheRM m) => CacheRM (ReaderT r m) where
-- --   askSchemaCache = lift askSchemaCache

-- instance (Monad m) => CacheRM (StateT SchemaCache m) where
--   askSchemaCache = get

-- instance (Monad m) => CacheRM (ReaderT SchemaCache m) where
--   askSchemaCache = ask

askTabInfoM
  :: (CacheRM m)
  => QualifiedTable -> m (Maybe TableInfo)
askTabInfoM tabName = do
  rawSchemaCache <- askSchemaCache
  return $ M.lookup tabName $ scTables rawSchemaCache

-- class (CacheRM m) => CacheRWM m where
--   writeSchemaCache :: SchemaCache -> m ()

-- -- instance (CacheRWM m) => CacheRWM (ReaderT r m) where
-- --   writeSchemaCache = lift . writeSchemaCache

-- instance (Monad m) => CacheRWM (StateT SchemaCache m) where
--   writeSchemaCache = put

-- getFuncsOfTable :: QualifiedTable -> FunctionCache -> [FunctionInfo]
-- getFuncsOfTable qt fc =
--   flip filter allFuncs $ \f -> qt == fiReturnType f
--   where
--     allFuncs = M.elems fc

-- modDepMapInCache :: (CacheRWM m) => (DepMap -> DepMap) -> m ()
-- modDepMapInCache f = do
--   sc <- askSchemaCache
--   writeSchemaCache $ sc { scDepMap = f (scDepMap sc)}

-- emptySchemaCache :: SchemaCache
-- emptySchemaCache =
--   SchemaCache
--   { scTables = M.empty
--   , scActions           = mempty
--   , scFunctions         = mempty
--   , scRemoteSchemas     = mempty
--   , scAllowlist         = mempty
--   , scCustomTypes       = mempty
--   , scGCtxMap           = mempty
--   , scDefaultRemoteGCtx = GC.emptyGCtx
--   , scDepMap            = mempty
--   , scInconsistentObjs  = mempty
--   }

-- modTableCache :: (CacheRWM m) => TableCache PGColumnInfo -> m ()
-- modTableCache tc = do
--   sc <- askSchemaCache
--   writeSchemaCache $ sc { scTables = tc }

-- addTableToCache :: (QErrM m, CacheRWM m)
--                 => TableInfo PGColumnInfo -> m ()
-- addTableToCache ti = do
--   sc <- askSchemaCache
--   assertTableNotExists sc
--   modTableCache $ M.insert tn ti $ scTables sc
--   where
--     tn = _tiName ti
--     assertTableNotExists :: (QErrM m) => SchemaCache -> m ()
--     assertTableNotExists sc =
--       case M.lookup tn (scTables sc) of
--         Nothing -> return ()
--         Just _  -> throw500 $ "table exists in cache : " <>> tn

-- getTableInfoFromCache :: (QErrM m)
--                       => QualifiedTable
--                       -> SchemaCache
--                       -> m (TableInfo PGColumnInfo)
-- getTableInfoFromCache tn sc =
--   case M.lookup tn (scTables sc) of
--     Nothing -> throw500 $ "table not found in cache : " <>> tn
--     Just ti -> return ti

-- delTableFromCache :: (QErrM m, CacheRWM m)
--                   => QualifiedTable -> m ()
-- delTableFromCache tn = do
--   sc <- askSchemaCache
--   void $ getTableInfoFromCache tn sc
--   modTableCache $ M.delete tn $ scTables sc
--   modDepMapInCache (M.filterWithKey notThisTableObj)
--   where
--     notThisTableObj (SOTableObj depTn _) _ = depTn /= tn
--     notThisTableObj _                    _ = True

-- modTableInCache :: (QErrM m, CacheRWM m)
--                 => (TableInfo PGColumnInfo -> m (TableInfo PGColumnInfo))
--                 -> QualifiedTable
--                 -> m ()
-- modTableInCache f tn = do
--   sc <- askSchemaCache
--   ti <- getTableInfoFromCache tn sc
--   newTi <- f ti
--   modTableCache $ M.insert tn newTi $ scTables sc

-- addColToCache
--   :: (QErrM m, CacheRWM m)
--   => PGCol -> PGColumnInfo
--   -> QualifiedTable -> m ()
-- addColToCache cn ci =
--   addFldToCache (fromPGCol cn) (FIColumn ci)

-- addRelToCache
--   :: (QErrM m, CacheRWM m)
--   => RelName -> RelInfo -> [SchemaDependency]
--   -> QualifiedTable -> m ()
-- addRelToCache rn ri deps tn = do
--   addFldToCache (fromRel rn) (FIRelationship ri)  tn
--   modDepMapInCache (addToDepMap schObjId deps)
--   where
--     schObjId = SOTableObj tn $ TORel $ riName ri

-- addComputedFieldToCache
--   :: (QErrM m, CacheRWM m)
--   => QualifiedTable -> ComputedFieldInfo -> m ()
-- addComputedFieldToCache table computedFieldInfo =
--   addFldToCache computedField (FIComputedField computedFieldInfo) table
--   where
--     computedField = fromComputedField $ _cfiName computedFieldInfo

-- addFldToCache
--   :: (QErrM m, CacheRWM m)
--   => FieldName -> FieldInfo PGColumnInfo
--   -> QualifiedTable -> m ()
-- addFldToCache fn fi =
--   modTableInCache modFieldInfoMap
--   where
--     modFieldInfoMap ti = do
--       let fim = _tiFieldInfoMap ti
--       case M.lookup fn fim of
--         Just _  -> throw500 "field already exists "
--         Nothing -> return $
--           ti { _tiFieldInfoMap = M.insert fn fi fim }

-- delFldFromCache :: (QErrM m, CacheRWM m)
--                 => FieldName -> QualifiedTable -> m ()
-- delFldFromCache fn =
--   modTableInCache modFieldInfoMap
--   where
--     modFieldInfoMap ti = do
--       let fim = _tiFieldInfoMap ti
--       case M.lookup fn fim of
--         Just _  -> return $
--           ti { _tiFieldInfoMap = M.delete fn fim }
--         Nothing -> throw500 "field does not exist"

-- setCustomTypesInCache
--   :: (QErrM m, CacheRWM m)
--   => (NonObjectTypeMap, AnnotatedObjects)
--   -> m ()
-- setCustomTypesInCache customTypes = do
--   sc <- askSchemaCache
--   writeSchemaCache sc {scCustomTypes = customTypes}

-- delColFromCache :: (QErrM m, CacheRWM m)
--                 => PGCol -> QualifiedTable -> m ()
-- delColFromCache cn =
--   delFldFromCache (fromPGCol cn)

-- delRelFromCache :: (QErrM m, CacheRWM m)
--                 => RelName -> QualifiedTable -> m ()
-- delRelFromCache rn tn = do
--   delFldFromCache (fromRel rn) tn
--   modDepMapInCache (removeFromDepMap schObjId)
--   where
--     schObjId = SOTableObj tn $ TORel rn

-- deleteComputedFieldFromCache
--   :: (QErrM m, CacheRWM m)
--   => QualifiedTable -> ComputedFieldName -> m ()
-- deleteComputedFieldFromCache table computedField =
--   delFldFromCache (fromComputedField computedField) table

-- updColInCache
--   :: (QErrM m, CacheRWM m)
--   => PGCol -> PGColumnInfo
--   -> QualifiedTable -> m ()
-- updColInCache cn ci tn = do
--   delColFromCache cn tn
--   addColToCache cn ci tn
-- =======
getFuncsOfTable :: QualifiedTable -> FunctionCache -> [FunctionInfo]
getFuncsOfTable qt fc = flip filter allFuncs $ \f -> qt == fiReturnType f
  where
    allFuncs = M.elems fc

-- | A more limited version of 'CacheRM' that is used when building the schema cache, since the
-- entire schema cache has not been built yet.
class (Monad m) => TableCoreInfoRM m where
  lookupTableCoreInfo :: QualifiedTable -> m (Maybe TableCoreInfo)
  default lookupTableCoreInfo :: (CacheRM m) => QualifiedTable -> m (Maybe TableCoreInfo)
  lookupTableCoreInfo tableName = fmap _tiCoreInfo . M.lookup tableName . scTables <$> askSchemaCache

instance (TableCoreInfoRM m) => TableCoreInfoRM (ReaderT r m) where
  lookupTableCoreInfo = lift . lookupTableCoreInfo
instance (TableCoreInfoRM m) => TableCoreInfoRM (StateT s m) where
  lookupTableCoreInfo = lift . lookupTableCoreInfo
instance (Monoid w, TableCoreInfoRM m) => TableCoreInfoRM (WriterT w m) where
  lookupTableCoreInfo = lift . lookupTableCoreInfo

newtype TableCoreCacheRT m a
  = TableCoreCacheRT { runTableCoreCacheRT :: Dependency TableCoreCache -> m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError e, MonadState s, MonadWriter w, MonadTx)
    via (ReaderT (Dependency TableCoreCache) m)
  deriving (MonadTrans) via (ReaderT (Dependency TableCoreCache))

instance (MonadReader r m) => MonadReader r (TableCoreCacheRT m) where
  ask = lift ask
  local f m = TableCoreCacheRT (local f . runTableCoreCacheRT m)
instance (MonadDepend m) => TableCoreInfoRM (TableCoreCacheRT m) where
  lookupTableCoreInfo tableName = TableCoreCacheRT (dependOnM . selectKeyD tableName)

class (TableCoreInfoRM m) => CacheRM m where
  askSchemaCache :: m SchemaCache

instance (CacheRM m) => CacheRM (ReaderT r m) where
  askSchemaCache = lift askSchemaCache
instance (CacheRM m) => CacheRM (StateT s m) where
  askSchemaCache = lift askSchemaCache
instance (Monoid w, CacheRM m) => CacheRM (WriterT w m) where
  askSchemaCache = lift askSchemaCache

newtype CacheRT m a = CacheRT { runCacheRT :: SchemaCache -> m a }
  deriving (Functor, Applicative, Monad, MonadError e, MonadWriter w) via (ReaderT SchemaCache m)
  deriving (MonadTrans) via (ReaderT SchemaCache)
instance (Monad m) => TableCoreInfoRM (CacheRT m)
instance (Monad m) => CacheRM (CacheRT m) where
  askSchemaCache = CacheRT pure
-- >>>>>>> 3354-faster-metadata-migrations

-- addPermToCache
--   :: (QErrM m, CacheRWM m)
--   => QualifiedTable
--   -> RoleName
--   -> PermAccessor a
--   -> a
--   -> [SchemaDependency]
--   -> m ()
-- addPermToCache tn rn pa i deps = do
--   modTableInCache modRolePermInfo tn
--   modDepMapInCache (addToDepMap schObjId deps)
--   where
--     permLens = permAccToLens pa
--     modRolePermInfo ti = do
--       let rpim = _tiRolePermInfoMap ti
--           rpi  = fromMaybe mkRolePermInfo $ M.lookup rn rpim
--           newRPI = rpi & permLens ?~ i
--       assertPermNotExists pa rpi
--       return $ ti { _tiRolePermInfoMap = M.insert rn newRPI rpim }
--     schObjId = SOTableObj tn $ TOPerm rn $ permAccToType pa

--     assertPermNotExists :: (QErrM m) => PermAccessor a -> RolePermInfo -> m ()
--     assertPermNotExists f rpi =
--       when (isJust $ rpi ^. permAccToLens f) $ throw500 "permission exists"

-- delPermFromCache
--   :: (QErrM m, CacheRWM m)
--   => PermAccessor a
--   -> RoleName
--   -> QualifiedTable
--   -> m ()
-- delPermFromCache pa rn tn = do
--   modTableInCache modRolePermInfo tn
--   modDepMapInCache (removeFromDepMap schObjId)
--   where
--     permLens = permAccToLens pa
--     modRolePermInfo ti = do
--       let rpim = _tiRolePermInfoMap ti
--           rpi  = fromMaybe mkRolePermInfo $ M.lookup rn rpim
--       assertPermExists pa rpi
--       let newRPI = rpi & permLens .~ Nothing
--       return $ ti { _tiRolePermInfoMap = M.insert rn newRPI rpim }
--     schObjId = SOTableObj tn $ TOPerm rn $ permAccToType pa

--     assertPermExists :: (QErrM m) => PermAccessor a -> RolePermInfo -> m ()
--     assertPermExists f rpi =
--       unless (isJust $ rpi ^. permAccToLens f) $ throw500 "permission does not exist"

-- <<<<<<< HEAD
-- addEventTriggerToCache
--   :: (QErrM m, CacheRWM m)
--   => QualifiedTable
--   -> EventTriggerInfo
--   -> [SchemaDependency]
--   -> m ()
-- addEventTriggerToCache qt eti deps = do
--   modTableInCache modEventTriggerInfo qt
--   modDepMapInCache (addToDepMap schObjId deps)
--   where
--     trn = etiName eti
--     modEventTriggerInfo ti = do
--       let etim = _tiEventTriggerInfoMap ti
--       return $ ti { _tiEventTriggerInfoMap = M.insert trn eti etim}
--     schObjId = SOTableObj qt $ TOTrigger trn

-- delEventTriggerFromCache
--   :: (QErrM m, CacheRWM m)
--   => QualifiedTable
--   -> TriggerName
--   -> m ()
-- delEventTriggerFromCache qt trn = do
--   modTableInCache modEventTriggerInfo qt
--   modDepMapInCache (removeFromDepMap schObjId)
--   where
--     modEventTriggerInfo ti = do
--       let etim = _tiEventTriggerInfoMap ti
--       return $ ti { _tiEventTriggerInfoMap = M.delete trn etim }
--     schObjId = SOTableObj qt $ TOTrigger trn

-- modifyActionCache :: (CacheRWM m) => (ActionCache -> ActionCache) -> m ()
-- modifyActionCache f = do
--   schemaCache <- askSchemaCache
--   writeSchemaCache $ schemaCache { scActions = f $ scActions schemaCache }

-- addActionToCache
--   :: (QErrM m, CacheRWM m) => ActionInfo -> m ()
-- addActionToCache actionInfo = do
--   assertActionNotExists
--   modifyActionCache (M.insert actionName actionInfo)
--   where
--     actionName = _aiName actionInfo
--     assertActionNotExists :: (CacheRM m, QErrM m) => m ()
--     assertActionNotExists = do
--       schemaCache <- askSchemaCache
--       case M.lookup actionName (scActions schemaCache) of
--         Nothing -> return ()
--         Just _  -> throw500 $ "action already exists in cache: " <>> actionName

-- getActionInfoFromCache
--   :: (QErrM m) => ActionName -> SchemaCache -> m ActionInfo
-- getActionInfoFromCache actionName schemaCache =
--   case M.lookup actionName (scActions schemaCache) of
--     Nothing -> throw500 $ "action not found in cache: " <>> actionName
--     Just ti -> return ti

-- delActionFromCache
--   :: (QErrM m, CacheRWM m) => ActionName -> m ()
-- delActionFromCache actionName = do
--   schemaCache <- askSchemaCache
--   void $ getActionInfoFromCache actionName schemaCache
--   modifyActionCache (M.delete actionName)

-- modifyActionInfoInCache
--   :: (QErrM m, CacheRWM m) => ActionName -> (ActionInfo -> m ActionInfo) -> m ()
-- modifyActionInfoInCache actionName f = do
--   schemaCache <- askSchemaCache
--   actionInfo  <- getActionInfoFromCache actionName schemaCache
--   newActionInfo <- f actionInfo
--   modifyActionCache (M.insert actionName newActionInfo)

-- -- TODO: use lens
-- addActionPermissionToCache
--   :: (QErrM m, CacheRWM m) => ActionName -> ActionPermissionInfo -> m ()
-- addActionPermissionToCache actionName permissionInfo =
--   modifyActionInfoInCache actionName $ \actionInfo -> do
--     let currentPermissions = _aiPermissions actionInfo
--     case M.lookup role currentPermissions of
--       Just _  -> throw500 $ "action permission already exists in cache: " <>
--                  actionName <<> ", " <>> role
--       Nothing ->
--         return $ actionInfo
--         { _aiPermissions = M.insert role permissionInfo currentPermissions }
--   where
--     role = _apiRole permissionInfo

-- delActionPermissionFromCache
--   :: (QErrM m, CacheRWM m) => ActionName -> RoleName -> m ()
-- delActionPermissionFromCache actionName role =
--   modifyActionInfoInCache actionName $ \actionInfo -> do
--     let currentPermissions = _aiPermissions actionInfo
--     case M.lookup role currentPermissions of
--       Just _  ->
--         return $ actionInfo
--         { _aiPermissions = M.delete role currentPermissions }
--       Nothing -> throw500 $ "action permission does not exist in cache: " <>
--                  actionName <<> ", " <>> role

-- addFunctionToCache
--   :: (QErrM m, CacheRWM m)
--   => FunctionInfo -> [SchemaDependency] -> m ()
-- addFunctionToCache fi deps = do
--   sc <- askSchemaCache
--   let functionCache = scFunctions sc
--   case M.lookup fn functionCache of
--     Just _ -> throw500 $ "function already exists in cache " <>> fn
--     Nothing -> do
--       let newFunctionCache = M.insert fn fi functionCache
--       writeSchemaCache $ sc {scFunctions = newFunctionCache}
--   modDepMapInCache (addToDepMap objId deps)
--   where
--     fn = fiName fi
--     objId = SOFunction $ fiName fi

-- =======
-- >>>>>>> 3354-faster-metadata-migrations
askFunctionInfo
  :: (CacheRM m, QErrM m)
  => QualifiedFunction ->  m FunctionInfo
askFunctionInfo qf = do
  sc <- askSchemaCache
  maybe throwNoFn return $ M.lookup qf $ scFunctions sc
  where
    throwNoFn = throw400 NotExists $
      "function not found in cache " <>> qf

-- <<<<<<< HEAD
-- delFunctionFromCache
--   :: (QErrM m, CacheRWM m)
--   => QualifiedFunction -> m ()
-- delFunctionFromCache qf = do
--   void $ askFunctionInfo qf
--   sc <- askSchemaCache
--   let functionCache = scFunctions sc
--       newFunctionCache = M.delete qf functionCache
--   writeSchemaCache $ sc {scFunctions = newFunctionCache}
--   modDepMapInCache (removeFromDepMap objId)
--   where
--     objId = SOFunction qf

-- updateFunctionDescription
--   :: (QErrM m, CacheRWM m)
--   => QualifiedFunction -> Maybe PGDescription -> m ()
-- updateFunctionDescription qf descM = do
--   fi <- askFunctionInfo qf
--   sc <- askSchemaCache
--   let newFuncInfo = fi{fiDescription = descM}
--       newFuncCache = M.insert qf newFuncInfo $ scFunctions sc
--   writeSchemaCache sc{scFunctions = newFuncCache}

-- addRemoteSchemaToCache
--   :: (QErrM m, CacheRWM m) => RemoteSchemaCtx -> m ()
-- addRemoteSchemaToCache rmCtx = do
--   sc <- askSchemaCache
--   let rmSchemas = scRemoteSchemas sc
--       name = rscName rmCtx
--   -- ideally, remote schema shouldn't present in cache
--   -- if present unexpected 500 is thrown
--   onJust (M.lookup name rmSchemas) $ const $
--     throw500 $ "remote schema with name " <> name
--     <<> " already found in cache"
--   writeSchemaCache sc
--     {scRemoteSchemas = M.insert name rmCtx rmSchemas}

-- delRemoteSchemaFromCache
--   :: (QErrM m, CacheRWM m) => RemoteSchemaName -> m ()
-- delRemoteSchemaFromCache name = do
--   sc <- askSchemaCache
--   let rmSchemas = scRemoteSchemas sc
--   -- ideally, remote schema should be present in cache
--   -- if not present unexpected 500 is thrown
--   void $ onNothing (M.lookup name rmSchemas) $
--     throw500 $ "remote schema with name " <> name
--     <<> " not found in cache"
--   writeSchemaCache sc {scRemoteSchemas = M.delete name rmSchemas}

-- replaceAllowlist
--   :: (CacheRWM m)
--   => QueryList -> m ()
-- replaceAllowlist qList = do
--   sc <- askSchemaCache
--   let allowlist = HS.fromList $
--         map (queryWithoutTypeNames . getGQLQuery . _lqQuery) qList
--   writeSchemaCache sc{scAllowlist = allowlist}

-- =======
-- >>>>>>> 3354-faster-metadata-migrations
getDependentObjs :: SchemaCache -> SchemaObjId -> [SchemaObjId]
getDependentObjs = getDependentObjsWith (const True)

getDependentObjsWith
  :: (DependencyReason -> Bool) -> SchemaCache -> SchemaObjId -> [SchemaObjId]
getDependentObjsWith f sc objId =
  -- [ sdObjId sd | sd <- filter (f . sdReason) allDeps]
  map fst $ filter (isDependency . snd) $ M.toList $ scDepMap sc
  where
    isDependency deps = not $ HS.null $ flip HS.filter deps $
      \(SchemaDependency depId reason) -> objId `induces` depId && f reason
    -- induces a b : is b dependent on a
    induces (SOTable tn1) (SOTable tn2)      = tn1 == tn2
    induces (SOTable tn1) (SOTableObj tn2 _) = tn1 == tn2
    induces objId1 objId2                    = objId1 == objId2
    -- allDeps = toList $ fromMaybe HS.empty $ M.lookup objId $ scDepMap sc
