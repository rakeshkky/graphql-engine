{-# LANGUAGE Arrows #-}

{-| Top-level functions concerned specifically with operations on the schema cache, such as
rebuilding it from the catalog and incorporating schema changes. See the module documentation for
"Hasura.RQL.DDL.Schema" for more details.

__Note__: this module is __mutually recursive__ with other @Hasura.RQL.DDL.Schema.*@ modules, which
both define pieces of the implementation of building the schema cache and define handlers that
trigger schema cache rebuilds. -}
module Hasura.RQL.DDL.Schema.Cache
-- <<<<<<< HEAD
--   ( CacheBuildM
--   , buildSchemaCache
--   , buildGCtxMap
--   , buildSchemaCacheFor
--   , buildSchemaCacheStrict
--   , buildSchemaCacheWithoutSetup
-- =======
  ( RebuildableSchemaCache
  , lastBuiltSchemaCache
  , buildRebuildableSchemaCache
  , CacheRWT
  , runCacheRWT
-- >>>>>>> 3354-faster-metadata-migrations

  , withMetadataCheck
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended             as M
import qualified Data.HashSet                             as HS
import qualified Data.Text                                as T
import qualified Database.PG.Query                        as Q

import           Control.Arrow.Extended
import           Control.Lens                             hiding ((.=))
import           Control.Monad.Unique
import           Data.Aeson
import           Data.List                                (nub)

import qualified Hasura.GraphQL.Context                   as GC
import qualified Hasura.GraphQL.Validate.Types      as VT
import qualified Language.GraphQL.Draft.Syntax      as G
import qualified Hasura.GraphQL.Schema                    as GS
import qualified Hasura.Incremental                       as Inc

import           Hasura.Db
import           Hasura.GraphQL.RemoteServer
import           Hasura.GraphQL.Schema.CustomTypes
import           Hasura.GraphQL.Utils               (showNames)
import           Hasura.RQL.DDL.Action
import           Hasura.RQL.DDL.ComputedField
import           Hasura.RQL.DDL.CustomTypes
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.DDL.EventTrigger
import           Hasura.RQL.DDL.RemoteSchema
import           Hasura.RQL.DDL.Schema.Cache.Common
import           Hasura.RQL.DDL.Schema.Cache.Dependencies
import           Hasura.RQL.DDL.Schema.Cache.Fields
import           Hasura.RQL.DDL.Schema.Cache.Permission
import           Hasura.RQL.DDL.Schema.Catalog
import           Hasura.RQL.DDL.Schema.Diff
import           Hasura.RQL.DDL.Schema.Function
import           Hasura.RQL.DDL.Schema.Table
import           Hasura.RQL.DDL.Utils
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Catalog
import           Hasura.RQL.Types.QueryCollection
import           Hasura.SQL.Types

-- <<<<<<< HEAD
-- type CacheBuildM m
--   = (CacheRWM m, MonadTx m, MonadIO m, HasHttpManager m, HasSQLGenCtx m)

-- buildSchemaCache :: (CacheBuildM m) => m ()
-- buildSchemaCache = buildSchemaCacheWithOptions True

-- buildSchemaCacheWithoutSetup :: (CacheBuildM m) => m ()
-- buildSchemaCacheWithoutSetup = buildSchemaCacheWithOptions False

-- buildSchemaCacheWithOptions :: (CacheBuildM m) => Bool -> m ()
-- buildSchemaCacheWithOptions withSetup = do
--   -- clean hdb_views
--   when withSetup $ liftTx $ Q.catchE defaultTxErrorHandler clearHdbViews
--   -- reset the current schemacache
--   writeSchemaCache emptySchemaCache
--   sqlGenCtx <- askSQLGenCtx

--   -- fetch all catalog metadata
--   CatalogMetadata tables relationships permissions
--     eventTriggers remoteSchemas functions fkeys' allowlistDefs computedFields
--     customTypes actions actionPermissions
--     <- liftTx fetchCatalogData

--   let fkeys = HS.fromList fkeys'

--   -- tables
--   modTableCache =<< buildTableCache tables

--   -- relationships
--   forM_ relationships $ \(CatalogRelation qt rn rt rDef cmnt) -> do
--     let objId = MOTableObj qt $ MTORel rn rt
--         def = toJSON $ WithTable qt $ RelDef rn rDef cmnt
--         mkInconsObj = InconsistentMetadataObj objId (MOTRel rt) def
--     modifyErr (\e -> "table " <> qt <<> "; rel " <> rn <<> "; " <> e) $
--       withSchemaObject_ mkInconsObj $
--       case rt of
--         ObjRel -> do
--           using <- decodeValue rDef
--           let relDef = RelDef rn using Nothing
--           validateObjRel qt relDef
--           objRelP2Setup qt fkeys relDef
--         ArrRel -> do
--           using <- decodeValue rDef
--           let relDef = RelDef rn using Nothing
--           validateArrRel qt relDef
--           arrRelP2Setup qt fkeys relDef

--   -- computedFields
--   forM_ computedFields $ \(CatalogComputedField column funcDefs) -> do
--     let AddComputedField qt name def comment = column
--         qf = _cfdFunction def
--         mkInconsObj =
--           InconsistentMetadataObj (MOTableObj qt $ MTOComputedField name)
--           MOTComputedField $ toJSON column
--     modifyErr (\e -> "computed field " <> name <<> "; " <> e) $
--       withSchemaObject_ mkInconsObj $ do
--       rawfi <- handleMultipleFunctions qf funcDefs
--       addComputedFieldP2Setup qt name def rawfi comment

--   -- permissions
--   forM_ permissions $ \(CatalogPermission qt rn pt pDef cmnt) -> do
--     let objId = MOTableObj qt $ MTOPerm rn pt
--         def = toJSON $ WithTable qt $ PermDef rn pDef cmnt
--         mkInconsObj = InconsistentMetadataObj objId (MOTPerm pt) def
--     modifyErr (\e -> "table " <> qt <<> "; role " <> rn <<> "; " <> e) $
--       withSchemaObject_ mkInconsObj $
--       case pt of
--           PTInsert -> permHelper withSetup sqlGenCtx qt rn pDef PAInsert
--           PTSelect -> permHelper withSetup sqlGenCtx qt rn pDef PASelect
--           PTUpdate -> permHelper withSetup sqlGenCtx qt rn pDef PAUpdate
--           PTDelete -> permHelper withSetup sqlGenCtx qt rn pDef PADelete

--   -- event triggers
--   forM_ eventTriggers $ \(CatalogEventTrigger qt trn configuration) -> do
--     let objId = MOTableObj qt $ MTOTrigger trn
--         def = object ["table" .= qt, "configuration" .= configuration]
--         mkInconsObj = InconsistentMetadataObj objId MOTEventTrigger def
--     withSchemaObject_ mkInconsObj $ do
--       etc <- decodeValue configuration
--       subTableP2Setup qt etc
--       allCols <- getCols . _tiFieldInfoMap <$> askTabInfo qt
--       when withSetup $ liftTx $
--         mkAllTriggersQ trn qt allCols (stringifyNum sqlGenCtx) (etcDefinition etc)

--   -- sql functions
--   forM_ functions $ \(CatalogFunction qf systemDefined config funcDefs) -> do
--     let def = toJSON $ TrackFunction qf
--         mkInconsObj =
--           InconsistentMetadataObj (MOFunction qf) MOTFunction def
--     modifyErr (\e -> "function " <> qf <<> "; " <> e) $
--       withSchemaObject_ mkInconsObj $ do
--       rawfi <- handleMultipleFunctions qf funcDefs
--       trackFunctionP2Setup qf systemDefined config rawfi

--   -- allow list
--   replaceAllowlist $ concatMap _cdQueries allowlistDefs

--   -- TODO: all of this needs to change
--   validateCustomTypesAndAddToCache customTypes
--   mapM_ validateAndCacheAction actions
--   mapM_ validateAndCacheActionPermission actionPermissions

--   -- build GraphQL context with tables and functions
--   GS.buildGCtxMapPG

--   -- remote schemas
--   forM_ remoteSchemas resolveSingleRemoteSchema

--   -- custom types
--   let mkInconistentCustomTypes =
--         InconsistentMetadataObj MOCustomTypes MOTCustomTypes $ toJSON customTypes
--   withSchemaObject_ mkInconistentCustomTypes $ do
--     validateCustomTypesAndAddToCache customTypes
--     -- TODO
--     sc <- askSchemaCache
--     (finalGCtxMap, finalDefaultGCtx) <-
--       mergeCustomTypes (scGCtxMap sc) (scDefaultRemoteGCtx sc) $ scCustomTypes sc
--     writeSchemaCache
--       sc { scGCtxMap = finalGCtxMap
--          , scDefaultRemoteGCtx = finalDefaultGCtx
--          }
--   -- validate tables' custom root fields
--   validateTablesCustomRootFields

--   where
--     permHelper setup sqlGenCtx qt rn pDef pa = do
--       qCtx <- mkAdminQCtx sqlGenCtx <$> askSchemaCache
--       perm <- decodeValue pDef
--       let permDef = PermDef rn perm Nothing
--           createPerm = WithTable qt permDef
--       (permInfo, deps) <- liftP1WithQCtx qCtx $ createPermP1 createPerm
--       when setup $ addPermP2Setup qt permDef permInfo
--       addPermToCache qt rn pa permInfo deps
--       -- p2F qt rn p1Res

--     resolveSingleRemoteSchema rs = do
--       let AddRemoteSchemaQuery name _ _ = rs
--           mkInconsObj = InconsistentMetadataObj (MORemoteSchema name)
--                         MOTRemoteSchema (toJSON rs)
--       withSchemaObject_ mkInconsObj $ do
--         rsCtx <- addRemoteSchemaP2Setup rs
--         sc <- askSchemaCache
--         let gCtxMap = scGCtxMap sc
--             defGCtx = scDefaultRemoteGCtx sc
--             rGCtx = convRemoteGCtx $ rscGCtx rsCtx
--         mergedGCtxMap <- mergeRemoteSchema gCtxMap rGCtx
--         mergedDefGCtx <- mergeGCtx defGCtx rGCtx
--         writeSchemaCache sc { scGCtxMap = mergedGCtxMap
--                             , scDefaultRemoteGCtx = mergedDefGCtx
--                             }

--     validateTablesCustomRootFields = do
--       sc <- askSchemaCache
--       let tables = M.elems $ scTables sc
--           defRemoteGCtx = scDefaultRemoteGCtx sc
--       forM_ tables $ \table -> do
--         let TableCustomRootFields sel selByPk selAgg ins upd del =
--               _tcCustomRootFields $ _tiCustomConfig table
--             rootFldNames = catMaybes [sel, selByPk, selAgg, ins, upd, del]
--         forM_ rootFldNames $ GS.checkConflictingNode defRemoteGCtx

-- -- | build GraphQL schema
-- buildGCtxMap
--   :: (QErrM m, CacheRWM m) => m ()
-- buildGCtxMap = do
--   -- build GraphQL Context with Hasura schema
--   GS.buildGCtxMapPG
--   sc <- askSchemaCache
--   let gCtxMap = scGCtxMap sc
--   -- Stitch remote schemas
--   (mergedGCtxMap, defGCtx) <- mergeSchemas (scRemoteSchemas sc) gCtxMap

--   -- ensure that there are no conflicts between autogenerated types
--   -- and custom types
--   (finalGCtxMap, finalDefaultGCtx) <-
--     mergeCustomTypes mergedGCtxMap defGCtx $ scCustomTypes sc

--   writeSchemaCache
--     sc { scGCtxMap = finalGCtxMap
--        , scDefaultRemoteGCtx = finalDefaultGCtx
--        }


-- -- | Rebuilds the schema cache. If an object with the given object id became newly inconsistent,
-- -- raises an error about it specifically. Otherwise, raises a generic metadata inconsistency error.
-- buildSchemaCacheFor :: (CacheBuildM m) => MetadataObjId -> m ()
-- buildSchemaCacheFor objectId = do
--   oldSchemaCache <- askSchemaCache
--   buildSchemaCache
--   newSchemaCache <- askSchemaCache

--   let diffInconsistentObjects = getDifference _moId `on` scInconsistentObjs
--       newInconsistentObjects = newSchemaCache `diffInconsistentObjects` oldSchemaCache

--   for_ (find ((== objectId) . _moId) newInconsistentObjects) $ \matchingObject ->
--     throw400 ConstraintViolation (_moReason matchingObject)

--   unless (null newInconsistentObjects) $
--     throwError (err400 Unexpected "cannot continue due to new inconsistent metadata")
--       { qeInternal = Just $ toJSON newInconsistentObjects }

-- -- | Like 'buildSchemaCache', but fails if there is any inconsistent metadata.
-- buildSchemaCacheStrict :: (CacheBuildM m) => m ()
-- buildSchemaCacheStrict = do
--   buildSchemaCache
--   sc <- askSchemaCache
--   let inconsObjs = scInconsistentObjs sc
--   unless (null inconsObjs) $ do
--     let err = err400 Unexpected "cannot continue due to inconsistent metadata"
--     throwError err{qeInternal = Just $ toJSON inconsObjs}

-- -- | Executes the given action, and if any new 'InconsistentMetadataObj's are added to the schema
-- -- cache as a result of its execution, raises an error.
-- withNewInconsistentObjsCheck :: (QErrM m, CacheRM m) => m a -> m a
-- withNewInconsistentObjsCheck action = do
--   originalObjects <- scInconsistentObjs <$> askSchemaCache
--   result <- action
--   currentObjects <- scInconsistentObjs <$> askSchemaCache
--   checkNewInconsistentMeta originalObjects currentObjects
--   pure result
-- =======
import           Debug.Trace

mergeCustomTypes
  :: MonadError QErr f
  -- => M.HashMap RoleName GS.GCtx -> GS.GCtx -> VT.TypeMap
  => M.HashMap RoleName GS.GCtx -> GS.GCtx -> (NonObjectTypeMap, AnnotatedObjects)
  -> f (GS.GCtxMap, GS.GCtx)
mergeCustomTypes gCtxMap remoteSchemaCtx customTypesState = do
  let adminCustomTypes = buildCustomTypesSchema (fst customTypesState)
                         (snd customTypesState) adminRole
  let commonTypes = M.intersectionWith (,) existingTypes adminCustomTypes
      conflictingCustomTypes =
        map (G.unNamedType . fst) $ M.toList $
        flip M.filter commonTypes $ \case
        -- only scalars can be common
        (VT.TIScalar _, VT.TIScalar _) -> False
        (_, _) -> True
  unless (null conflictingCustomTypes) $
    throw400 InvalidCustomTypes $
    "following custom types confilct with the " <>
    "autogenerated hasura types or from remote schemas: "
    <> showNames conflictingCustomTypes

  let gCtxMapWithCustomTypes = flip M.mapWithKey gCtxMap $ \roleName gCtx ->
        let customTypes = buildCustomTypesSchema (fst customTypesState)
                          (snd customTypesState) roleName
        in addCustomTypes gCtx customTypes

  -- populate the gctx of each role with the custom types
  return ( gCtxMapWithCustomTypes
         , addCustomTypes remoteSchemaCtx adminCustomTypes
         )
  where
    addCustomTypes gCtx customTypes =
      gCtx { GS._gTypes = GS._gTypes gCtx <> customTypes}
    existingTypes =
      case (M.lookup adminRole gCtxMap) of
        Just gCtx -> GS._gTypes gCtx
        Nothing   -> GS._gTypes remoteSchemaCtx

buildRebuildableSchemaCache
  :: (MonadIO m, MonadUnique m, MonadTx m, HasHttpManager m, HasSQLGenCtx m)
  => m (RebuildableSchemaCache m)
buildRebuildableSchemaCache = do
  catalogMetadata <- liftTx fetchCatalogData
  result <- flip runReaderT CatalogSync $ Inc.build buildSchemaCacheRule (catalogMetadata, M.empty)
  pure $ RebuildableSchemaCache (Inc.result result) M.empty (Inc.rebuildRule result)

newtype CacheRWT m a
  = CacheRWT { unCacheRWT :: StateT (RebuildableSchemaCache m) m a }
  deriving
    ( Functor, Applicative, Monad, MonadIO, MonadReader r, MonadError e, MonadWriter w, MonadTx
    , UserInfoM, HasHttpManager, HasSQLGenCtx, HasSystemDefined )

runCacheRWT :: RebuildableSchemaCache m -> CacheRWT m a -> m (a, RebuildableSchemaCache m)
runCacheRWT cache = flip runStateT cache . unCacheRWT

instance MonadTrans CacheRWT where
  lift = CacheRWT . lift

instance (Monad m) => TableCoreInfoRM (CacheRWT m)
instance (Monad m) => CacheRM (CacheRWT m) where
  askSchemaCache = CacheRWT $ gets lastBuiltSchemaCache

instance (MonadIO m, MonadTx m, MonadUnique m) => CacheRWM (CacheRWT m) where
  buildSchemaCacheWithOptions buildReason = CacheRWT do
    liftIO $ traceEventIO "START refresh"
    RebuildableSchemaCache _ invalidationMap rule <- get
    catalogMetadata <- liftTx fetchCatalogData
    liftIO $ traceEventIO "START build"
    result <- lift $ flip runReaderT buildReason $ Inc.build rule (catalogMetadata, invalidationMap)
    let schemaCache = Inc.result result
    liftIO $ traceEventIO "STOP build"
    liftIO $ traceEventIO "START prune"
    let !prunedInvalidationMap = pruneInvalidationMap schemaCache invalidationMap
    liftIO $ traceEventIO "STOP prune"
    liftIO $ traceEventIO "STOP refresh"
    put $ RebuildableSchemaCache schemaCache prunedInvalidationMap (Inc.rebuildRule result)
    where
      pruneInvalidationMap schemaCache = M.filterWithKey \name _ ->
        M.member name (scRemoteSchemas schemaCache)

  invalidateCachedRemoteSchema name = CacheRWT do
    unique <- newUnique
    assign (rscInvalidationMap . at name) (Just unique)

buildSchemaCacheRule
  -- Note: by supplying BuildReason via MonadReader, it does not participate in caching, which is
  -- what we want!
  :: ( ArrowChoice arr, Inc.ArrowDistribute arr, Inc.ArrowCache m arr
     , MonadIO m, MonadTx m, MonadReader BuildReason m, HasHttpManager m, HasSQLGenCtx m )
  => (CatalogMetadata, InvalidationMap) `arr` SchemaCache
buildSchemaCacheRule = proc inputs -> do
  (outputs, collectedInfo) <- runWriterA buildAndCollectInfo -< inputs
  let (inconsistentObjects, unresolvedDependencies) = partitionCollectedInfo collectedInfo
  (resolvedOutputs, extraInconsistentObjects, resolvedDependencies) <-
    resolveDependencies -< (outputs, unresolvedDependencies)
  returnA -< SchemaCache
    { scTables = _boTables resolvedOutputs
    , scActions = _boActions resolvedOutputs
    , scFunctions = _boFunctions resolvedOutputs
    , scRemoteSchemas = _boRemoteSchemas resolvedOutputs
    , scAllowlist = _boAllowlist resolvedOutputs
    , scCustomTypes = _boCustomTypes resolvedOutputs
    , scGCtxMap = _boGCtxMap resolvedOutputs
    , scDefaultRemoteGCtx = _boDefaultRemoteGCtx resolvedOutputs
    , scDepMap = resolvedDependencies
    , scInconsistentObjs = inconsistentObjects <> extraInconsistentObjects
    }
  where
    buildAndCollectInfo
      :: ( ArrowChoice arr, Inc.ArrowDistribute arr, Inc.ArrowCache m arr
         , ArrowWriter (Seq CollectedInfo) arr, MonadIO m, MonadTx m, MonadReader BuildReason m
         , HasHttpManager m, HasSQLGenCtx m )
      => (CatalogMetadata, InvalidationMap) `arr` BuildOutputs
    buildAndCollectInfo = proc (catalogMetadata, invalidationMap) -> do
      let CatalogMetadata tables relationships permissions
            eventTriggers remoteSchemas functions allowlistDefs
            computedFields customTypes actions actionPermissions
            = catalogMetadata

      -- tables
      bindA -< liftIO $ traceEventIO "START tables"
      tableRawInfos <- buildTableCache -< tables
      bindA -< liftIO $ traceEventIO "STOP tables"

      -- relationships and computed fields
      let relationshipsByTable = M.groupOn _crTable relationships
          computedFieldsByTable = M.groupOn (_afcTable . _cccComputedField) computedFields
      bindA -< liftIO $ traceEventIO "START fields"
      tableCoreInfos <- (tableRawInfos >- returnA)
        >-> (\info -> (info, relationshipsByTable) >- alignExtraTableInfo mkRelationshipMetadataObject)
        >-> (\info -> (info, computedFieldsByTable) >- alignExtraTableInfo mkComputedFieldMetadataObject)
        >-> (| Inc.keyed (\_ ((tableRawInfo, tableRelationships), tableComputedFields) -> do
                 let columns = _tciFieldInfoMap tableRawInfo
                 allFields <- addNonColumnFields -<
                   (tableRawInfos, columns, tableRelationships, tableComputedFields)
                 returnA -< tableRawInfo { _tciFieldInfoMap = allFields }) |)
      bindA -< liftIO $ traceEventIO "STOP fields"

      -- permissions and event triggers
      tableCoreInfosDep <- Inc.newDependency -< tableCoreInfos
      tableCache <- (tableCoreInfos >- returnA)
        >-> (\info -> (info, M.groupOn _cpTable permissions) >- alignExtraTableInfo mkPermissionMetadataObject)
        >-> (\info -> (info, M.groupOn _cetTable eventTriggers) >- alignExtraTableInfo mkEventTriggerMetadataObject)
        >-> (| Inc.keyed (\_ ((tableCoreInfo, tablePermissions), tableEventTriggers) -> do
                 bindA -< liftIO $ traceEventIO "START permissions"
                 let tableName = _tciName tableCoreInfo
                     tableFields = _tciFieldInfoMap tableCoreInfo
                 permissionInfos <- buildTablePermissions -<
                   (tableCoreInfosDep, tableName, tableFields, HS.fromList tablePermissions)
                 bindA -< liftIO $ traceEventIO "STOP permissions"
                 bindA -< liftIO $ traceEventIO "START event triggers"
                 eventTriggerInfos <- buildTableEventTriggers -< (tableCoreInfo, tableEventTriggers)
                 bindA -< liftIO $ traceEventIO "STOP event triggers"
                 returnA -< TableInfo
                   { _tiCoreInfo = tableCoreInfo
                   , _tiRolePermInfoMap = permissionInfos
                   , _tiEventTriggerInfoMap = eventTriggerInfos
                   }) |)

      -- sql functions
      bindA -< liftIO $ traceEventIO "START functions"
      let tableNames = HS.fromList $ M.keys tableCache
      functionCache <- (mapFromL _cfFunction functions >- returnA)
        >-> (| Inc.keyed (\_ (CatalogFunction qf systemDefined config funcDefs) -> do
                 let definition = toJSON $ TrackFunction qf
                     metadataObject = MetadataObject (MOFunction qf) definition
                     schemaObject = SOFunction qf
                     addFunctionContext e = "in function " <> qf <<> ": " <> e
                 (| withRecordInconsistency (
                    (| modifyErrA (do
                         rawfi <- bindErrorA -< handleMultipleFunctions qf funcDefs
                         (fi, dep) <- bindErrorA -<
                           trackFunctionP2Setup tableNames qf systemDefined config rawfi
                         recordDependencies -< (metadataObject, schemaObject, [dep])
                         returnA -< fi)
                    |) addFunctionContext)
                  |) metadataObject) |)
        >-> (\infos -> M.catMaybes infos >- returnA)
      bindA -< liftIO $ traceEventIO "STOP functions"

      -- allow list
      let allowList = allowlistDefs
            & concatMap _cdQueries
            & map (queryWithoutTypeNames . getGQLQuery . _lqQuery)
            & HS.fromList

      -- custom types
      resolvedCustomTypes <- bindA -< resolveCustomTypes tableCache customTypes

      actionCache <- (\infos -> (M.catMaybes . M.catMaybes) infos >- returnA) <-<
        (| Inc.keyed (\_ duplicateActions -> do
             maybeAction <- noDuplicates mkActionMetadataObj -< duplicateActions
             (| traverseA (\action -> do
                  let CreateAction name def _ = action
                      metadataObj = mkActionMetadataObj action
                      addActionContext e = "in action " <> name <<> "; " <> e

                  (| withRecordInconsistency (
                      (| modifyErrA (do
                           resolvedDef <- bindErrorA -< resolveAction resolvedCustomTypes name def
                           returnA -< resolvedDef)
                       |) addActionContext)
                   |) metadataObj
                   >-> (\maybeResolvedDef ->
                        (| traverseA (\resolvedDef -> do
                            actionPerms <- (\info -> M.catMaybes info >- returnA) <-<
                              (| Inc.keyed (\_ duplicatePerms -> do
                                   maybePerm <- noDuplicates mkActionPermissionObj -< duplicatePerms
                                   (| traverseA (\perm -> do
                                        let role = _capRole perm
                                            permDef = _capDefinition perm
                                        selFilter <- bindA -< buildActionFilter (_apdSelect permDef)
                                        returnA -< ActionPermissionInfo role selFilter)
                                    |) maybePerm)
                               |) (M.groupOn _capRole $ filter (\perm -> _capAction perm == name) actionPermissions)
                            returnA -< ActionInfo name resolvedDef actionPerms
                            )
                         |) maybeResolvedDef
                        ))
              |) maybeAction)
        |) (M.groupOn _caName actions)


      -- build GraphQL context with tables and functions
      bindA -< liftIO $ traceEventIO "START GQL"
      baseGQLSchema <- bindA -< GS.mkGCtxMap (snd resolvedCustomTypes) tableCache functionCache actionCache
      bindA -< liftIO $ traceEventIO "STOP GQL"

      -- remote schemas
      bindA -< liftIO $ traceEventIO "START remote schemas"
      let invalidatedRemoteSchemas = flip map remoteSchemas \remoteSchema ->
            (M.lookup (_arsqName remoteSchema) invalidationMap, remoteSchema)
      (remoteSchemaMap, gqlSchema, remoteGQLSchema) <-
        (| foldlA' (\schemas schema -> (schemas, schema) >- addRemoteSchema)
        |) (M.empty, baseGQLSchema, GC.emptyGCtx) invalidatedRemoteSchemas
        >-> (\(remoteSchemaMap, gqlSchema, remoteGQLSchema) -> do
                 (gqlSchema', defGqlSchema') <- bindA -< mergeCustomTypes gqlSchema remoteGQLSchema resolvedCustomTypes
                 returnA -< (remoteSchemaMap, gqlSchema', defGqlSchema'))
      bindA -< liftIO $ traceEventIO "STOP remote schemas"

      returnA -< BuildOutputs
        { _boTables = tableCache
        , _boActions = actionCache
        , _boFunctions = functionCache
        , _boRemoteSchemas = remoteSchemaMap
        , _boAllowlist = allowList
        , _boCustomTypes = resolvedCustomTypes
        , _boGCtxMap = gqlSchema
        , _boDefaultRemoteGCtx = remoteGQLSchema
        }

    mkEventTriggerMetadataObject (CatalogEventTrigger qt trn configuration) =
      let objectId = MOTableObj qt $ MTOTrigger trn
          definition = object ["table" .= qt, "configuration" .= configuration]
      in MetadataObject objectId definition

    mkActionMetadataObj ca = MetadataObject (MOAction $ _caName ca) $ toJSON ca
    mkActionPermissionObj p =
      let role = _capRole p
          name = _capAction p
      in MetadataObject (MOActionPermission name role) $ toJSON p

    -- Given a map of table info, “folds in” another map of information, accumulating inconsistent
    -- metadata objects for any entries in the second map that don’t appear in the first map. This
    -- is used to “line up” the metadata for relationships, computed fields, permissions, etc. with
    -- the tracked table info.
    alignExtraTableInfo
      :: forall a b arr
       . (ArrowChoice arr, Inc.ArrowDistribute arr, ArrowWriter (Seq CollectedInfo) arr)
      => (b -> MetadataObject)
      -> ( M.HashMap QualifiedTable a
         , M.HashMap QualifiedTable [b]
         ) `arr` M.HashMap QualifiedTable (a, [b])
    alignExtraTableInfo mkMetadataObject = proc (baseInfo, extraInfo) -> do
      combinedInfo <-
        (| Inc.keyed (\tableName infos -> combine -< (tableName, infos))
        |) (align baseInfo extraInfo)
      returnA -< M.catMaybes combinedInfo
      where
        combine :: (QualifiedTable, These a [b]) `arr` Maybe (a, [b])
        combine = proc (tableName, infos) -> case infos of
          This  base        -> returnA -< Just (base, [])
          These base extras -> returnA -< Just (base, extras)
          That       extras -> do
            let errorMessage = "table " <> tableName <<> " does not exist"
            recordInconsistencies -< (map mkMetadataObject extras, errorMessage)
            returnA -< Nothing

    buildTableEventTriggers
      :: ( ArrowChoice arr, Inc.ArrowDistribute arr, ArrowWriter (Seq CollectedInfo) arr
         , Inc.ArrowCache m arr, MonadIO m, MonadTx m, MonadReader BuildReason m, HasSQLGenCtx m )
      => (TableCoreInfo, [CatalogEventTrigger]) `arr` EventTriggerInfoMap
    buildTableEventTriggers = proc (tableInfo, eventTriggers) ->
      (\infos -> M.catMaybes infos >- returnA) <-<
        (| Inc.keyed (\_ duplicateEventTriggers -> do
             maybeEventTrigger <- noDuplicates mkEventTriggerMetadataObject -< duplicateEventTriggers
             (\info -> join info >- returnA) <-<
               (| traverseA (\eventTrigger -> buildEventTrigger -< (tableInfo, eventTrigger))
               |) maybeEventTrigger)
        |) (M.groupOn _cetName eventTriggers)
      where
        buildEventTrigger = proc (tableInfo, eventTrigger) -> do
          let CatalogEventTrigger qt trn configuration = eventTrigger
              metadataObject = mkEventTriggerMetadataObject eventTrigger
              schemaObjectId = SOTableObj qt $ TOTrigger trn
              addTriggerContext e = "in event trigger " <> trn <<> ": " <> e
          (| withRecordInconsistency (
             (| modifyErrA (do
                  etc <- bindErrorA -< decodeValue configuration
                  (info, dependencies) <- bindErrorA -< subTableP2Setup qt etc
                  let tableColumns = M.mapMaybe (^? _FIColumn) (_tciFieldInfoMap tableInfo)
                  recreateViewIfNeeded -< (qt, tableColumns, trn, etcDefinition etc)
                  recordDependencies -< (metadataObject, schemaObjectId, dependencies)
                  returnA -< info)
             |) (addTableContext qt . addTriggerContext))
           |) metadataObject

        recreateViewIfNeeded = Inc.cache $
          arrM \(tableName, tableColumns, triggerName, triggerDefinition) -> do
            buildReason <- ask
            when (buildReason == CatalogUpdate) $
              mkAllTriggersQ triggerName tableName (M.elems tableColumns) triggerDefinition

    addRemoteSchema
      :: ( ArrowChoice arr, ArrowWriter (Seq CollectedInfo) arr, ArrowKleisli m arr
         , MonadIO m, HasHttpManager m )
      => ( (RemoteSchemaMap, GS.GCtxMap, GS.GCtx)
         , (Maybe InvalidationKey, AddRemoteSchemaQuery)
         ) `arr` (RemoteSchemaMap, GS.GCtxMap, GS.GCtx)
    addRemoteSchema = proc ((remoteSchemas, gCtxMap, defGCtx), (_, remoteSchema)) -> do
      let name = _arsqName remoteSchema
      (| onNothingA (returnA -< (remoteSchemas, gCtxMap, defGCtx)) |) <-<
         (| withRecordInconsistency (case M.lookup name remoteSchemas of
              Just _ -> throwA -< err400 AlreadyExists "duplicate definition for remote schema"
              Nothing -> liftEitherA <<< bindA -< runExceptT do
                rsCtx <- addRemoteSchemaP2Setup remoteSchema
                let rGCtx = convRemoteGCtx $ rscGCtx rsCtx
                mergedGCtxMap <- mergeRemoteSchema gCtxMap rGCtx
                mergedDefGCtx <- mergeGCtx defGCtx rGCtx
                pure (M.insert name rsCtx remoteSchemas, mergedGCtxMap, mergedDefGCtx))
         |) (MetadataObject (MORemoteSchema name) (toJSON remoteSchema))
-- >>>>>>> 3354-faster-metadata-migrations

-- | @'withMetadataCheck' cascade action@ runs @action@ and checks if the schema changed as a
-- result. If it did, it checks to ensure the changes do not violate any integrity constraints, and
-- if not, incorporates them into the schema cache.
withMetadataCheck :: (MonadTx m, CacheRWM m) => Bool -> m a -> m a
withMetadataCheck cascade action = do
  -- Drop hdb_views so no interference is caused to the sql query
  liftTx $ Q.catchE defaultTxErrorHandler clearHdbViews

  -- Get the metadata before the sql query, everything, need to filter this
  oldMetaU <- liftTx $ Q.catchE defaultTxErrorHandler fetchTableMeta
  oldFuncMetaU <- liftTx $ Q.catchE defaultTxErrorHandler fetchFunctionMeta

  -- Run the action
  res <- action

  -- Get the metadata after the sql query
  newMeta <- liftTx $ Q.catchE defaultTxErrorHandler fetchTableMeta
  newFuncMeta <- liftTx $ Q.catchE defaultTxErrorHandler fetchFunctionMeta
  sc <- askSchemaCache
  let existingInconsistentObjs = scInconsistentObjs sc
      existingTables = M.keys $ scTables sc
      oldMeta = flip filter oldMetaU $ \tm -> tmTable tm `elem` existingTables
      schemaDiff = getSchemaDiff oldMeta newMeta
      existingFuncs = M.keys $ scFunctions sc
      oldFuncMeta = flip filter oldFuncMetaU $ \fm -> fmFunction fm `elem` existingFuncs
      FunctionDiff droppedFuncs alteredFuncs = getFuncDiff oldFuncMeta newFuncMeta
      overloadedFuncs = getOverloadedFuncs existingFuncs newFuncMeta

  -- Do not allow overloading functions
  unless (null overloadedFuncs) $
    throw400 NotSupported $ "the following tracked function(s) cannot be overloaded: "
    <> reportFuncs overloadedFuncs

  indirectDeps <- getSchemaChangeDeps schemaDiff

  -- Report back with an error if cascade is not set
  when (indirectDeps /= [] && not cascade) $ reportDepsExt indirectDeps []

  -- Purge all the indirect dependents from state
  mapM_ purgeDependentObject indirectDeps

  -- Purge all dropped functions
  let purgedFuncs = flip mapMaybe indirectDeps $ \dep ->
        case dep of
          SOFunction qf -> Just qf
          _             -> Nothing

  forM_ (droppedFuncs \\ purgedFuncs) $ \qf -> do
    liftTx $ delFunctionFromCatalog qf

  -- Process altered functions
  forM_ alteredFuncs $ \(qf, newTy) -> do
    when (newTy == FTVOLATILE) $
      throw400 NotSupported $
      "type of function " <> qf <<> " is altered to \"VOLATILE\" which is not supported now"

  -- update the schema cache and hdb_catalog with the changes
  processSchemaChanges schemaDiff

  buildSchemaCache
  currentInconsistentObjs <- scInconsistentObjs <$> askSchemaCache
  checkNewInconsistentMeta existingInconsistentObjs currentInconsistentObjs

  return res
  where
    reportFuncs = T.intercalate ", " . map dquoteTxt

    processSchemaChanges :: (MonadTx m, CacheRM m) => SchemaDiff -> m ()
    processSchemaChanges schemaDiff = do
      -- Purge the dropped tables
      mapM_ delTableAndDirectDeps droppedTables

      sc <- askSchemaCache
      for_ alteredTables $ \(oldQtn, tableDiff) -> do
        ti <- case M.lookup oldQtn $ scTables sc of
          Just ti -> return ti
          Nothing -> throw500 $ "old table metadata not found in cache : " <>> oldQtn
        processTableChanges (_tiCoreInfo ti) tableDiff
      where
        SchemaDiff droppedTables alteredTables = schemaDiff

    checkNewInconsistentMeta
      :: (QErrM m)
      => [InconsistentMetadata] -> [InconsistentMetadata] -> m ()
    checkNewInconsistentMeta originalInconsMeta currentInconsMeta =
      unless (null newInconsistentObjects) $
        throwError (err500 Unexpected "cannot continue due to newly found inconsistent metadata")
          { qeInternal = Just $ toJSON newInconsistentObjects }
      where
        diffInconsistentObjects = M.difference `on` groupInconsistentMetadataById
        newInconsistentObjects = nub $ concatMap toList $
          M.elems (currentInconsMeta `diffInconsistentObjects` originalInconsMeta)
