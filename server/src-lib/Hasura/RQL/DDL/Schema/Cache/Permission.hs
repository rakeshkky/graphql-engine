{-# LANGUAGE Arrows #-}

module Hasura.RQL.DDL.Schema.Cache.Permission
  ( buildTablePermissions
  , mkPermissionMetadataObject
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended       as M
import qualified Data.Sequence                      as Seq

import           Control.Arrow.Extended
import           Data.Aeson

import qualified Hasura.Incremental                 as Inc

import           Hasura.Db
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.DDL.Permission.Internal
import           Hasura.RQL.DDL.Schema.Cache.Common
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Catalog
import           Hasura.SQL.Types

-- see Note [Specialization of buildRebuildableSchemaCache] in Hasura.RQL.DDL.Schema.Cache
{-# SPECIALIZE buildTablePermissions
    :: CacheBuildA
    ( TableCoreCache
    , TableCoreInfo FieldInfo
    , [CatalogPermission]
    ) RolePermInfoMap #-}

buildTablePermissions
  :: ( Inc.ArrowCache arr, Inc.ArrowDistribute arr, ArrowKleisli m arr
     , ArrowWriter (Seq CollectedInfo) arr, MonadTx m, MonadReader BuildReason m )
  => ( TableCoreCache
     , TableCoreInfo FieldInfo
     , [CatalogPermission]
     ) `arr` RolePermInfoMap
buildTablePermissions = proc (tableCache, tableInfo, tablePermissions) ->
  (| Inc.keyed (\_ rolePermissions -> do
       let (insertPerms, selectPerms, updatePerms, deletePerms) =
             partitionPermissions rolePermissions

       insertPermInfo <- buildPermission -< (tableCache, tableInfo, insertPerms)
       selectPermInfo <- buildPermission -< (tableCache, tableInfo, selectPerms)
       updatePermInfo <- buildPermission -< (tableCache, tableInfo, updatePerms)
       deletePermInfo <- buildPermission -< (tableCache, tableInfo, deletePerms)

       returnA -< RolePermInfo
         { _permIns = insertPermInfo
         , _permSel = selectPermInfo
         , _permUpd = updatePermInfo
         , _permDel = deletePermInfo
         })
  |) (M.groupOn _cpRole tablePermissions)
  where
    partitionPermissions = flip foldr ([], [], [], []) $
      \perm (insertPerms, selectPerms, updatePerms, deletePerms) -> case _cpPermType perm of
        PTInsert -> (perm:insertPerms, selectPerms, updatePerms, deletePerms)
        PTSelect -> (insertPerms, perm:selectPerms, updatePerms, deletePerms)
        PTUpdate -> (insertPerms, selectPerms, perm:updatePerms, deletePerms)
        PTDelete -> (insertPerms, selectPerms, updatePerms, perm:deletePerms)

mkPermissionMetadataObject :: CatalogPermission -> MetadataObject
mkPermissionMetadataObject (CatalogPermission qt rn pt pDef cmnt) =
  let objectId = MOTableObj qt $ MTOPerm rn pt
      definition = toJSON $ WithTable qt $ PermDef rn pDef cmnt
  in MetadataObject objectId definition

withPermission
  :: (ArrowChoice arr, ArrowWriter (Seq CollectedInfo) arr)
  => WriterA (Seq SchemaDependency) (ErrorA QErr arr) (a, s) b
  -> arr (a, (CatalogPermission, s)) (Maybe b)
withPermission f = proc (e, (permission, s)) -> do
  let CatalogPermission tableName roleName permType _ _ = permission
      metadataObject = mkPermissionMetadataObject permission
      schemaObject = SOTableObj tableName $ TOPerm roleName permType
      addPermContext err = "in permission for role " <> roleName <<> ": " <> err
  (| withRecordInconsistency (
     (| withRecordDependencies (
        (| modifyErrA (f -< (e, s))
        |) (addTableContext tableName . addPermContext))
     |) metadataObject schemaObject)
   |) metadataObject

buildPermission
  :: ( Inc.ArrowCache arr, Inc.ArrowDistribute arr, ArrowKleisli m arr
     , ArrowWriter (Seq CollectedInfo) arr, MonadTx m, MonadReader BuildReason m
     , Eq a, IsPerm a, FromJSON a, Eq (PermInfo a) )
  => ( TableCoreCache
     , TableCoreInfo FieldInfo
     , [CatalogPermission]
     ) `arr` Maybe (PermInfo a)
buildPermission = proc (tableCache, tableInfo, permissions) ->
      (permissions >- noDuplicates mkPermissionMetadataObject)
  >-> (| traverseA (\permission@(CatalogPermission _ roleName _ pDef _) ->
         (| withPermission (do
              bindErrorA -< when (roleName == adminRole) $
                throw400 ConstraintViolation "cannot define permission for admin role"
              perm <- bindErrorA -< decodeValue pDef
              let permDef = PermDef roleName perm Nothing
              (info, dependencies) <- bindErrorA -<
                runTableCoreCacheRT (buildPermInfo tableInfo permDef) tableCache
              tellA -< Seq.fromList dependencies
              rebuildViewsIfNeeded -< (_tciName tableInfo, permDef, info)
              returnA -< info)
         |) permission) |)
  >-> (\info -> join info >- returnA)

rebuildViewsIfNeeded
  :: ( Inc.ArrowCache arr, ArrowKleisli m arr, MonadTx m, MonadReader BuildReason m
     , Eq a, IsPerm a, Eq (PermInfo a) )
  => (QualifiedTable, PermDef a, PermInfo a) `arr` ()
rebuildViewsIfNeeded = Inc.cache $ arrM \(tableName, permDef, info) -> do
  buildReason <- ask
  when (buildReason == CatalogUpdate) $
    addPermP2Setup tableName permDef info
