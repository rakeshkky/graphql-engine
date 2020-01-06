module Hasura.RQL.Types.Metadata where

import qualified Data.HashMap.Strict.Extended   as M

import           Data.Aeson
import           Hasura.Prelude

import qualified Data.Text                      as T

import           Hasura.RQL.Types.Action
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.ComputedField
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Permission
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.SQL.Types

-- <<<<<<< HEAD
-- data MetadataObjType
--   = MOTTable
--   | MOTRel !RelType
--   | MOTPerm !PermType
--   | MOTEventTrigger
--   | MOTFunction
--   | MOTRemoteSchema
--   | MOTComputedField
--   | MOTCustomTypes
--   deriving (Eq, Generic)
-- instance Hashable MetadataObjType

-- instance Show MetadataObjType where
--   show MOTTable         = "table"
--   show (MOTRel ty)      = T.unpack (relTypeToTxt ty) <> "_relation"
--   show (MOTPerm ty)     = show ty <> "_permission"
--   show MOTEventTrigger  = "event_trigger"
--   show MOTFunction      = "function"
--   show MOTRemoteSchema  = "remote_schema"
--   show MOTComputedField = "computed_field"
--   show MOTCustomTypes   = "custom_types"

-- instance ToJSON MetadataObjType where
--   toJSON = String . T.pack . show

-- =======
-- >>>>>>> 3354-faster-metadata-migrations
data TableMetadataObjId
  = MTORel !RelName !RelType
  | MTOComputedField !ComputedFieldName
  | MTOPerm !RoleName !PermType
  | MTOTrigger !TriggerName
  deriving (Show, Eq, Generic)
instance Hashable TableMetadataObjId

data MetadataObjId
  = MOTable !QualifiedTable
  | MOFunction !QualifiedFunction
  | MORemoteSchema !RemoteSchemaName
  | MOTableObj !QualifiedTable !TableMetadataObjId
  | MOCustomTypes
  | MOAction !ActionName
  | MOActionPermission !ActionName !RoleName
  deriving (Show, Eq, Generic)
instance Hashable MetadataObjId

moiTypeName :: MetadataObjId -> Text
moiTypeName = \case
  MOTable _ -> "table"
  MOFunction _ -> "function"
  MORemoteSchema _ -> "remote_schema"
  MOTableObj _ tableObjectId -> case tableObjectId of
    MTORel _ relType   -> relTypeToTxt relType <> "_relation"
    MTOPerm _ permType -> permTypeToCode permType <> "_permission"
    MTOTrigger _       -> "event_trigger"
    MTOComputedField _ -> "computed_field"
  MOCustomTypes -> "custom_types"
  MOAction _    -> "action"
  MOActionPermission _ _ -> "action_permission"

moiName :: MetadataObjId -> Text
moiName objectId = moiTypeName objectId <> " " <> case objectId of
  MOTable name -> dquoteTxt name
  MOFunction name -> dquoteTxt name
  MORemoteSchema name -> dquoteTxt name
  MOTableObj tableName tableObjectId ->
    let tableObjectName = case tableObjectId of
          MTORel name _         -> dquoteTxt name
          MTOComputedField name -> dquoteTxt name
          MTOPerm name _        -> dquoteTxt name
          MTOTrigger name       -> dquoteTxt name
    in tableObjectName <> " in " <> moiName (MOTable tableName)
  MOCustomTypes -> ""
  MOAction name -> dquoteTxt name
  MOActionPermission name role -> dquoteTxt role <> " in " <> dquoteTxt name

data MetadataObject
  = MetadataObject
  { _moId         :: !MetadataObjId
  , _moDefinition :: !Value
  } deriving (Show, Eq)

data InconsistentMetadata
  = InconsistentObject !Text !MetadataObject
  | ConflictingObjects !Text ![MetadataObject]
  | DuplicateObjects !MetadataObjId ![Value]
  deriving (Show, Eq)

imObjectIds :: InconsistentMetadata -> [MetadataObjId]
imObjectIds = \case
  InconsistentObject _ metadata -> [_moId metadata]
  ConflictingObjects _ metadatas -> map _moId metadatas
  DuplicateObjects objectId _ -> [objectId]

imReason :: InconsistentMetadata -> Text
imReason = \case
  InconsistentObject reason _ -> reason
  ConflictingObjects reason _ -> reason
  DuplicateObjects objectId _ -> "multiple definitions for " <> moiName objectId

-- | Builds a map from each unique metadata object id to the inconsistencies associated with it.
-- Note that a single inconsistency can involve multiple metadata objects, so the same inconsistency
-- may appear in the resulting map multiple times!
groupInconsistentMetadataById
  :: [InconsistentMetadata] -> HashMap MetadataObjId (NonEmpty InconsistentMetadata)
groupInconsistentMetadataById = M.fromListWith (<>) . concatMap \metadata ->
  map (, metadata :| []) (imObjectIds metadata)

instance ToJSON InconsistentMetadata where
  toJSON inconsistentMetadata = object (("reason" .= imReason inconsistentMetadata) : extraFields)
    where
      extraFields = case inconsistentMetadata of
        InconsistentObject _ metadata -> metadataObjectFields metadata
        ConflictingObjects _ metadatas ->
          [ "objects" .= map (object . metadataObjectFields) metadatas ]
        DuplicateObjects objectId definitions ->
          [ "type" .= String (moiTypeName objectId)
          , "definitions" .= definitions ]

      metadataObjectFields (MetadataObject objectId definition) =
        [ "type" .= String (moiTypeName objectId)
        , "definition" .= definition ]
