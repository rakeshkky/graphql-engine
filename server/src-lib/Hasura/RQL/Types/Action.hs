module Hasura.RQL.Types.Action
  ( ArgumentName(..)
  , ArgumentDefinition(..)

  , ActionName(..)
  , ActionKind(..)
  , ActionDefinition(..)
  , getActionKind
  , CreateAction(..)
  , UpdateAction(..)
  , ActionDefinitionInput
  , InputWebhook(..)

  , ResolvedWebhook(..)
  , ResolvedActionDefinition

  , ActionInfo(..)
  , ActionPermissionInfo(..)

  , ActionPermissionMap

  , ActionPermissionSelect(..)
  , ActionPermissionDefinition(..)
  , CreateActionPermission(..)

  ) where


import           Data.URL.Template
import           Hasura.Incremental            (Cacheable)
import           Hasura.Prelude
import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.CustomTypes
import           Hasura.RQL.Types.DML
import           Hasura.RQL.Types.Permission
import           Hasura.SQL.Types
import           Language.Haskell.TH.Syntax    (Lift)

import qualified Data.Aeson                    as J
import qualified Data.Aeson.Casing             as J
import qualified Data.Aeson.TH                 as J
import qualified Data.HashMap.Strict           as Map
import qualified Data.Text                     as T
import qualified Database.PG.Query             as Q
import qualified Language.GraphQL.Draft.Syntax as G

newtype ActionName
  = ActionName { unActionName :: G.Name }
  deriving ( Show, Eq, J.FromJSON, J.ToJSON, J.FromJSONKey, J.ToJSONKey
           , Hashable, DQuote, Lift, Generic, NFData, Cacheable)

instance Q.FromCol ActionName where
  fromCol bs = ActionName . G.Name <$> Q.fromCol bs

instance Q.ToPrepArg ActionName where
  toPrepVal = Q.toPrepVal . G.unName . unActionName

newtype ResolvedWebhook
  = ResolvedWebhook { unResolvedWebhook :: Text}
  deriving ( Show, Eq, J.FromJSON, J.ToJSON, Hashable, DQuote, Lift)

data ActionKind
  = ActionSynchronous
  | ActionAsynchronous
  deriving (Show, Eq, Lift, Generic)
instance NFData ActionKind
instance Cacheable ActionKind
$(J.deriveJSON
  J.defaultOptions { J.constructorTagModifier = J.snakeCase . drop 6}
  ''ActionKind)

newtype ArgumentName
  = ArgumentName { unArgumentName :: G.Name }
  deriving ( Show, Eq, J.FromJSON, J.ToJSON, J.FromJSONKey, J.ToJSONKey
           , Hashable, DQuote, Lift, Generic, NFData, Cacheable)

data ArgumentDefinition
  = ArgumentDefinition
  { _argName        :: !ArgumentName
  , _argType        :: !GraphQLType
  , _argDescription :: !(Maybe G.Description)
  } deriving (Show, Eq, Lift, Generic)
instance NFData ArgumentDefinition
instance Cacheable ArgumentDefinition
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''ArgumentDefinition)

data ActionDefinition a
  = ActionDefinition
  { _adArguments  :: ![ArgumentDefinition]
  , _adOutputType :: !GraphQLType
  , _adKind       :: !(Maybe ActionKind)
  , _adHandler    :: !a
  } deriving (Show, Eq, Lift, Functor, Foldable, Traversable, Generic)
instance (NFData a) => NFData (ActionDefinition a)
instance (Cacheable a) => Cacheable (ActionDefinition a)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''ActionDefinition)

getActionKind :: ActionDefinition a -> ActionKind
getActionKind = fromMaybe ActionSynchronous . _adKind

type ResolvedActionDefinition = ActionDefinition ResolvedWebhook

data ActionPermissionInfo
  = ActionPermissionInfo
  { _apiRole   :: !RoleName
  , _apiFilter :: !AnnBoolExpPartialSQL
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) ''ActionPermissionInfo)

type ActionPermissionMap
  = Map.HashMap RoleName ActionPermissionInfo

-- data ActionMetadataField
--   = ActionMetadataFieldId
--   | ActionMetadataFieldCreatedAt
--   | ActionMetadataFieldStatus
--   deriving (Show, Eq)

-- data ActionOutputTypeInfo
--   = ActionOutputScalar !VT.ScalarTyInfo
--   | ActionOutputEnum !VT.EnumTyInfo
--   | ActionOutputObject !VT.ObjTyInfo
--   deriving (Show, Eq)

-- TODO: this is terrible
-- instance J.ToJSON ActionOutputTypeInfo where
--   toJSON = J.toJSON . show

data ActionInfo
  = ActionInfo
  { _aiName        :: !ActionName
  , _aiDefinition  :: !ResolvedActionDefinition
  , _aiPermissions :: !ActionPermissionMap
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 3 J.snakeCase) ''ActionInfo)

data InputWebhook
  = IWTemplate !URLTemplate
  | IWPlain !Text
  deriving (Show, Eq, Lift, Generic)
instance NFData InputWebhook
instance Cacheable InputWebhook

instance J.ToJSON InputWebhook where
  toJSON = \case
    IWTemplate template -> J.String $ printURLTemplate template
    IWPlain t           -> J.String t

instance J.FromJSON InputWebhook where
  parseJSON = J.withText "String" $ \t ->
    if T.any (== '{') t then
      case parseURLTemplate t of
        Left _         -> fail "Parsing URL template failed"
        Right template -> pure $ IWTemplate template
    else pure $ IWPlain t

type ActionDefinitionInput = ActionDefinition InputWebhook

data CreateAction
  = CreateAction
  { _caName       :: !ActionName
  , _caDefinition :: !ActionDefinitionInput
  , _caComment    :: !(Maybe Text)
  } deriving (Show, Eq, Lift, Generic)
instance NFData CreateAction
instance Cacheable CreateAction
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''CreateAction)

data UpdateAction
  = UpdateAction
  { _uaName       :: !ActionName
  , _uaDefinition :: !ActionDefinitionInput
  } deriving (Show, Eq, Lift)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''UpdateAction)

newtype ActionPermissionSelect
  = ActionPermissionSelect
  { _apsFilter :: BoolExp
  } deriving (Show, Eq, Lift, Generic)
instance NFData ActionPermissionSelect
instance Cacheable ActionPermissionSelect
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''ActionPermissionSelect)

newtype ActionPermissionDefinition
  = ActionPermissionDefinition
  { _apdSelect :: ActionPermissionSelect
  } deriving (Show, Eq, Lift, Generic)
instance NFData ActionPermissionDefinition
instance Cacheable ActionPermissionDefinition
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''ActionPermissionDefinition)

data CreateActionPermission
  = CreateActionPermission
  { _capAction     :: !ActionName
  , _capRole       :: !RoleName
  , _capDefinition :: !ActionPermissionDefinition
  , _capComment    :: !(Maybe Text)
  } deriving (Show, Eq, Lift, Generic)
instance NFData CreateActionPermission
instance Cacheable CreateActionPermission
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''CreateActionPermission)
