module Hasura.GraphQL.Schema.Action
  ( mkActionsSchema
  ) where

import qualified Data.HashMap.Strict           as Map
import qualified Language.GraphQL.Draft.Syntax as G

import           Control.Lens                  ((^.))
import           Data.Coerce                   (coerce)

import           Hasura.GraphQL.Schema.Builder

import           Hasura.GraphQL.Resolve.Types
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

mkActionSelectionType :: ActionName -> G.NamedType
mkActionSelectionType =
  G.NamedType . unActionName

mkActionResponseTypeInfo
  :: ActionName
  -- Name of the action
  -> GraphQLType
  -- output type
  -> ObjTyInfo
mkActionResponseTypeInfo actionName outputType =
  mkHsraObjTyInfo
  (Just description)
  (mkActionSelectionType actionName) -- "(action_name)_input"
  mempty -- no arguments
  (mapFromL _fiName fieldDefinitions)
  where
    description = G.Description $ "fields of action: " <>> actionName

    mkFieldDefinition (fieldName, fieldDescription, fieldType) =
      mkHsraObjFldInfo
      (Just fieldDescription)
      fieldName
      mempty
      fieldType

    fieldDefinitions = map mkFieldDefinition
      [ ( "id", "the unique id of an action"
        , G.toGT $ mkScalarTy PGUUID)
      , ( "created_at", "the time at which this action was created"
        , G.toGT $ mkScalarTy PGTimeStampTZ)
      , ( "errors", "errors related to the invocation"
        , G.toGT $ mkScalarTy PGJSON)
      -- , ( "status", "the status of this action, whether it is processed, etc."
      --   , G.toGT $ G.NamedType "action_status")
      , ( "output", "the output fields of this action"
        , unGraphQLType outputType)
      ]

mkMutationField
  :: ActionName
  -> ActionInfo
  -> ActionPermissionInfo
  -> [(PGCol, PGScalarType)]
  -> (ActionExecutionContext, ObjFldInfo)
mkMutationField actionName actionInfo permission definitionList =
  ( actionExecutionContext
  , fieldInfo
  )
  where
    definition = _aiDefinition actionInfo
    actionExecutionContext =
      case getActionKind definition of
        ActionSynchronous  ->
          ActionExecutionSyncWebhook $ SyncActionExecutionContext
  -- TODO: only covers object types
          (ExecOnPostgres definitionList)
          (_adHandler definition)
        ActionAsynchronous -> ActionExecutionAsync $ _apiFilter permission

    -- TODO: we need to capture the comment from action definition
    description =
      G.Description $ "perform the action: " <>> actionName

    fieldInfo =
      mkHsraObjFldInfo
      (Just description)
      (unActionName actionName)
      (mapFromL _iviName $ map mkActionArgument $ _adArguments definition) $
      actionFieldResponseType actionName definition

    mkActionArgument argument =
      InpValInfo (_argDescription argument) (unArgumentName $ _argName argument)
      Nothing $ unGraphQLType $ _argType argument

actionFieldResponseType :: ActionName -> ActionDefinition a -> G.GType
actionFieldResponseType actionName definition =
  case getActionKind definition of
    ActionSynchronous  -> unGraphQLType $ _adOutputType definition
    ActionAsynchronous -> G.toGT $ G.toGT $ mkActionSelectionType actionName

mkQueryField
  :: ActionName
  -> ResolvedActionDefinition
  -> ActionPermissionInfo
  -> [(PGCol, PGScalarType)]
  -> Maybe (ActionSelectOpContext, ObjFldInfo, TypeInfo)
mkQueryField actionName definition permission definitionList =
  case getActionKind definition of
    ActionAsynchronous ->
      Just ( ActionSelectOpContext (_apiFilter permission) definitionList
           , fieldInfo
           , TIObj $ mkActionResponseTypeInfo actionName $
             _adOutputType definition
           )
    ActionSynchronous -> Nothing
  where
    -- TODO: we need to capture the comment from action definition
    description =
      G.Description $ "retrieve the result of action: " <>> actionName

    idArgument =
      InpValInfo (Just idDescription) "id" Nothing $ G.toNT $ mkScalarTy PGUUID
      where
        idDescription = G.Description $ "id of the action: " <>> actionName

    fieldInfo =
      mkHsraObjFldInfo
      (Just description)
      (unActionName actionName)
      (mapFromL _iviName [idArgument])
      (actionFieldResponseType actionName definition)

mkActionFieldsAndTypes
  :: (QErrM m)
  => ActionInfo
  -> AnnotatedObjectType
  -> ActionPermissionInfo
  -> m ( Maybe (ActionSelectOpContext, ObjFldInfo, TypeInfo)
       -- context, field, response type info
     , (ActionExecutionContext, ObjFldInfo) -- mutation field
     , FieldMap
     )
mkActionFieldsAndTypes actionInfo annotatedOutputType permission =
  return ( mkQueryField actionName definition permission definitionList
         , mkMutationField actionName actionInfo permission definitionList
         -- , maybe mempty mkFieldMap annotatedOutputTypeM
         , fieldMap
         )
  where
    actionName = _aiName actionInfo
    definition = _aiDefinition actionInfo
    roleName = _apiRole permission

    -- all the possible field references
    fieldReferences =
      Map.unions $ map _orFieldMapping $ Map.elems $
      _aotRelationships annotatedOutputType

    mkPGFieldType fieldName (fieldType, fieldTypeInfo) =
      case (G.isListType fieldType, fieldTypeInfo) of
        -- for scalar lists, we treat them as json columns
        (True, _) -> PGJSON
        -- enums the same
        (False, OutputFieldEnum _) -> PGJSON
        -- default to PGJSON unless you have to join with a postgres table
        -- i.e, if this field is specified as part of some relationship's
        -- mapping, we can cast this column's value as the remote column's type
        (False, OutputFieldScalar _) ->
          case Map.lookup fieldName fieldReferences of
            Just columnInfo -> unsafePGColumnToRepresentation $ pgiType columnInfo
            Nothing         -> PGJSON

    definitionList =
      [ (unsafePGCol $ coerce k, mkPGFieldType k v)
      | (k, v) <- Map.toList $ _aotAnnotatedFields annotatedOutputType
      ]
    -- mkFieldMap annotatedOutputType =
    fieldMap =
      Map.fromList $ fields <> catMaybes relationships
      where
        fields =
          flip map (Map.toList $ _aotAnnotatedFields annotatedOutputType) $
          \(fieldName, (fieldType, fieldTypeInfo)) ->
            ( (actionOutputBaseType, unObjectFieldName fieldName)
            , RFPGColumn $ PGColumnInfo
              (unsafePGCol $ coerce fieldName)
              (coerce fieldName)
              (PGColumnScalar $ mkPGFieldType fieldName (fieldType, fieldTypeInfo))
              (G.isNullable fieldType)
              Nothing
            )
        relationships =
          flip map (Map.toList $ _aotRelationships annotatedOutputType) $
          \(relationshipName, relationship) ->
            let remoteTableInfo = _orRemoteTable relationship
                remoteTable = remoteTableInfo ^. tiCoreInfo.tciName
                filterAndLimitM = getFilterAndLimit remoteTableInfo
                columnMapping = Map.fromList
                  [ (unsafePGCol $ coerce k, pgiColumn v)
                  | (k, v) <- Map.toList $ _orFieldMapping relationship
                  ]
            in case filterAndLimitM of
              Just (tableFilter, tableLimit) ->
                Just ( ( actionOutputBaseType
                       , unObjectRelationshipName relationshipName
                       )
                     , RFRelationship $ RelationshipField
                       (RelInfo
                        (RelName $ mkNonEmptyTextUnsafe $ coerce relationshipName)
                        ObjRel
                        columnMapping remoteTable True)
                       False mempty
                       tableFilter
                       tableLimit
                     )
              Nothing -> Nothing
    getFilterAndLimit remoteTableInfo =
      if roleName == adminRole
      then Just (annBoolExpTrue, Nothing)
      else do
        selectPermisisonInfo <-
          getSelectPermissionInfoM remoteTableInfo roleName
        return (spiFilter selectPermisisonInfo, spiLimit selectPermisisonInfo)
    actionOutputBaseType =
      G.getBaseType $ unGraphQLType $ _adOutputType $ _aiDefinition actionInfo

mkActionSchemaOne
  :: (QErrM m)
  => AnnotatedObjects
  -> ActionInfo
  -> m (Map.HashMap RoleName
         ( Maybe (ActionSelectOpContext, ObjFldInfo, TypeInfo)
         , (ActionExecutionContext, ObjFldInfo)
         , FieldMap
         )
       )
mkActionSchemaOne annotatedObjects actionInfo = do
  -- annotatedOutputTypeM <- case _aiOutputTypeInfo actionInfo of
  --   ActionOutputObject _ ->
  -- annotatedOutputTypeM <- fmap Just $ onNothing
  annotatedOutputType <- onNothing
      (Map.lookup (ObjectTypeName actionOutputBaseType) annotatedObjects) $
      throw500 $ "missing annotated type for: " <> showNamedTy actionOutputBaseType
    -- _ -> return Nothing
  forM permissions $ \permission ->
    mkActionFieldsAndTypes actionInfo annotatedOutputType permission
  where
    adminPermission = ActionPermissionInfo adminRole annBoolExpTrue
    permissions = Map.insert adminRole adminPermission $ _aiPermissions actionInfo
    actionOutputBaseType =
      G.getBaseType $ unGraphQLType $ _adOutputType $ _aiDefinition actionInfo

mkActionsSchema
  :: (QErrM m)
  => AnnotatedObjects
  -> ActionCache
  -> m (Map.HashMap RoleName (RootFields, TyAgg))
mkActionsSchema annotatedObjects =
  foldM
  (\aggregate actionInfo ->
     Map.foldrWithKey f aggregate <$>
     mkActionSchemaOne annotatedObjects actionInfo
  )
  mempty
  where
    -- we'll need to add uuid and timestamptz for actions
    newRoleState = (mempty, addScalarToTyAgg PGJSON $
                            addScalarToTyAgg PGTimeStampTZ $
                            addScalarToTyAgg PGUUID mempty)
    f roleName (queryFieldM, mutationField, fields) =
      Map.alter (Just . addToState . fromMaybe newRoleState) roleName
      where
        addToState = case queryFieldM of
          Just (fldCtx, fldDefinition, responseTypeInfo) ->
            addToStateAsync (fldCtx, fldDefinition) responseTypeInfo
          Nothing -> addToStateSync
        addToStateSync (rootFields, tyAgg) =
          ( addMutationField (first MCAction mutationField) rootFields
          , addFieldsToTyAgg fields tyAgg
          )
        addToStateAsync queryField responseTypeInfo (rootFields, tyAgg) =
          ( addMutationField (first MCAction mutationField) $
            addQueryField
            (first QCActionFetch queryField)
            rootFields
          , addTypeInfoToTyAgg responseTypeInfo $
            addFieldsToTyAgg fields tyAgg
          )
