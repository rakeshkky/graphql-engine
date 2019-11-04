module Hasura.RQL.DML.Mutation
  ( Mutation(..)
  , runMutation
  , mutateAndFetchCols
  , mkSelCTEFromColumnVals
  )
where

import           Hasura.Prelude

import qualified Data.Aeson.Extended      as J
import qualified Data.HashMap.Strict      as Map
import qualified Data.Sequence            as DS
import qualified Database.PG.Query        as Q

import qualified Hasura.SQL.DML           as S

import           Hasura.EncJSON
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Returning
import           Hasura.RQL.DML.Select
import           Hasura.RQL.Instances     ()
import           Hasura.RQL.Types
import           Hasura.SQL.Types

data Mutation
  = Mutation
  { _mTable    :: !QualifiedTable
  , _mQuery    :: !(S.CTE, DS.Seq Q.PrepArg)
  , _mFields   :: !MutFlds
  , _mCols     :: ![PGColumnInfo]
  , _mStrfyNum :: !Bool
  } deriving (Show, Eq)

runMutation :: Mutation -> Q.TxE QErr EncJSON
runMutation mut =
  bool (mutateAndReturn mut) (mutateAndSel mut) $
    hasNestedFld $ _mFields mut

mutateAndReturn :: Mutation -> Q.TxE QErr EncJSON
mutateAndReturn (Mutation qt (cte, p) mutFlds _ strfyNum) =
  encJFromBS . runIdentity . Q.getRow
    <$> Q.rawQE dmlTxErrorHandler (Q.fromBuilder $ toSQL selWith)
        (toList p) True
  where
    selWith = mkSelWith qt cte mutFlds False strfyNum

mutateAndSel :: Mutation -> Q.TxE QErr EncJSON
mutateAndSel (Mutation qt q mutFlds allCols strfyNum) = do
  -- Perform mutation and fetch unique columns
  MutateResp _ columnVals <- mutateAndFetchCols qt allCols q strfyNum
  let selCTE = mkSelCTEFromColumnVals allCols columnVals
      selWith = mkSelWith qt selCTE mutFlds False strfyNum
  -- Perform select query and fetch returning fields
  encJFromBS . runIdentity . Q.getRow
    <$> Q.rawQE dmlTxErrorHandler (Q.fromBuilder $ toSQL selWith) [] True


mutateAndFetchCols
  :: QualifiedTable
  -> [PGColumnInfo]
  -> (S.CTE, DS.Seq Q.PrepArg)
  -> Bool
  -> Q.TxE QErr MutateResp
mutateAndFetchCols qt cols (cte, p) strfyNum =
  Q.getAltJ . runIdentity . Q.getRow
    <$> Q.rawQE dmlTxErrorHandler (Q.fromBuilder sql) (toList p) True
  where
    aliasIden = Iden $ qualObjectToText qt <> "__mutation_result"
    tabFrom = FromIden aliasIden
    tabPerm = TablePerm annBoolExpTrue Nothing
    selFlds = flip map cols $
              \ci -> (fromPGCol $ pgiColumn ci, FCol ci Nothing)

    sql = toSQL selectWith
    selectWith = S.SelectWith [(S.Alias aliasIden, cte)] select
    select = S.mkSelect {S.selExtr = [S.Extractor extrExp Nothing]}
    extrExp = S.applyJsonBuildObj
              [ S.SELit "affected_rows", affRowsSel
              , S.SELit "returning_columns", colSel
              ]

    affRowsSel = S.SESelect $
      S.mkSelect
      { S.selExtr = [S.Extractor S.countStar Nothing]
      , S.selFrom = Just $ S.FromExp [S.FIIden aliasIden]
      }
    colSel = S.SESelect $ mkSQLSelect False $
             AnnSelG selFlds tabFrom tabPerm noTableArgs strfyNum

mkSelCTEFromColumnVals :: [PGColumnInfo] -> [ColVals] -> S.CTE
mkSelCTEFromColumnVals allCols columnVals =
  S.CTESelect S.mkSelect
        { S.selExtr = [S.selectStar]
        , S.selFrom = Just $ S.FromExp [selectFrom]
        }
  where
    selectFrom =
      let jsonToRecordsetFunction =
            QualifiedObject (SchemaName "pg_catalog") (FunctionName "json_to_recordset")

          columnValuesJson = S.withTyAnn PGJSON $
                             S.SELit $ J.encodeToStrictText $ reverse columnVals

          functionArgs = S.FunctionArgs [columnValuesJson] Map.empty

          definitions = flip map allCols $ \pci ->
                        S.ColumnDefinitionItem (pgiColumn pci)
                        (toPGScalarType $ pgiType pci)

          functionAlias = S.FunctionAlias (S.Alias $ Iden "json_to_recordset_row")
                          $ Just definitions

      in S.mkFunctionFromItem jsonToRecordsetFunction functionArgs $ Just functionAlias

    toPGScalarType (PGColumnScalar scalar) =
      if isGeoType scalar then PGJSON else scalar
    toPGScalarType (PGColumnEnumReference _) = PGText
