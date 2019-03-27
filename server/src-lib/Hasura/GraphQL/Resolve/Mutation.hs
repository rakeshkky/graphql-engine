module Hasura.GraphQL.Resolve.Mutation
  ( convertUpdate
  , convertDelete
  , convertMutResp
  , buildEmptyMutResp
  ) where

import           Data.Has
import           Hasura.Prelude

import qualified Data.Aeson                        as J
import qualified Data.HashMap.Strict               as Map
import qualified Data.HashMap.Strict.InsOrd        as OMap
import qualified Language.GraphQL.Draft.Syntax     as G

import qualified Hasura.RQL.DML.Delete             as RD
import qualified Hasura.RQL.DML.Returning          as RR
import qualified Hasura.RQL.DML.Update             as RU

import qualified Hasura.SQL.DML                    as S

import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Resolve.BoolExp
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.InputValue
import           Hasura.GraphQL.Resolve.Select     (fromSelSet, withSelSet)
import           Hasura.GraphQL.Validate.Field
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

convertMutResp
  :: ( MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r, Has QueryResolver r
     )
  => G.NamedType -> SelSet -> m RR.MutFlds
convertMutResp ty selSet =
  withSelSet selSet $ \fld -> case _fName fld of
    "__typename"    -> return $ RR.MExp $ G.unName $ G.unNamedType ty
    "affected_rows" -> return RR.MCount
    "returning"     -> fmap RR.MRet $
                       fromSelSet txtConverter (_fType fld) $ _fSelSet fld
    "query"         -> do
      resolver <- asks getter
      return $ RR.MQuery $ getQueryResolver resolver $ _fSelSet fld
    G.Name t        -> throw500 $ "unexpected field in mutation resp : " <> t

convertRowObj
  :: (MonadError QErr m, MonadState PrepArgs m)
  => AnnInpVal
  -> m [(PGCol, S.SQLExp)]
convertRowObj val =
  flip withObject val $ \_ obj ->
  forM (OMap.toList obj) $ \(k, v) -> do
    prepExpM <- asPGColValM v >>= mapM prepare
    let prepExp = fromMaybe (S.SEUnsafe "NULL") prepExpM
    return (PGCol $ G.unName k, prepExp)

type ApplySQLOp =  (PGCol, S.SQLExp) -> S.SQLExp

rhsExpOp :: S.SQLOp -> S.AnnType -> ApplySQLOp
rhsExpOp op annTy (col, e) =
  S.mkSQLOpExp op (S.SEIden $ toIden col) annExp
  where
    annExp = S.SETyAnn e annTy

lhsExpOp :: S.SQLOp -> S.AnnType -> ApplySQLOp
lhsExpOp op annTy (col, e) =
  S.mkSQLOpExp op annExp $ S.SEIden $ toIden col
  where
    annExp = S.SETyAnn e annTy

convObjWithOp
  :: (MonadError QErr m)
  => ApplySQLOp -> AnnInpVal -> m [(PGCol, S.SQLExp)]
convObjWithOp opFn val =
  flip withObject val $ \_ obj -> forM (OMap.toList obj) $ \(k, v) -> do
  colVal <- _apvValue <$> asPGColVal v
  let pgCol = PGCol $ G.unName k
      encVal = txtEncoder colVal
      sqlExp = opFn (pgCol, encVal)
  return (pgCol, sqlExp)

convDeleteAtPathObj
  :: (MonadError QErr m)
  => AnnInpVal -> m [(PGCol, S.SQLExp)]
convDeleteAtPathObj val =
  flip withObject val $ \_ obj -> forM (OMap.toList obj) $ \(k, v) -> do
    vals <- flip withArray v $ \_ annVals -> mapM asPGColVal annVals
    let valExps = map (txtEncoder . _apvValue) vals
        pgCol = PGCol $ G.unName k
        annEncVal = S.SETyAnn (S.SEArray valExps) S.textArrType
        sqlExp = S.SEOpApp S.jsonbDeleteAtPathOp
                 [S.SEIden $ toIden pgCol, annEncVal]
    return (pgCol, sqlExp)

convertUpdateP1
  :: ( MonadError QErr m
     , MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     , Has QueryResolver r
     , MonadState PrepArgs m
     )
  => UpdOpCtx -- the update context
  -> Field -- the mutation field
  -> m RU.UpdateQueryP1
convertUpdateP1 opCtx fld = do
  -- a set expression is same as a row object
  setExpM   <- withArgM args "_set" convertRowObj
  -- where bool expression to filter column
  whereExp <- withArg args "where" (parseBoolExp prepare)
  -- increment operator on integer columns
  incExpM <- withArgM args "_inc" $
    convObjWithOp $ rhsExpOp S.incOp S.intType
  -- append jsonb value
  appendExpM <- withArgM args "_append" $
    convObjWithOp $ rhsExpOp S.jsonbConcatOp S.jsonbType
  -- prepend jsonb value
  prependExpM <- withArgM args "_prepend" $
    convObjWithOp $ lhsExpOp S.jsonbConcatOp S.jsonbType
  -- delete a key in jsonb object
  deleteKeyExpM <- withArgM args "_delete_key" $
    convObjWithOp $ rhsExpOp S.jsonbDeleteOp S.textType
  -- delete an element in jsonb array
  deleteElemExpM <- withArgM args "_delete_elem" $
    convObjWithOp $ rhsExpOp S.jsonbDeleteOp S.intType
  -- delete at path in jsonb value
  deleteAtPathExpM <- withArgM args "_delete_at_path" convDeleteAtPathObj
  mutFlds  <- convertMutResp (_fType fld) $ _fSelSet fld
  let updExpsM = [ setExpM, incExpM, appendExpM, prependExpM
                 , deleteKeyExpM, deleteElemExpM, deleteAtPathExpM
                 ]
      setItems = preSetItems ++ concat (catMaybes updExpsM)

  -- atleast one of update operators is expected
  -- or preSetItems shouldn't be empty
  -- this is not equivalent to (null setItems)
  unless (any isJust updExpsM || not (null preSetItems)) $ throwVE $
    "atleast any one of _set, _inc, _append, _prepend, "
    <> "_delete_key, _delete_elem and "
    <> "_delete_at_path operator is expected"

  return $ RU.UpdateQueryP1 tn setItems (filterExp, whereExp) mutFlds allCols
  where
    UpdOpCtx tn _ filterExp preSetCols allCols = opCtx
    args = _fArguments fld
    preSetItems = Map.toList preSetCols

convertUpdate
  :: UpdOpCtx -- the update context
  -> Field -- the mutation field
  -> Convert RespTx
convertUpdate opCtx fld = do
  (p1, prepArgs) <- withPrepArgs $ convertUpdateP1 opCtx fld
  strfyNum <- stringifyNum <$> asks getter
  let whenNonEmptyItems = return $ RU.updateQueryToTx strfyNum (p1, prepArgs)
      whenEmptyItems    = buildEmptyMutResp $ RU.uqp1MutFlds p1
   -- if there are not set items then do not perform
   -- update and return empty mutation response
  bool whenNonEmptyItems whenEmptyItems $ null $ RU.uqp1SetExps p1

convertDelete
  :: DelOpCtx -- the delete context
  -> Field -- the mutation field
  -> Convert RespTx
convertDelete opCtx fld = do
  (p1, prepArgs) <- p1m
  strfyNum <- stringifyNum <$> asks getter
  return $ RD.deleteQueryToTx strfyNum (p1, prepArgs)
  where
    DelOpCtx tn _ filterExp allCols = opCtx
    p1m = withPrepArgs $ do
      whereExp <- withArg (_fArguments fld) "where" (parseBoolExp prepare)
      mutFlds  <- convertMutResp (_fType fld) $ _fSelSet fld
      return $ RD.DeleteQueryP1 tn (filterExp, whereExp) mutFlds allCols

-- | build mutation response for empty objects
buildEmptyMutResp :: Monad m => RR.MutFlds -> m RespTx
buildEmptyMutResp mutFlds =
  return $ do
    mutResults <- mkMutResults
    return $ encJFromAssocList mutResults
  where
    -- generate empty mutation response
    mkMutResults = forM mutFlds $ \(t, fld) -> (t,) <$>
      case fld of
        RR.MCount     -> return $ encJFromJValue (0 :: Int)
        RR.MExp e     -> return $ encJFromJValue e
        RR.MRet _     -> return $ encJFromJValue ([] :: [J.Value])
        RR.MQuery qTx -> qTx
