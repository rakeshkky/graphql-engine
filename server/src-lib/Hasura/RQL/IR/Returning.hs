module Hasura.RQL.IR.Returning where

import           Hasura.Prelude

import qualified Data.Aeson                       as J
import qualified Data.HashMap.Strict.InsOrd       as OMap

import qualified Hasura.Backends.Postgres.SQL.DML as S

import           Hasura.EncJSON
import           Hasura.RQL.IR.Select
import           Hasura.SQL.Backend


data MutFldG (b :: Backend) v
  = MCount
  | MExp !Text
  | MRet !(AnnFieldsG b v)

type MutFld b = MutFldG b S.SQLExp

type MutFldsG b v = Fields (MutFldG b v)

data MutationOutputG (b :: Backend) v
  = MOutMultirowFields !(MutFldsG b v)
  | MOutSinglerowObject !(AnnFieldsG b v)

type MutationOutput b = MutationOutputG b S.SQLExp

type MutFlds b = MutFldsG b S.SQLExp

buildEmptyMutResp :: MutationOutput backend -> EncJSON
buildEmptyMutResp = \case
  MOutMultirowFields mutFlds -> encJFromJValue $ OMap.fromList $ map (second convMutFld) mutFlds
  MOutSinglerowObject _      -> encJFromJValue $ J.Object mempty
  where
    convMutFld = \case
      MCount -> J.toJSON (0 :: Int)
      MExp e -> J.toJSON e
      MRet _ -> J.toJSON ([] :: [J.Value])

traverseMutFld
  :: (Applicative f)
  => (a -> f b)
  -> MutFldG backend a
  -> f (MutFldG backend b)
traverseMutFld f = \case
  MCount    -> pure MCount
  MExp t    -> pure $ MExp t
  MRet flds -> MRet <$> traverse (traverse (traverseAnnField f)) flds

traverseMutationOutput
  :: (Applicative f)
  => (a -> f b)
  -> MutationOutputG backend a -> f (MutationOutputG backend b)
traverseMutationOutput f = \case
  MOutMultirowFields mutationFields ->
    MOutMultirowFields <$> traverse (traverse (traverseMutFld f)) mutationFields
  MOutSinglerowObject annFields ->
    MOutSinglerowObject <$> traverseAnnFields f annFields

traverseMutFlds
  :: (Applicative f)
  => (a -> f b)
  -> MutFldsG backend a
  -> f (MutFldsG backend b)
traverseMutFlds f =
  traverse (traverse (traverseMutFld f))

hasNestedFld :: MutationOutputG backend a -> Bool
hasNestedFld = \case
  MOutMultirowFields flds     -> any isNestedMutFld flds
  MOutSinglerowObject annFlds -> any isNestedAnnField annFlds
  where
    isNestedMutFld (_, mutFld) = case mutFld of
      MRet annFlds -> any isNestedAnnField annFlds
      _            -> False
    isNestedAnnField (_, annFld) = case annFld of
      AFObjectRelation _ -> True
      AFArrayRelation _  -> True
      _                  -> False

data MutationCTE
  = MCPermissionCheck !S.CTE
  | MCNoPermissionCheck !S.CTE
  deriving (Show, Eq)

getMutationCTE :: MutationCTE -> S.CTE
getMutationCTE = \case
  MCPermissionCheck c   -> c
  MCNoPermissionCheck c -> c

checkPermissionRequired :: MutationCTE -> Bool
checkPermissionRequired = \case
  MCPermissionCheck _   -> True
  MCNoPermissionCheck _ -> False
