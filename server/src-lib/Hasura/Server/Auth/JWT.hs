module Hasura.Server.Auth.JWT
  ( processJwt
  , RawJWT
  , JWTConfig (..)
  , JWTCtx (..)
  , Jose.JWKSet (..)
  , JWTClaimsFormat (..)
  , JWTClaims(..)
  , JWTClaimsMap
  , JwkFetchError (..)
  , updateJwkRef
  , jwkRefreshCtrl
  , defaultClaimsFormat
  , defaultClaimsNamespace
  ) where

import           Control.Exception               (try)
import           Control.Lens
import           Control.Monad                   (when)

import           Control.Monad.Trans.Maybe
import           Data.IORef                      (IORef, readIORef, writeIORef)
import           Data.List                       (find)
import           Data.Parser.JSONPath            (parseJSONPath)
import           Data.Time.Clock                 (NominalDiffTime, UTCTime, diffUTCTime,
                                                  getCurrentTime)
import           GHC.AssertNF
import           Network.URI                     (URI)

import           Data.Parser.CacheControl
import           Data.Parser.Expires
import           Hasura.HTTP
import           Hasura.Logging                  (Hasura, LogLevel (..), Logger (..))
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Auth.JWT.Internal (parseHmacKey, parseRsaKey)
import           Hasura.Server.Auth.JWT.Logging
import           Hasura.Server.Utils             (executeJSONPath, getRequestHeader, userRoleHeader)
import           Hasura.Server.Version           (HasVersion)

import qualified Control.Concurrent.Extended     as C
import qualified Crypto.JWT                      as Jose
import qualified Data.Aeson                      as J
import qualified Data.Aeson.Casing               as J
import qualified Data.Aeson.Internal             as J
import qualified Data.Aeson.TH                   as J
import qualified Data.Aeson.Types                as J
import qualified Data.ByteString.Lazy            as BL
import qualified Data.ByteString.Lazy.Char8      as BLC
import qualified Data.CaseInsensitive            as CI
import qualified Data.HashMap.Strict             as Map
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Network.HTTP.Client             as HTTP
import qualified Network.HTTP.Types              as HTTP
import qualified Network.Wreq                    as Wreq

newtype RawJWT = RawJWT BL.ByteString

data JWTClaimsFormat
  = JCFJson
  | JCFStringifiedJson
  deriving (Show, Eq)

$(J.deriveJSON J.defaultOptions { J.sumEncoding = J.ObjectWithSingleField
                                , J.constructorTagModifier = J.snakeCase . drop 3 } ''JWTClaimsFormat)

defaultClaimsFormat :: JWTClaimsFormat
defaultClaimsFormat = JCFJson

allowedRolesClaim :: T.Text
allowedRolesClaim = "x-hasura-allowed-roles"

defaultRoleClaim :: T.Text
defaultRoleClaim = "x-hasura-default-role"

isXHasuraPrefix :: T.Text -> Bool
isXHasuraPrefix = T.isPrefixOf "x-hasura-" . T.toLower

defaultClaimsNamespace :: Text
defaultClaimsNamespace = "https://hasura.io/jwt/claims"

type CustomClaimsMap = Map.HashMap Text J.JSONPath

data JWTClaimsMap
  = JWTClaimsMap
  { jcmDefaultRole  :: !J.JSONPath
  , jcmAllowedRoles :: !J.JSONPath
  , jcmCustomClaims :: !CustomClaimsMap
  } deriving (Show, Eq)

instance J.FromJSON JWTClaimsMap where
  parseJSON = J.withObject "Object" $ \obj -> do
    let onlyXHasuraTuples = filter (isXHasuraPrefix . fst) $ Map.toList obj
    jsonPathTuples <- forM onlyXHasuraTuples $
      \(t, val) -> flip (J.<?>) (J.Key t) $ (T.toLower t,) <$> case val of
                         J.String s -> either fail pure $ parseJSONPath s
                         _          -> fail "expecting String for JSONPath"

    let withParseError l = maybe (fail $ T.unpack $ l <> " is expected but not found")
                           pure $ Map.lookup l $ Map.fromList jsonPathTuples
        customClaims = Map.fromList $
                       filter (not . flip elem [allowedRolesClaim, defaultRoleClaim] . fst) jsonPathTuples
    JWTClaimsMap <$> withParseError defaultRoleClaim <*> withParseError allowedRolesClaim <*> pure customClaims

instance J.ToJSON JWTClaimsMap where
  toJSON (JWTClaimsMap defaultRole allowedRoles customClaims) =
    let encodeJSONPath' = T.pack . encodeJSONPath
    in J.Object $
       Map.fromList $ [ (defaultRoleClaim, J.String $ encodeJSONPath' defaultRole)
                      , (allowedRolesClaim, J.String $ encodeJSONPath' allowedRoles)
                      ] <> map (second (J.String . encodeJSONPath')) (Map.toList customClaims)

data JWTClaims
  = JCNamespace !Text !JWTClaimsFormat
  | JCMap !JWTClaimsMap
  deriving (Show, Eq)

data JWTConfig
  = JWTConfig
  { jcType     :: !T.Text
  , jcKeyOrUrl :: !(Either Jose.JWK URI)
  , jcAudience :: !(Maybe Jose.Audience)
  , jcIssuer   :: !(Maybe Jose.StringOrURI)
  , jcClaims   :: !JWTClaims
  } deriving (Show, Eq)

data JWTCtx
  = JWTCtx
  { jcxKey      :: !(IORef Jose.JWKSet)
  , jcxAudience :: !(Maybe Jose.Audience)
  , jcxIssuer   :: !(Maybe Jose.StringOrURI)
  , jcxClaims   :: !JWTClaims
  } deriving (Eq)

instance Show JWTCtx where
  show (JWTCtx _ audM iss claims) =
    show ["<IORef JWKSet>", show audM, show iss, show claims]

data HasuraClaims
  = HasuraClaims
  { _cmAllowedRoles :: ![RoleName]
  , _cmDefaultRole  :: !RoleName
  } deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''HasuraClaims)

-- | An action that refreshes the JWK at intervals in an infinite loop.
jwkRefreshCtrl
  :: HasVersion
  => Logger Hasura
  -> HTTP.Manager
  -> URI
  -> IORef Jose.JWKSet
  -> DiffTime
  -> IO void
jwkRefreshCtrl logger manager url ref time = liftIO $ do
    C.sleep time
    forever $ do
      res <- runExceptT $ updateJwkRef logger manager url ref
      mTime <- either (const $ logNotice >> return Nothing) return res
      -- if can't parse time from header, defaults to 1 min
      let delay = maybe (minutes 1) fromUnits mTime
      C.sleep delay
  where
    logNotice = do
      let err = JwkRefreshLog LevelInfo (Just "retrying again in 60 secs") Nothing
      liftIO $ unLogger logger err

-- | Given a JWK url, fetch JWK from it and update the IORef
updateJwkRef
  :: ( HasVersion
     , MonadIO m
     , MonadError JwkFetchError m
     )
  => Logger Hasura
  -> HTTP.Manager
  -> URI
  -> IORef Jose.JWKSet
  -> m (Maybe NominalDiffTime)
updateJwkRef (Logger logger) manager url jwkRef = do
  let options = wreqOptions manager []
      urlT    = T.pack $ show url
      infoMsg = "refreshing JWK from endpoint: " <> urlT
  liftIO $ logger $ JwkRefreshLog LevelInfo (Just infoMsg) Nothing
  res  <- liftIO $ try $ Wreq.getWith options $ show url
  resp <- either logAndThrowHttp return res
  let status = resp ^. Wreq.responseStatus
      respBody = resp ^. Wreq.responseBody
      statusCode = status ^. Wreq.statusCode

  unless (statusCode >= 200 && statusCode < 300) $ do
    let errMsg = "Non-2xx response on fetching JWK from: " <> urlT
        err = JFEHttpError url status respBody errMsg
    logAndThrow err

  let parseErr e = JFEJwkParseError (T.pack e) $ "Error parsing JWK from url: " <> urlT
  !jwkset <- either (logAndThrow . parseErr) return $ J.eitherDecode' respBody
  liftIO $ do
    $assertNFHere jwkset  -- so we don't write thunks to mutable vars
    writeIORef jwkRef jwkset

  -- first check for Cache-Control header to get max-age, if not found, look for Expires header
  runMaybeT $ timeFromCacheControl resp <|> timeFromExpires resp

  where
    parseCacheControlErr e =
      JFEExpiryParseError (Just e)
      "Failed parsing Cache-Control header from JWK response. Could not find max-age or s-maxage"
    parseTimeErr =
      JFEExpiryParseError Nothing
      "Failed parsing Expires header from JWK response. Value of header is not a valid timestamp"

    timeFromCacheControl resp = do
      header <- afold $ bsToTxt <$> resp ^? Wreq.responseHeader "Cache-Control"
      fromInteger <$> parseMaxAge header `onLeft` \err -> logAndThrowInfo $ parseCacheControlErr $ T.pack err
    timeFromExpires resp = do
      header <- afold $ bsToTxt <$> resp ^? Wreq.responseHeader "Expires"
      expiry <- parseExpirationTime header `onLeft` const (logAndThrowInfo parseTimeErr)
      diffUTCTime expiry <$> liftIO getCurrentTime

    logAndThrowInfo :: (MonadIO m, MonadError JwkFetchError m) => JwkFetchError -> m a
    logAndThrowInfo err = do
      liftIO $ logger $ JwkRefreshLog LevelInfo Nothing (Just err)
      throwError err

    logAndThrow :: (MonadIO m, MonadError JwkFetchError m) => JwkFetchError -> m a
    logAndThrow err = do
      liftIO $ logger $ JwkRefreshLog (LevelOther "critical") Nothing (Just err)
      throwError err

    logAndThrowHttp :: (MonadIO m, MonadError JwkFetchError m) => HTTP.HttpException -> m a
    logAndThrowHttp httpEx = do
      let errMsg = "Error fetching JWK: " <> T.pack (getHttpExceptionMsg httpEx)
          err = JFEHttpException (HttpException httpEx) errMsg
      logAndThrow err

    getHttpExceptionMsg = \case
      HTTP.HttpExceptionRequest _ reason -> show reason
      HTTP.InvalidUrlException _ reason -> show reason


-- | Process the request headers to verify the JWT and extract UserInfo from it
processJwt
  :: ( MonadIO m
     , MonadError QErr m)
  => JWTCtx
  -> HTTP.RequestHeaders
  -> Maybe RoleName
  -> m (UserInfo, Maybe UTCTime)
processJwt jwtCtx headers mUnAuthRole =
  maybe withoutAuthZHeader withAuthZHeader mAuthZHeader
  where
    mAuthZHeader = find (\h -> fst h == CI.mk "Authorization") headers

    withAuthZHeader (_, authzHeader) =
      processAuthZHeader jwtCtx headers $ BL.fromStrict authzHeader

    withoutAuthZHeader = do
      unAuthRole <- maybe missingAuthzHeader return mUnAuthRole
      return $ (, Nothing) $
        mkUserInfo unAuthRole $ mkUserVars $ hdrsToText headers

    missingAuthzHeader =
      throw400 InvalidHeaders "Missing Authorization header in JWT authentication mode"

processAuthZHeader
  :: ( MonadIO m
     , MonadError QErr m)
  => JWTCtx
  -> HTTP.RequestHeaders
  -> BLC.ByteString
  -> m (UserInfo, Maybe UTCTime)
processAuthZHeader jwtCtx headers authzHeader = do
  -- try to parse JWT token from Authorization header
  jwt <- parseAuthzHeader

  -- verify the JWT
  claims <- liftJWTError invalidJWTError $ verifyJwt jwtCtx $ RawJWT jwt
  let expTimeM = fmap (\(Jose.NumericDate t) -> t) $ claims ^. Jose.claimExp

  (HasuraClaims allowedRoles defaultRole, otherClaims) <-
    parseHasuraClaims (claims ^. Jose.unregisteredClaims) claimsConfig

  let role = getCurrentRole defaultRole
  when (role `notElem` allowedRoles) currRoleNotAllowed
  return $ (, expTimeM) $ mkUserInfo role $ mkUserVars $ Map.toList otherClaims

  where
    claimsConfig = jcxClaims jwtCtx
    parseAuthzHeader = do
      let tokenParts = BLC.words authzHeader
      case tokenParts of
        ["Bearer", jwt] -> return jwt
        _               -> malformedAuthzHeader

    -- see if there is a x-hasura-role header, or else pick the default role
    getCurrentRole defaultRole =
      let mUserRole = getRequestHeader userRoleHeader headers
      in maybe defaultRole RoleName $ mUserRole >>= mkNonEmptyText . bsToTxt

    liftJWTError :: (MonadError e' m) => (e -> e') -> ExceptT e m a -> m a
    liftJWTError ef action = do
      res <- runExceptT action
      either (throwError . ef) return res

    invalidJWTError e =
      err400 JWTInvalid $ "Could not verify JWT: " <> T.pack (show e)

    malformedAuthzHeader =
      throw400 InvalidHeaders "Malformed Authorization header"
    currRoleNotAllowed =
      throw400 AccessDenied "Your current role is not in allowed roles"


-- parse x-hasura-allowed-roles, x-hasura-default-role and other JWT claims
parseHasuraClaims
  :: (MonadError QErr m)
  => J.Object -- ^ Unregistered JWT claims
  -> JWTClaims -- ^ Claims config
  -> m (HasuraClaims, Map.HashMap Text Text) -- ^ Hasura claims and other claims
parseHasuraClaims unregisteredClaims = \case
  JCNamespace namespace claimsFormat -> do
    claimsV <- maybe (claimsNotFound namespace) pure $ Map.lookup namespace unregisteredClaims
    -- get hasura claims value as an object. parse from string possibly
    claimsObject <- Map.fromList . map (first T.toLower) . Map.toList <$>
                    parseObjectFromString namespace claimsFormat claimsV

    allowedRoles <- parseAllowedRolesClaim $ Map.lookup allowedRolesClaim claimsObject
    defaultRole <- parseDefaultRoleClaim $ Map.lookup defaultRoleClaim claimsObject

    otherClaims <- toClaimsTextMap $
      Map.delete defaultRoleClaim . Map.delete allowedRolesClaim $ claimsObject

    pure (HasuraClaims allowedRoles defaultRole, otherClaims)

  JCMap claimsConfig -> do
    let JWTClaimsMap defaultRoleJsonPath allowedRolesJsonPath otherClaimsMap = claimsConfig
        claimsObjValue = J.Object unregisteredClaims

    allowedRoles <- parseAllowedRolesClaim $ iResultToMaybe $
                    executeJSONPath allowedRolesJsonPath claimsObjValue

    defaultRole <- parseDefaultRoleClaim $ iResultToMaybe $
                   executeJSONPath defaultRoleJsonPath claimsObjValue

    otherClaimsObject <- flip Map.traverseWithKey otherClaimsMap $ \k path -> do
      let throwClaimErr = throw400 JWTInvalidClaims $ "JWT claim from claims_map, " <> k <> " not found"
      maybe throwClaimErr pure $ iResultToMaybe $ executeJSONPath path claimsObjValue

    otherClaims <- toClaimsTextMap otherClaimsObject

    pure (HasuraClaims allowedRoles defaultRole, otherClaims)

  where
    parseAllowedRolesClaim = \case
      Nothing -> throw400 JWTRoleClaimMissing $ "JWT claim does not contain " <> allowedRolesClaim
      Just v -> parseJwtClaim (J.fromJSON v) $ "invalid " <> allowedRolesClaim <>
                "; should be a list of roles"

    parseDefaultRoleClaim = \case
      Nothing -> throw400 JWTRoleClaimMissing $ "JWT claim does not contain " <> defaultRoleClaim
      Just v -> parseJwtClaim (J.fromJSON v) $ "invalid " <> defaultRoleClaim <>
                "; should be a role"

    parseJwtClaim res errMsg =
      case res of
        J.Success val -> return val
        J.Error _     -> throw400 JWTInvalidClaims errMsg

    toClaimsTextMap = Map.traverseWithKey $ \key value ->
      either (throw400 JWTInvalidClaims . T.pack) pure $ J.parseEither
      (J.modifyFailure ("x-hasura-* cliams: " <>) . J.withText (T.unpack key) pure) value

    claimsNotFound claimNs =
      throw400 JWTInvalidClaims $ "claims key: '" <> claimNs <> "' not found"

    parseObjectFromString claimNs claimsFmt jVal =
      case (claimsFmt, jVal) of
        (JCFStringifiedJson, J.String v) ->
          either (const $ claimsErr $ strngfyErr v) return
          $ J.eitherDecodeStrict $ T.encodeUtf8 v
        (JCFStringifiedJson, _) ->
          claimsErr "expecting a string when claims_format is stringified_json"
        (JCFJson, J.Object o) -> return o
        (JCFJson, _) ->
          claimsErr "expecting a json object when claims_format is json"
      where
        strngfyErr v = "expecting stringified json at: '" <> claimNs
                       <> "', but found: " <> v

        claimsErr = throw400 JWTInvalidClaims


-- | Verify the JWT against given JWK
verifyJwt
  :: ( MonadError Jose.JWTError m
     , MonadIO m
     )
  => JWTCtx
  -> RawJWT
  -> m Jose.ClaimsSet
verifyJwt ctx (RawJWT rawJWT) = do
  key <- liftIO $ readIORef $ jcxKey ctx
  jwt <- Jose.decodeCompact rawJWT
  t   <- liftIO getCurrentTime
  Jose.verifyClaimsAt config key t jwt
  where
    config = case jcxIssuer ctx of
      Nothing  -> Jose.defaultJWTValidationSettings audCheck
      Just iss -> Jose.defaultJWTValidationSettings audCheck
                  & set Jose.issuerPredicate (== iss)
    audCheck audience =
      -- dont perform the check if there are no audiences in the conf
      case jcxAudience ctx of
        Nothing                        -> True
        Just (Jose.Audience audiences) -> audience `elem` audiences


instance J.ToJSON JWTConfig where
  toJSON (JWTConfig ty keyOrUrl aud iss claims) =
    let keyOrUrlPair = case keyOrUrl of
          Left _    -> "key" J..= J.String "<JWK REDACTED>"
          Right url -> "jwk_url" J..= url
        claimsPairs = case claims of
          JCNamespace namespace claimsFormat ->
            [ "claims_namespace" J..= namespace
            , "claims_format" J..= claimsFormat
            ]
          JCMap claimsMap ->
            ["claims_map" J..= claimsMap]
    in J.object $ [ "type" J..= ty
                  , keyOrUrlPair
                  , "audience" J..= aud
                  , "issuer" J..= iss
                  ] <> claimsPairs

-- | Parse from a json string like:
-- | `{"type": "RS256", "key": "<PEM-encoded-public-key-or-X509-cert>"}`
-- | to JWTConfig
instance J.FromJSON JWTConfig where

  parseJSON = J.withObject "JWTConfig" $ \o -> do
    keyType <- o J..: "type"
    mRawKey <- o J..:? "key"
    claimNs <- o J..:? "claims_namespace" J..!= defaultClaimsNamespace
    aud     <- o J..:? "audience"
    iss     <- o J..:? "issuer"
    jwkUrl  <- o J..:? "jwk_url"
    claimsFormat <- o J..:? "claims_format" J..!= defaultClaimsFormat
    claimsMap <- o J..:? "claims_map"

    keyOrUrl <- case (mRawKey, jwkUrl) of
      (Nothing, Nothing) -> fail "key and jwk_url both cannot be empty"
      (Just _, Just _)   -> fail "key, jwk_url both cannot be present"
      (Just rawKey, Nothing) -> do
        key <- parseKey keyType rawKey
        pure $ Left key
      (Nothing, Just url) -> pure $ Right url

    pure $ JWTConfig keyType keyOrUrl aud iss $
           maybe (JCNamespace claimNs claimsFormat) JCMap claimsMap

    where
      parseKey keyType rawKey =
       case keyType of
          "HS256" -> runEither $ parseHmacKey rawKey 256
          "HS384" -> runEither $ parseHmacKey rawKey 384
          "HS512" -> runEither $ parseHmacKey rawKey 512
          "RS256" -> runEither $ parseRsaKey rawKey
          "RS384" -> runEither $ parseRsaKey rawKey
          "RS512" -> runEither $ parseRsaKey rawKey
          -- TODO: support ES256, ES384, ES512, PS256, PS384
          _       -> invalidJwk ("Key type: " <> T.unpack keyType <> " is not supported")

      runEither = either (invalidJwk . T.unpack) return
      invalidJwk msg = fail ("Invalid JWK: " <> msg)
