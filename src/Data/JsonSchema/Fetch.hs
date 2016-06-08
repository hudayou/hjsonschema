
module Data.JsonSchema.Fetch where

import           Import
-- Hiding is for GHCs before 7.10:
import           Prelude                  hiding (concat, sequence)

import           Control.Arrow            (left)
import           Control.Exception        (catch)
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.ByteString          as BS
import qualified Data.HashMap.Strict      as H
import qualified Data.Text                as T
import           Network.HTTP.Client

import           Data.Validator.Reference (resolveReference,
                                           updateResolutionScope)

--------------------------------------------------
-- * Types
--------------------------------------------------

-- | This is all the fetching functions need to know about a particular
-- JSON Schema draft, e.g. JSON Schema Draft 4.
data Spec schema = Spec
  { _ssEmbedded :: schema -> [schema]
  , _ssGetId    :: schema -> Maybe Text
  , _ssGetRef   :: schema -> Maybe Text
  }

data ReferencedSchemas schema = ReferencedSchemas
  { _rsStarting  :: !schema
  -- ^ Used to resolve relative references when we don't know what the scope
  -- of the current schema is. This only happens with starting schemas
  -- because if we're using a remote schema we had to know its URI in order
  -- to fetch it.
  --
  -- Tracking the starting schema (instead of just resolving the reference to
  -- the current schema being used for validation) is necessary for cases
  -- where schemas are embedded inside one another. For instance in this
  -- case not distinguishing the starting and "foo" schemas sends the code
  -- into an infinite loop:
  --
  -- {
  --   "additionalProperties": false,
  --   "properties": {
  --     "foo": {
  --       "$ref": "#"
  --     }
  --   }
  -- }
  , _rsSchemaMap :: !(URISchemaMap schema)
  } deriving (Eq, Show)

-- | Keys are URIs (without URI fragments).
type URISchemaMap schema = HashMap Text schema

data SchemaWithURI schema = SchemaWithURI
  { _swSchema :: !schema
  , _swURI    :: !(Maybe Text)
  -- ^ This is the URI identifying the document containing the schema.
  -- It's different than the schema's "id" field, which controls scope
  -- when resolving references contained in the schema.

  -- TODO: Make the no URI fragment requirement unnecessary.
  } deriving (Eq, Show)

--------------------------------------------------
-- * Fetch via HTTP
--------------------------------------------------

data HTTPFailure
  = HTTPParseFailure   Text
  | HTTPRequestFailure HttpException
  deriving Show

-- | Take a schema. Retrieve every document either it or its subschemas
-- include via the "$ref" keyword.
referencesViaHTTP'
  :: forall schema. FromJSON schema
  => Spec schema
  -> SchemaWithURI schema
  -> IO (Either HTTPFailure (URISchemaMap schema))
referencesViaHTTP' spec sw = do
  manager <- newManager defaultManagerSettings
  let f = referencesMethodAgnostic (get manager) spec sw
  catch (left HTTPParseFailure <$> f) handler
  where
    get :: Manager -> Text -> IO LBS.ByteString
    get man url = do
      request <- parseUrl (T.unpack url)
      responseBody <$> httpLbs request man

    handler
      :: HttpException
      -> IO (Either HTTPFailure (URISchemaMap schema))
    handler = pure . Left . HTTPRequestFailure

--------------------------------------------------
-- * Fetch via Filesystem
--------------------------------------------------

data FilesystemFailure
  = FSParseFailure Text
  | FSReadFailure  IOError
  deriving Show

referencesViaFilesystem'
  :: forall schema. FromJSON schema
  => Spec schema
  -> SchemaWithURI schema
  -> IO (Either FilesystemFailure (URISchemaMap schema))
referencesViaFilesystem' spec sw = catch (left FSParseFailure <$> f) handler
  where
    f :: IO (Either Text (URISchemaMap schema))
    f = referencesMethodAgnostic readFile' spec sw

    readFile' :: Text -> IO LBS.ByteString
    readFile' = fmap LBS.fromStrict . BS.readFile . T.unpack

    handler
      :: IOError
      -> IO (Either FilesystemFailure (URISchemaMap schema))
    handler = pure . Left . FSReadFailure

--------------------------------------------------
-- * Method Agnostic Fetching Tools
--------------------------------------------------

-- | A version of 'fetchReferencedSchema's where the function to fetch
-- schemas is provided by the user. This allows restrictions to be added,
-- e.g. rejecting non-local URIs.
referencesMethodAgnostic
  :: forall schema. FromJSON schema
  => (Text -> IO LBS.ByteString)
  -> Spec schema
  -> SchemaWithURI schema
  -> IO (Either Text (URISchemaMap schema))
referencesMethodAgnostic fetchRef spec =
  getRecursiveReferences fetchRef spec mempty

getRecursiveReferences
  :: forall schema. FromJSON schema
  => (Text -> IO LBS.ByteString)
  -> Spec schema
  -> URISchemaMap schema
  -> SchemaWithURI schema
  -> IO (Either Text (URISchemaMap schema))
getRecursiveReferences fetchRef spec@(Spec _ _ getRef) referenced sw =
  foldlM f (Right referenced) (includeSubschemas spec sw)
  where
    f :: Either Text (URISchemaMap schema)
      -> SchemaWithURI schema
      -> IO (Either Text (URISchemaMap schema))
    f (Left e) _                            = pure (Left e)
    f (Right g) (SchemaWithURI schema mUri) =
      case newRef of
        Nothing  -> pure (Right g)
        Just uri -> do
          bts <- fetchRef uri
          case eitherDecode bts of
            Left e     -> pure . Left . T.pack $ e
            Right schm -> getRecursiveReferences
                            fetchRef spec (H.insert uri schm g)
                            (SchemaWithURI schm (Just uri))
      where
        newRef :: Maybe Text
        newRef
          | Just (Just uri,_) <- resolveReference mUri <$> getRef schema
            = case H.lookup uri g of
                Nothing -> Just uri
                Just _  -> Nothing
          | otherwise = Nothing

-- | Return the schema passed in as an argument, as well as every
-- subschema contained within it.
includeSubschemas
  :: forall schema.
     Spec schema
  -> SchemaWithURI schema
  -> [SchemaWithURI schema]
includeSubschemas spec@(Spec embedded getId _) (SchemaWithURI schema mUri) =
  SchemaWithURI schema mUri
  : (includeSubschemas spec =<< subSchemas)
  where
    subSchemas :: [SchemaWithURI schema]
    subSchemas =
      (\a -> SchemaWithURI a (updateResolutionScope mUri (getId schema)))
        <$> embedded schema
