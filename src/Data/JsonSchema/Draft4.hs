{-# LANGUAGE TemplateHaskell     #-}

module Data.JsonSchema.Draft4
  ( -- * Draft 4 Schema
    SchemaWithURI(..)
  , Schema(..)
  , SC.emptySchema

    -- * One-step validation (getting references over HTTP)
  , fetchHTTPAndValidate
  , HTTPValidationFailure(..)
  , HTTPFailure(..)
  , InvalidSchema

    -- * One-step validation (getting references from the filesystem)
  , fetchFilesystemAndValidate
  , FilesystemValidationFailure(..)
  , FilesystemFailure(..)

    -- * Validation failure
  , Invalid
  , FR.Failure(..)
  , ValidatorChain(..)

    -- * Fetching tools
  , ReferencedSchemas(..)
  , referencesViaHTTP
  , referencesViaFilesystem

    -- * Other Draft 4 things exported just in case
  , IN.runValidate
  , schemaValidity
  , referencesValidity
  , checkSchema
  , draft4Spec
  ) where

import           Import
import           Prelude

import           Control.Applicative
import           Control.Arrow                   (first, left)
import qualified Data.ByteString.Lazy            as LBS
import           Data.FileEmbed                  (embedFile,
                                                  makeRelativeToProject)
import qualified Data.HashMap.Strict             as H
import           Data.List.NonEmpty              (NonEmpty)
import qualified Data.List.NonEmpty              as N
import           Data.Maybe                      (fromMaybe)

import           Data.JsonSchema.Draft4.Failure  (Invalid, InvalidSchema,
                                                  ValidatorChain(..))
import qualified Data.JsonSchema.Draft4.Internal as IN
import           Data.JsonSchema.Draft4.Schema   (Schema)
import qualified Data.JsonSchema.Draft4.Schema   as SC
import           Data.JsonSchema.Fetch           (FilesystemFailure(..),
                                                  HTTPFailure(..),
                                                  ReferencedSchemas(..),
                                                  SchemaWithURI(..))
import qualified Data.JsonSchema.Fetch           as FE
import qualified Data.Validator.Failure          as FR

data HTTPValidationFailure
  = HVRequest HTTPFailure
  | HVSchema  InvalidSchema
  | HVData    (NonEmpty Invalid)
  deriving Show

-- | Fetch recursively referenced schemas over HTTP, check that both the
-- original and referenced schemas are valid, and then validate data.
fetchHTTPAndValidate
  :: SchemaWithURI Schema
  -> Value
  -> IO (Either HTTPValidationFailure ())
fetchHTTPAndValidate sw v = do
  res <- referencesViaHTTP sw
  pure (g =<< f =<< left HVRequest res)
  where
    f :: FE.URISchemaMap Schema
      -> Either HTTPValidationFailure (Value -> [Invalid])
    f references = left HVSchema (checkSchema references sw)

    g :: (Value -> [Invalid]) -> Either HTTPValidationFailure ()
    g validate = case N.nonEmpty (validate v) of
                   Nothing       -> Right ()
                   Just failures -> Left (HVData failures)

data FilesystemValidationFailure
  = FVRead   FilesystemFailure
  | FVSchema InvalidSchema
  | FVData   (NonEmpty Invalid)
  deriving Show

-- | Fetch recursively referenced schemas from the filesystem, check that
-- both the original and referenced schemas are valid, and then
-- validate data.
fetchFilesystemAndValidate
  :: SchemaWithURI Schema
  -> Value
  -> IO (Either FilesystemValidationFailure ())
fetchFilesystemAndValidate sw v = do
  res <- referencesViaFilesystem sw
  pure (g =<< f =<< left FVRead res)
  where
    f :: FE.URISchemaMap Schema
      -> Either FilesystemValidationFailure (Value -> [Invalid])
    f references = left FVSchema (checkSchema references sw)

    g :: (Value -> [Invalid]) -> Either FilesystemValidationFailure ()
    g validate = case N.nonEmpty (validate v) of
                   Nothing       -> Right ()
                   Just failures -> Left (FVData failures)

-- | An instance of 'Data.JsonSchema.Fetch.Spec' specialized for
-- JSON Schema Draft 4.
draft4Spec :: FE.Spec Schema
draft4Spec = FE.Spec IN.embedded SC._schemaId SC._schemaRef

-- | Fetch the schemas recursively referenced by a starting schema over HTTP.
referencesViaHTTP
  :: SchemaWithURI Schema
  -> IO (Either HTTPFailure (FE.URISchemaMap Schema))
referencesViaHTTP = FE.referencesViaHTTP' draft4Spec

-- | Fetch the schemas recursively referenced by a starting schema from
-- the filesystem.
referencesViaFilesystem
  :: SchemaWithURI Schema
  -> IO (Either FilesystemFailure (FE.URISchemaMap Schema))
referencesViaFilesystem = FE.referencesViaFilesystem' draft4Spec

-- | A helper function.
--
-- Checks if a schema and a set of referenced schemas are valid.
--
-- Return a function to validate data.
checkSchema
  :: FE.URISchemaMap Schema
  -> SchemaWithURI Schema
  -> Either InvalidSchema (Value -> [Invalid])
checkSchema sm sw =
  case N.nonEmpty failures of
    Nothing -> Right (IN.runValidate (ReferencedSchemas (_swSchema sw) sm) sw)
    Just fs -> Left fs
  where
    failures :: [(Maybe Text, Invalid)]
    failures = ((\v -> (Nothing, v)) <$> schemaValidity (_swSchema sw))
            <> (first Just <$> referencesValidity sm)

-- | Check that a schema itself is valid
-- (if so the returned list will be empty).
schemaValidity :: Schema -> [Invalid]
schemaValidity = IN.runValidate referenced (SchemaWithURI d4 Nothing) . toJSON
  where
    d4 :: Schema
    d4 = fromMaybe (error "Schema decode failed (this should never happen)")
       . decode
       . LBS.fromStrict
       $ $(makeRelativeToProject "src/draft4.json" >>= embedFile)

    referenced :: ReferencedSchemas Schema
    referenced = ReferencedSchemas
                   d4
                   (H.singleton "http://json-schema.org/draft-04/schema" d4)

-- | Check that a set of referenced schemas are valid
-- (if so the returned list will be empty).
referencesValidity
  :: FE.URISchemaMap Schema
  -> [(Text, Invalid)]
  -- ^ The first value in the tuple is the URI of a referenced schema.
referencesValidity = H.foldlWithKey' f mempty
  where
    f :: [(Text, Invalid)]
      -> Text
      -> Schema
      -> [(Text, Invalid)]
    f acc k v = ((\a -> (k,a)) <$> schemaValidity v) <> acc
