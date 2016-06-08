{-# LANGUAGE DeriveGeneric #-}

module Shared where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy   as LBS
import           Data.Char              (toLower)
import qualified Data.List.NonEmpty     as N
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as T
import           GHC.Generics
import           System.FilePath        ((</>))
import           Test.Tasty             (TestTree)
import qualified Test.Tasty.HUnit       as HU

import qualified Data.JsonSchema.Draft4 as D4

isLocal :: String -> Bool
isLocal file = (file /= "definitions.json")
            && (file /= "ref.json")
            && (file /= "refRemote.json")

data SchemaTest = SchemaTest
  { _stDescription :: Text
  , _stSchema      :: D4.Schema
  , _stCases       :: [SchemaTestCase]
  }

data SchemaTestCase = SchemaTestCase
  { _scDescription :: Text
  , _scData        :: Value
  , _scValid       :: Bool
  } deriving Generic

instance FromJSON SchemaTest where
  parseJSON = withObject "SchemaTest" $ \o -> SchemaTest
    <$> o .: "description"
    <*> o .: "schema"
    <*> o .: "tests" -- Perhaps "cases" would have been a more descriptive key.

instance FromJSON SchemaTestCase where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = fmap toLower . drop 3 }

readSchemaTests
    :: String
    -- ^ The path to a directory.
    -> [String]
    -- ^ The names of JSON files in that directory.
    -> IO [SchemaTest]
readSchemaTests dir = concatMapM fileToCases
  where
    -- Each file contains an array of SchemaTests, not just one.
    fileToCases :: String -> IO [SchemaTest]
    fileToCases name = do
      let fullPath = dir </> name
      jsonBS <- LBS.readFile fullPath
      case eitherDecode jsonBS of
        Left e -> fail $ "couldn't parse file '" <> fullPath <> "': " <> e
        Right schemaTests -> pure $ prependFileName name <$> schemaTests

    prependFileName :: String -> SchemaTest -> SchemaTest
    prependFileName fileName s = s
      { _stDescription = T.pack fileName <> ": " <> _stDescription s
      }

    -- | Can remove the Functor constraint after GHC 7.10.
    concatMapM :: (Functor m, Monad m) => (a -> m [b]) -> [a] -> m [b]
    concatMapM f xs = concat <$> mapM f xs

toTest :: SchemaTest -> TestTree
toTest st =
  HU.testCase (T.unpack (_stDescription st)) $ do
    forM_ (_stCases st) $ \sc -> do
      res <- D4.fetchHTTPAndValidate
               (D4.SchemaWithURI (_stSchema st) Nothing)
               (_scData sc)
      let failures = case res of
                       Right ()           -> mempty
                       Left (D4.HVData a) -> N.toList a
                       other              -> error (show other)
      if _scValid sc
          then assertValid   sc failures
          else assertInvalid sc failures

assertValid :: SchemaTestCase -> [D4.Invalid] -> HU.Assertion
assertValid _ [] = pure ()
assertValid sc failures =
  HU.assertFailure $ unlines
    [ "    Failed to validate data"
    , "    Description: "         <> T.unpack (_scDescription sc)
    , "    Data: "                <> show (_scData sc)
    , "    Validation failures: " <> show failures
    ]

assertInvalid :: SchemaTestCase -> [D4.Invalid] -> HU.Assertion
assertInvalid sc [] =
  HU.assertFailure $ unlines
    [ "    Validated invalid data"
    , "    Description: " <> T.unpack (_scDescription sc)
    , "    Data: "        <> show (_scData sc)
    ]
assertInvalid _ _ = pure ()
