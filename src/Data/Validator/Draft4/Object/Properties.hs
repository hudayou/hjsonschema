
module Data.Validator.Draft4.Object.Properties where

import           Import
import           Prelude

import           Control.Monad
import qualified Data.Aeson.Pointer     as P
import qualified Data.HashMap.Strict    as H
import           Data.Text.Encoding     (encodeUtf8)
import qualified Text.Regex.PCRE.Heavy  as RE

import           Data.Validator.Failure

newtype Remaining = Remaining { _unRemaining :: HashMap Text Value }

--------------------------------------------------
-- * properties
--------------------------------------------------

data PropertiesInvalid err
  = PropertiesInvalid err
  | PropPatternInvalid err
  | PropAdditionalInvalid (AdditionalPropertiesInvalid err)
  deriving (Eq, Show)

-- | In order of what's tried: "properties", "patternProperties",
-- "additionalProperties".
properties
  :: forall err schema.
     (schema -> Value -> [Failure err])
  -> Maybe (HashMap Text schema)
  -> Maybe (AdditionalProperties schema)
  -> HashMap Text schema
  -> HashMap Text Value
  -> [Failure (PropertiesInvalid err)]
properties f mPat mAdd propertiesHm x =
     fmap (modFailure PropertiesInvalid) propFailures
  <> fmap (modFailure PropPatternInvalid) patternFailures
  <> fmap (modFailure PropAdditionalInvalid) additionalFailures
  where
    propertiesAndUnmatched :: ([Failure err], Remaining)
    propertiesAndUnmatched = ( failures
                             , Remaining (H.difference x propertiesHm)
                             )
      where
        failures :: [Failure err]
        failures = H.toList (H.intersectionWith f propertiesHm x)
               >>= (\(k,vs) -> fmap (addToPath (P.Token k)) vs)

    (propFailures, remaining1) = propertiesAndUnmatched

    mPatProp :: Maybe (HashMap Text Value -> ([Failure err], Remaining))
    mPatProp = patternAndUnmatched f <$> mPat

    patternFailures :: [Failure err]
    patternFailures = case mPatProp of
                        Nothing  -> mempty
                        Just val -> fst (val x)

    remaining2 :: Remaining
    remaining2 = case mPatProp of
                   Nothing  -> remaining1
                   Just val -> snd . val . _unRemaining $ remaining1

    additionalFailures :: [Failure (AdditionalPropertiesInvalid err)]
    additionalFailures = case additionalProperties f <$> mAdd of
                           Nothing  -> mempty
                           Just val -> val (_unRemaining remaining2)

--------------------------------------------------
-- * patternProperties
--------------------------------------------------

data PatternPropertiesInvalid err
  = PPInvalid err
  | PPAdditionalPropertiesInvalid (AdditionalPropertiesInvalid err)
  deriving (Eq, Show)

patternProperties
  :: forall err schema.
     (schema -> Value -> [Failure err])
  -> Maybe (AdditionalProperties schema)
  -> HashMap Text schema
  -> HashMap Text Value
  -> [Failure (PatternPropertiesInvalid err)]
patternProperties f mAdd patternPropertiesHm x =
     fmap (modFailure PPInvalid) ppFailures
  <> fmap (modFailure PPAdditionalPropertiesInvalid) addFailures
  where
    patternProps :: ([Failure err], Remaining)
    patternProps = patternAndUnmatched f patternPropertiesHm x

    (ppFailures, remaining) = patternProps

    addFailures :: [Failure (AdditionalPropertiesInvalid err)]
    addFailures = case additionalProperties f <$> mAdd of
                    Nothing  -> mempty
                    Just val -> val (_unRemaining remaining)

patternAndUnmatched
  :: forall err schema.
     (schema -> Value -> [Failure err])
  -> HashMap Text schema
  -> HashMap Text Value
  -> ([Failure err], Remaining)
patternAndUnmatched f patPropertiesHm x =
  (H.foldlWithKey' runVals mempty perhapsMatches, remaining)
  where
    perhapsMatches :: HashMap Text (Value, [schema])
    perhapsMatches = H.foldlWithKey' (matchingSchemas patPropertiesHm) mempty x
      where
        matchingSchemas
          :: HashMap Text schema
          -> HashMap Text (Value, [schema])
          -> Text
          -> Value
          -> HashMap Text (Value, [schema])
        matchingSchemas subSchemas acc k v =
          H.insert k (v, H.foldlWithKey' (checkKey k) mempty subSchemas) acc

        checkKey
          :: Text
          -> [schema]
          -> Text
          -> schema
          -> [schema]
        checkKey k acc r subSchema =
          case RE.compileM (encodeUtf8 r) mempty of
            Left _   -> acc
            Right re -> if k RE.=~ re
                          then pure subSchema <> acc
                          else acc

    runVals
      :: [Failure err]
      -> Text
      -> (Value, [schema])
      -> [Failure err]
    runVals acc k (v,subSchemas) =
      (subSchemas >>= (\schema -> addToPath (P.Token k) <$> f schema v))
      <> acc

    remaining :: Remaining
    remaining = Remaining . fmap fst . H.filter (null . snd) $ perhapsMatches

--------------------------------------------------
-- * additionalProperties
--------------------------------------------------

data AdditionalPropertiesInvalid err
  = APBoolInvalid
  | APObjectInvalid err
  deriving (Eq, Show)

data AdditionalProperties schema
  = AdditionalPropertiesBool Bool
  | AdditionalPropertiesObject schema
  deriving (Eq, Show)

instance FromJSON schema => FromJSON (AdditionalProperties schema) where
  parseJSON v = fmap AdditionalPropertiesBool (parseJSON v)
            <|> fmap AdditionalPropertiesObject (parseJSON v)

instance ToJSON schema => ToJSON (AdditionalProperties schema) where
  toJSON (AdditionalPropertiesBool b)    = toJSON b
  toJSON (AdditionalPropertiesObject hm) = toJSON hm

instance Arbitrary schema => Arbitrary (AdditionalProperties schema) where
  arbitrary = oneof [ AdditionalPropertiesBool <$> arbitrary
                    , AdditionalPropertiesObject <$> arbitrary
                    ]

additionalProperties
  :: forall err schema.
     (schema -> Value -> [Failure err])
  -> AdditionalProperties schema
  -> HashMap Text Value
  -> [Failure (AdditionalPropertiesInvalid err)]
additionalProperties _ (AdditionalPropertiesBool False) x
  | H.size x > 0 = pure $ Invalid APBoolInvalid (Bool False) mempty
  | otherwise    = mempty
additionalProperties _ (AdditionalPropertiesBool True) _ = mempty
additionalProperties f (AdditionalPropertiesObject schema) x = H.toList x >>= g
  where
    g :: (Text, Value) -> [Failure (AdditionalPropertiesInvalid err)]
    g (k,v) = modFailure APObjectInvalid
            . addToPath (P.Token k)
          <$> f schema v
