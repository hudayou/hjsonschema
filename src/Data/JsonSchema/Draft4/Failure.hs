
module Data.JsonSchema.Draft4.Failure where

import           Prelude

import           Data.List.NonEmpty              (NonEmpty)
import           Data.Text                       (Text)

import qualified Data.Validator.Failure as FR

-- | A description of why a schema or one of its reference is invalid.
--
-- The schema that caused the failure is indicated by the first item in the
-- tuple. 'Nothing' indicates it is the starting schema. 'Just' indicates
-- that it is a referenced schema -- the contents of the 'Just' is the
-- schema's URI.
type InvalidSchema = NonEmpty (Maybe Text, Invalid)

type Invalid = FR.Failure ValidatorChain

-- | Distinguish all the different possible causes of failure for
-- Draft 4 validation.
data ValidatorChain
  = MultipleOf
  | Maximum
  | ExclusiveMaximum
  | Minimum
  | ExclusiveMinimum

  | MaxLength
  | MinLength
  | PatternValidator

  | MaxItems
  | MinItems
  | UniqueItems
  | Items ValidatorChain
  | AdditionalItemsBool
  | AdditionalItemsObject ValidatorChain

  | MaxProperties
  | MinProperties
  | Required
  | SchemaDependency ValidatorChain
  | PropertyDependency
  | Properties ValidatorChain
  | PatternProperties ValidatorChain
  | AdditionalPropertiesBool
  | AdditionalPropertiesObject ValidatorChain

  | RefResolution
    -- ^ Indicates a reference that failed to resolve.
    --
    -- NOTE: The language agnostic test suite doesn't specify if this should
    -- cause a validation error or should allow data to pass. We choose to
    -- return a validation error.
    --
    -- Also note that ideally we would enforce in the type system that any
    -- failing references be dealt with before valididation. Then this could
    -- be removed entirely.
  | Ref ValidatorChain
  | Enum
  | TypeValidator
  | AllOf ValidatorChain
  | AnyOf
  | OneOf
  | NotValidator
  deriving (Eq, Show)
