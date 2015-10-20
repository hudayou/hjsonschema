
module Data.JsonSchema.Draft4.Arrays where

import           Control.Monad
import qualified Data.HashMap.Strict     as H
import qualified Data.Vector             as V

import           Data.JsonSchema.Core
import           Data.JsonSchema.Helpers
import           Import

data ItemsFailure err = Items err | AdditionalItemsBool | AdditionalItemsObject err

-- | A combination of items and additionalItems.
items :: ValidatorConstructor err [ValidationFailure (ItemsFailure err)]
items spec g s (Object o) =
  let subSchema = compile spec g (RawSchema (_rsURI s) o)
  in Just $ \x ->
    case x of
      Array ys -> V.toList ys >>= fmap (modifyFailureName Items) . validate subSchema
      _        -> mempty
items spec g s (Array vs) = do
  os <- traverse toObj vs
  let subSchemas = compile spec g . RawSchema (_rsURI s) <$> V.toList os
      mAdditionalItems = additionalItems spec g s =<< H.lookup "additionalItems" (_rsObject s)
  Just $ \x ->
    case x of
      Array ys ->
        let extras = V.drop (V.length os) ys
            itemFailures = join $ fmap (modifyFailureName Items) <$> zipWith validate subSchemas (V.toList ys)
            additionalItemFailures = runMaybeVal mAdditionalItems (Array extras)
        in itemFailures <> additionalItemFailures
      _ -> mempty
items _ _ _ _ = Nothing

-- | Not included directly in the 'draft4' spec hashmap because it always
-- validates data unless 'items' is also present. This is because 'items'
-- defaults to {}.
--
-- TODO: Should have its own error type instead of sharing with Items.
additionalItems :: ValidatorConstructor err [ValidationFailure (ItemsFailure err)]
additionalItems _ _ _ val@(Bool v) =
  Just $ \x ->
    case x of
      Array ys ->
        if not v && V.length ys > 0
          then pure $ ValidationFailure AdditionalItemsBool (FailureInfo val x)
          else mempty
      _ -> mempty
additionalItems spec g s (Object o) =
  let subSchema = compile spec g (RawSchema (_rsURI s) o)
  in Just $ \x ->
    case x of
      Array ys -> V.toList ys >>= fmap (modifyFailureName AdditionalItemsObject) . validate subSchema
      _        -> mempty
additionalItems _ _ _ _ = Nothing

maxItems :: ValidatorConstructor err [FailureInfo]
maxItems _ _ _ val = do
  n <- fromJSONInt val
  greaterThanZero n
  Just $ \x ->
    case x of
      Array ys ->
        if V.length ys > n
          then pure (FailureInfo val x)
          else mempty
      _ -> mempty

minItems :: ValidatorConstructor err [FailureInfo]
minItems _ _ _ val = do
  n <- fromJSONInt val
  greaterThanZero n
  Just $ \x ->
    case x of
      Array ys ->
        if V.length ys < n
          then pure (FailureInfo val x)
          else mempty
      _ -> mempty

uniqueItems :: ValidatorConstructor err [FailureInfo]
uniqueItems _ _ _ val@(Bool v) = do
  unless v Nothing
  Just $ \x ->
    case x of
      (Array ys) -> if allUniqueValues ys
        then mempty
        else pure (FailureInfo val x)
      _ -> mempty
uniqueItems _ _ _ _ = Nothing