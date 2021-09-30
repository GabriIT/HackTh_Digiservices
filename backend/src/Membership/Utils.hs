{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Membership.Utils where

import Ledger
  ( TxInInfo (txInInfoResolved),
    TxInfo (txInfoInputs, txInfoOutputs),
    TxOut (TxOut),
    ValidatorHash,
    toValidatorHash,
  )
import Ledger.Scripts (ValidatorHash (..))
import PlutusTx.AssocMap as Map (Map, keys, lookup, member)
import PlutusTx.Prelude
  ( BuiltinByteString,
    Eq (..),
    Maybe (..),
    filter,
    find,
    map,
    not,
    return,
    ($),
    (.),
  )

-- Transforms a ValidatorHash into a BuiltinByteString
{-# INLINEABLE unValidatorHash #-}
unValidatorHash :: ValidatorHash -> BuiltinByteString
unValidatorHash vh = case vh of ValidatorHash h -> h

-- Given a triple (a, b, c) returns a (the first element)
{-# INLINEABLE tripleFst #-}
tripleFst :: (a, b, c) -> a
tripleFst (a, _, _) = a

-- Given a triple (a, b, c) returns b (the second element)
{-# INLINEABLE tripleSnd #-}
tripleSnd :: (a, b, c) -> b
tripleSnd (_, b, _) = b

-- Given a triple (a, b, c) returns c (the third element)
{-# INLINEABLE tripleThd #-}
tripleThd :: (a, b, c) -> c
tripleThd (_, _, c) = c

-- Given two AssocMaps, tries to get a key from the fisrt map which is not in the second
-- If it's able to find it, returns a pair (key, value) 
{-# INLINEABLE subtractMaps #-}
subtractMaps :: forall k v. (Eq k) => Map.Map k v -> Map.Map k v -> Maybe (k, v)
subtractMaps m m' = do
  k <- find (\k' -> not $ k' `Map.member` m') (keys m)
  v <- lookup k m
  return (k, v)

-- Given a ValidatorHash and a TxInfo, tries to find an output inside the inputs from this TxInfo
-- that has the same ValidatorHash as the given one
{-# INLINEABLE findInputWithValHash #-}
findInputWithValHash :: ValidatorHash -> TxInfo -> Maybe TxOut
findInputWithValHash vh info = find predicate ((map txInInfoResolved . txInfoInputs) info)
  where
    predicate (TxOut addr _ _) = toValidatorHash addr == Just vh

-- Searchs for a transaction output of a specific validator hash
-- in the inputs list, but only return it if it's unique
{-# INLINEABLE strictFindInputWithValHash #-}
strictFindInputWithValHash :: ValidatorHash -> TxInfo -> Maybe TxOut
strictFindInputWithValHash vh info = case filter predicate ((map txInInfoResolved . txInfoInputs) info) of
  [o] -> Just o
  _ -> Nothing
  where
    predicate (TxOut addr _ _) = toValidatorHash addr == Just vh

-- Given a ValidatorHash and a TxInfo, tries to find an output inside the outputs from this TxInfo
-- that has the same ValidatorHash as the given one
{-# INLINEABLE findOutputWithValHash #-}
findOutputWithValHash :: ValidatorHash -> TxInfo -> Maybe TxOut
findOutputWithValHash vh info = find predicate (txInfoOutputs info)
  where
    predicate (TxOut addr _ _) = toValidatorHash addr == Just vh

-- Searchs for a transaction output of a specific validator hash
-- in the outputs list, but only return it if it's unique
{-# INLINEABLE strictFindOutputWithValHash #-}
strictFindOutputWithValHash :: ValidatorHash -> TxInfo -> Maybe TxOut
strictFindOutputWithValHash vh info = case filter predicate (txInfoOutputs info) of
  [o] -> Just o
  _ -> Nothing
  where
    predicate (TxOut addr _ _) = toValidatorHash addr == Just vh