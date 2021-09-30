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

module Membership.ShameToken where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger
  ( CurrencySymbol,
    PubKeyHash (..),
    TokenName,
    ValidatorHash (..),
    Value,
  )
import Ledger.Value as Value
  ( AssetClass (..),
    TokenName (TokenName, unTokenName),
    assetClass,
    flattenValue,
    singleton,
  )
import qualified PlutusTx
import PlutusTx.Prelude
  ( Bool,
    BuiltinByteString,
    Eq (..),
    Maybe,
    appendByteString,
    find,
    lengthOfByteString,
    return,
    sliceByteString,
    ($),
    (&&),
    (+),
  )
import Membership.Utils (unValidatorHash)
import qualified Prelude as P

-- A Shame Token is a token that will be stored inside the user's account.
-- It indicates that he once broke the rules from a contract he signed.
-- This token can be useful, since it provides a way to access a user's history.
-- It also has the important function of uniquely identifying logic scripts.

-- The ShameToken datatype is a way of representing the ShameToken and
-- easily manipulate it's information. It holds the token name it self,
-- the user who holds this token, the logic script where this token was sent to and
-- the "key", which is simply a BuiltinByteString used to differentiate different
-- tokens and logic scripts
data ShameToken = ShameToken
  { stTokenName :: !TokenName,
    stUser :: !PubKeyHash,
    stScript :: !ValidatorHash,
    stKey :: !BuiltinByteString
  }
  deriving (P.Show, Generic, FromJSON, ToJSON, P.Eq)

PlutusTx.unstableMakeIsData ''ShameToken

instance Eq ShameToken where
  {-# INLINEABLE (==) #-}
  ShameToken tn usr scr key == ShameToken tn' usr' scr' key' =
    tn == tn'
      && usr == usr'
      && scr == scr'
      && key == key'

-- Based on a token name, constructs a ShameToken data type for ease of use
{-# INLINEABLE makeShameToken #-}
makeShameToken :: TokenName -> ShameToken
makeShameToken tn = ShameToken tn pkh vh key
  where
    bTokenName :: BuiltinByteString
    bTokenName = unTokenName tn

    pkh :: PubKeyHash
    pkh = PubKeyHash $ sliceByteString 0 28 bTokenName

    vh :: ValidatorHash
    vh = ValidatorHash $ sliceByteString 28 28 bTokenName

    key :: BuiltinByteString
    key = sliceByteString 56 (lengthOfByteString (unTokenName tn) + 1) bTokenName

-- Constructs a Shame Token name based on the user's PubKeyHash, the
-- logic ValidatorHash and the key
{-# INLINEABLE makeShameTokenName #-}
makeShameTokenName :: PubKeyHash -> ValidatorHash -> BuiltinByteString -> TokenName
makeShameTokenName pkh vh key =
  TokenName $ getPubKeyHash pkh `appendByteString` unValidatorHash vh `appendByteString` key

-- Constructs a Shame Token based on the user's PubKeyHash, the
-- logic ValidatorHash and the key
{-# INLINEABLE shameToken #-}
shameToken :: PubKeyHash -> ValidatorHash -> BuiltinByteString -> ShameToken
shameToken pkh vh key = makeShameToken $ makeShameTokenName pkh vh key

-- Based on a currency symbol and the shame token essential information, returns an asset class
{-# INLINEABLE shameAssetClass #-}
shameAssetClass :: CurrencySymbol -> PubKeyHash -> ValidatorHash -> BuiltinByteString -> AssetClass
shameAssetClass cs pkh vh key = assetClass cs (makeShameTokenName pkh vh key)

-- The value corresponding to a single shame token
{-# INLINEABLE shameValue #-}
shameValue :: CurrencySymbol -> ShameToken -> Value
shameValue cs st = singleton cs (stTokenName st) 1

-- Tries to find a shame token inside a value based on it's currency symbol
{-# INLINEABLE findShameToken #-}
findShameToken :: CurrencySymbol -> Value -> Maybe ShameToken
findShameToken cs v = do
  (_, tn, _) <- find (\(cs', _, _) -> cs == cs') (flattenValue v)
  return $ makeShameToken tn

-- Tries to find a shame token inside a value based on it's currency symbol
-- only if the public key hash and key from this token correspond to the given ones
{-# INLINEABLE findShameToken' #-}
findShameToken' :: CurrencySymbol -> PubKeyHash -> BuiltinByteString -> Value -> Maybe ShameToken
findShameToken' cs pkh key v = do
  (_, tn, _) <- find (\(cs', tn', _) -> f cs' tn') (flattenValue v)
  return $ makeShameToken tn
  where
    f :: CurrencySymbol -> TokenName -> Bool
    f cs' tn' =
      (cs == cs')
        && (stUser (makeShameToken tn') == pkh)
        && (stKey (makeShameToken tn') == key)