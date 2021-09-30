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

module Membership.Signature where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger.Contexts (TxInfo, txSignedBy)
import Ledger.Crypto (PubKeyHash (..))
import Ledger.Scripts (ValidatorHash (..))
import Ledger.Value as Value
  ( AssetClass (AssetClass),
    CurrencySymbol,
    TokenName (..),
    Value,
    flattenValue,
    singleton,
  )
import Membership.Utils (tripleSnd, unValidatorHash)
import qualified PlutusTx
import PlutusTx.Prelude
  ( Bool (True),
    BuiltinByteString,
    Eq ((==)),
    Integer,
    Maybe,
    all,
    any,
    appendByteString,
    filter,
    find,
    isJust,
    map,
    return,
    sliceByteString,
    ($),
    (&&),
    (.),
    (<$>),
  )
import qualified Prelude as P

-- Because we can't know the account validator hash when
-- creating the signature policy, we embed the output script
-- validator hash in the sig token name. Authentication,
-- later on, must verify that the public key corressponds to
-- the user, but also that the embeded validator hash corresponds
-- to the account

-- Sig is a data type that can be created by giving makeSig a token name
-- and serves as an easy way to get the "signature" essential information
data Sig = Sig
  { sTokenName :: !TokenName,
    sUser :: !PubKeyHash,
    sScript :: !ValidatorHash
  }
  deriving (P.Show, Generic, FromJSON, ToJSON, P.Eq)

PlutusTx.unstableMakeIsData ''Sig

instance Eq Sig where
  {-# INLINEABLE (==) #-}
  Sig tn usr scr == Sig tn' usr' scr' =
    tn
      == tn' && usr
      == usr' && scr
      == scr'

-- Join two BuiltinByteString corresponding to the user's
-- PubKeyHash and the account ValidatorHash and make that
-- into a TokenName
{-# INLINEABLE makeSigToken #-}
makeSigToken :: PubKeyHash -> ValidatorHash -> TokenName
makeSigToken pkh vh =
  TokenName $ getPubKeyHash pkh `appendByteString` unValidatorHash vh

-- Given a TokenName, create a SIG. Useful for easily extracting the information from a SIG token.
{-# INLINEABLE makeSig #-}
makeSig :: TokenName -> Sig
makeSig tn = Sig tn pkh vh
  where
    -- The policy output script validator hash and the user
    -- public key hash can be found by slicing the TokenName in two

    bTokenName :: BuiltinByteString
    bTokenName = unTokenName tn

    vh :: ValidatorHash
    vh = ValidatorHash $ sliceByteString 28 28 bTokenName

    pkh :: PubKeyHash
    pkh = PubKeyHash $ sliceByteString 0 28 bTokenName

-- Given the user's pubkeyhash and the account validator hash, create a SIG
{-# INLINEABLE sig #-}
sig :: PubKeyHash -> ValidatorHash -> Sig
sig pkh = makeSig . makeSigToken pkh

{-# INLINEABLE sigTokenToUser #-}
sigTokenToUser :: TokenName -> PubKeyHash
sigTokenToUser = sUser . makeSig

-- Given the signature currency symbol and a value,
-- tries to find a signature token
{-# INLINEABLE findSignature #-}
findSignature :: CurrencySymbol -> Value -> Maybe Sig
findSignature cs v = do
  (_, tn, _) <- find (\(cs', _, _) -> cs == cs') (flattenValue v)
  return $ makeSig tn

-- Given the signature currency symbol and a value,
-- tries to find all signature tokens
{-# INLINEABLE findSignatures #-}
findSignatures :: CurrencySymbol -> Value -> [Sig]
findSignatures cs v = map (makeSig . tripleSnd) $ filter (\(cs', _, _) -> cs == cs') (flattenValue v)

-- Given the signature currency symbol and a value,
-- tries to find a signature token and returns it's user
{-# INLINEABLE findSignatory #-}
findSignatory :: CurrencySymbol -> Value -> Maybe PubKeyHash
findSignatory cs v = sUser <$> findSignature cs v

-- Given the signature currency symbol and a value,
-- tries to find all signature tokens and return their users
{-# INLINEABLE findSignatories #-}
findSignatories :: CurrencySymbol -> Value -> [PubKeyHash]
findSignatories cs v = map (sigTokenToUser . tripleSnd) $ filter (\(cs', _, _) -> cs == cs') (flattenValue v)

-- Given a transaction context info and a list of
-- public keys, verify if any of those signed this transaction
{-# INLINEABLE anySigned #-}
anySigned :: TxInfo -> [PubKeyHash] -> Bool
anySigned info = any (txSignedBy info)

-- Given a transaction context info and a list of
-- public keys, get the one that signed this transaction
{-# INLINEABLE whoSigned #-}
whoSigned :: TxInfo -> [PubKeyHash] -> Maybe PubKeyHash
whoSigned info = find (txSignedBy info)

-- Given a transaction context info and a list of
-- Sigs, get the one corresponding to the user that
-- signed this transaction
{-# INLINEABLE whoSigned' #-}
whoSigned' :: TxInfo -> [Sig] -> Maybe Sig
whoSigned' info = find (txSignedBy info . sUser)

-- Find the signature with the corresponding public key
{-# INLINEABLE findSig #-}
findSig :: PubKeyHash -> [Sig] -> Maybe Sig
findSig pkh = find (\s -> sUser s == pkh)

{-# INLINEABLE sigIn #-}
sigIn :: PubKeyHash -> [Sig] -> Bool
sigIn pkh s = isJust $ findSig pkh s

-- Verify if all sig tokens have the same script address embeded
{-# INLINEABLE consistentSigs #-}
consistentSigs :: [Sig] -> Bool
consistentSigs [] = True
consistentSigs (x : xs) = all ((== sScript x) . sScript) xs

-- Make a SIG token asset class
{-# INLINEABLE signatureAssetClass #-}
signatureAssetClass :: CurrencySymbol -> PubKeyHash -> ValidatorHash -> AssetClass
signatureAssetClass cs pkh vh = AssetClass (cs, makeSigToken pkh vh)

-- Make a single SIG token value based on the user's
-- public key hash and the account's validator hash
{-# INLINEABLE signatureValue #-}
signatureValue :: CurrencySymbol -> PubKeyHash -> ValidatorHash -> Value
signatureValue cs pkh vh = singleton cs (makeSigToken pkh vh) 1

-- Make a single SIG token value based on the given SIG
{-# INLINEABLE signatureValue' #-}
signatureValue' :: CurrencySymbol -> Sig -> Value
signatureValue' cs s = singleton cs (makeSigToken (sUser s) (sScript s)) 1

-- Make a single SIG token value based on the given SIG and an amount
{-# INLINEABLE signatureValueOf' #-}
signatureValueOf' :: CurrencySymbol -> Sig -> Integer -> Value
signatureValueOf' cs s = singleton cs (makeSigToken (sUser s) (sScript s))