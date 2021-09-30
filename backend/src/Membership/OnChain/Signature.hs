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

module Membership.OnChain.Signature where

import Ledger.Contexts
  ( ScriptContext (scriptContextTxInfo),
    TxInfo (txInfoMint),
    TxOut (..),
    findDatum,
    ownCurrencySymbol,
    scriptCurrencySymbol,
    txSignedBy,
  )
import Ledger.Crypto (PubKeyHash)
import Ledger.Scripts
  ( ValidatorHash,
    mkMintingPolicyScript,
  )
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value as Value
  ( CurrencySymbol,
    Value,
    flattenValue,
    assetClassValue
  )
import Membership.Account
import Membership.Signature
  ( Sig (..),
    makeSig,
    signatureValueOf'
  )
import Membership.Utils
import qualified PlutusTx
import PlutusTx.Prelude
  ( Bool,
    Eq ((==)),
    Maybe (..),
    traceError,
    traceIfFalse,
    ($),
    (&&),
    (.),
    (<>),
    (==),
  )
import Membership.PlatformSettings (PlatformSettings (..))

-- The policy that will allow or not the minting of new signature tokens
-- Signature tokens are DigiService's way of authenticating accounts,
-- making sure the fees are paid and identifying users
{-# INLINEABLE mkSignaturePolicy #-}
mkSignaturePolicy :: PlatformSettings -> () -> ScriptContext -> Bool
mkSignaturePolicy ps () ctx =
  traceIfFalse "Sig Policy - Transaction not signed by token public key" (txSignedBy info pkh)
    && traceIfFalse "Sig Policy - Invalid account value" validAccountValue
    && traceIfFalse "Sig Policy - Invalid account datum" validAccountDatum
  where
    -- The basic information about this transaction
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Verifies if only a specific token was minted and get's
    -- a Sig based on this token name
    ownSig :: Sig
    ownSig = case flattenValue (txInfoMint info) of
      [(_, tn, _)] -> makeSig tn
      _ -> traceError "Sig Policy - Must mint one token only"

    -- Builds a value based on our currency symbol and the
    -- sig we made with the token name minted
    sigValue :: Value
    sigValue = signatureValueOf' (ownCurrencySymbol ctx) ownSig 100

    pkh :: PubKeyHash
    pkh = sUser ownSig

    -- This should be the account validator hash, but could be any script
    -- Therefore, off-chain authentication should also make sure that the
    -- validator hash found in the token name is equivalent to the account one
    accValHash :: ValidatorHash
    accValHash = sScript ownSig

    -- The entrance fee required to enter in the platform
    fees :: Value
    fees = assetClassValue (psToken ps) (psEntranceFee ps)

    accountOutput :: TxOut
    accountOutput = case strictFindOutputWithValHash accValHash info of
      Just out -> out
      Nothing -> traceError "Contract Remove Account - Couldn't an unique account output"
    
    accountDatum :: AccountDatum
    accountDatum = case findAccountDatum accountOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError "Logic Mediate - Logic Datum could not be found"
    
    validAccountValue :: Bool
    validAccountValue = txOutValue accountOutput == sigValue <> fees

    validAccountDatum :: Bool
    validAccountDatum = accountDatum == initDatum

signaturePolicy :: PlatformSettings -> Scripts.MintingPolicy
signaturePolicy ps =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkSignaturePolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode ps

signatureCurrencySymbol :: PlatformSettings -> CurrencySymbol
signatureCurrencySymbol = scriptCurrencySymbol . signaturePolicy