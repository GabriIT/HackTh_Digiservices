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

module Membership.OnChain.ShameToken where

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
  )
import Membership.Logic
import Membership.Utils
import Membership.ShameToken
import qualified PlutusTx
import PlutusTx.Prelude
  ( Bool (..),
    Eq ((==)),
    Maybe (..),
    traceError,
    traceIfFalse,
    (&&),
    (==)
  )

-- This is the policy that allows or not the minting of new shame tokens
-- Shame tokens are both a way of uniquely identifying logic scripts, as
-- well as, marking a bad actor's history
{-# INLINEABLE mkShameTokenPolicy #-}
mkShameTokenPolicy :: () -> ScriptContext -> Bool
mkShameTokenPolicy () ctx =
    traceIfFalse "Shame Token Policy - Transaction not signed by shame token user" (txSignedBy info pkh)
      && traceIfFalse "Shame Token Policy - Invalid logic value" validLogicValue
      && traceIfFalse "Shame Token Policy - Invalid logic datum" validLogicDatum
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- If there is only one token minted, constructs a
    -- Shame Token based on it's token name
    shame :: ShameToken
    shame = case flattenValue (txInfoMint info) of
      [(_, tn, amt)]
        | amt == 1 -> makeShameToken tn
      _ -> traceError "Shame Token Policy - Wrong token amount"

    pkh :: PubKeyHash
    pkh = stUser shame

    logValHash :: ValidatorHash
    logValHash = stScript shame

    -- Based on the shame token we built, create a value with our
    -- currency symbol, shame token and with an amount of one
    shameTokenValue :: Value
    shameTokenValue = shameValue (ownCurrencySymbol ctx) shame

    -- Based on the shame token we built, get's the validator hash
    -- embeded inside it and tries to find the script with this address
    logicOutput :: TxOut
    logicOutput = case strictFindOutputWithValHash logValHash info of
      Just out -> out
      Nothing -> traceError "Shame Token Policy - Couldn't find an unique logic output"

    -- Tries to find the datum from the script we found
    logicDatum :: LogicState
    logicDatum = case findLogicDatum logicOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError "Shame Token Policy - Logic Datum could not be found"

    -- Makes sure that minted shame token is transfered to the logic script
    validLogicValue :: Bool
    validLogicValue = txOutValue logicOutput == shameTokenValue

    -- Makes sure the datum is equal to the initial one
    validLogicDatum :: Bool
    validLogicDatum = logicDatum == LSWaitingStart

shameTokenPolicy :: Scripts.MintingPolicy
shameTokenPolicy =
  mkMintingPolicyScript
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy mkShameTokenPolicy||])

shameTokenCurrencySymbol :: CurrencySymbol
shameTokenCurrencySymbol = scriptCurrencySymbol shameTokenPolicy