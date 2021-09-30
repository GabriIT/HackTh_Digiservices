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

module Membership.OffChain.Account where

import qualified Control.Monad as Monad
import qualified Data.Map as Map
import Data.Monoid (Last (Last))
import Data.Monoid as M (Last (Last), Monoid (mconcat))
import Data.Text (Text)
import qualified Ledger as L
import Ledger.Constraints as Constraints
import Ledger.Contexts (pubKeyHash)
import Ledger.Scripts
import Ledger.Typed.Scripts (ValidatorTypes (..))
import Ledger.Value as Value
  ( AssetClass,
    CurrencySymbol,
    TokenName,
    Value,
    assetClassValue,
    singleton,
  )
import Membership.OffChain.Utils
import Membership.OnChain.Account
import Membership.OnChain.Contract
import Membership.OnChain.Signature
import Membership.PlatformSettings
import Membership.Signature (makeSigToken)
import Plutus.Contract as Contract
  ( Contract,
    Endpoint,
    Promise (awaitPromise),
    awaitTxConfirmed,
    endpoint,
    handleError,
    logError,
    logInfo,
    ownPubKey,
    select,
    submitTxConstraintsWith,
    tell,
    type (.\/),
  )
import qualified PlutusTx
import PlutusTx.Prelude (Maybe (Just), length, return, ($), (++), (<$>), (<>))
import Text.Printf (printf)
import qualified Prelude as P

import Membership.Account

type AccountSchema =
  Endpoint "create-account" PlatformSettings
    .\/ Endpoint "collect-fees" AccountSettings

getAccountSettings :: PlatformSettings -> AccountSettings
getAccountSettings platformSettings =
  AccountSettings
    { asPlatformSettings = platformSettings,
      asSignatureSymbol = sigSymbol,
      asContractValidatorHash = contrValHash,
      asCollectors = [] -- TODO: Change that later
    }
  where
    token :: AssetClass
    token = psToken platformSettings

    sigSymbol :: CurrencySymbol
    sigSymbol = signatureCurrencySymbol platformSettings

    contrSett :: ContractSettings
    contrSett =
      ContractSettings
        { csPlatformSettings = platformSettings,
          csSignatureSymbol = sigSymbol
        }

    contrValHash :: ValidatorHash
    contrValHash = validatorHash $ contractValidator contrSett

createAccount :: PlatformSettings -> Contract () AccountSchema Text ()
createAccount ps = do
  pkh <- pubKeyHash <$> Contract.ownPubKey

  let fees :: Value
      fees = assetClassValue (psToken ps) (psEntranceFee ps)

      sigSymbol :: CurrencySymbol
      sigSymbol = signatureCurrencySymbol ps

      accValHash :: ValidatorHash
      accValHash = accountValidatorHash accountSettings

      sigTokenName :: TokenName
      sigTokenName = makeSigToken pkh accValHash

      accountSettings :: AccountSettings
      accountSettings = getAccountSettings ps

      mintVal :: Value
      mintVal = singleton sigSymbol sigTokenName 100

      lookups :: ScriptLookups AccountType
      lookups = Constraints.mintingPolicy (signaturePolicy ps)

      tx :: TxConstraints (RedeemerType AccountType) (DatumType AccountType)
      tx =
        Constraints.mustMintValue mintVal
          P.<> Constraints.mustPayToOtherScript
            accValHash
            (Datum $ PlutusTx.toBuiltinData initDatum)
            (mintVal <> fees)
  
  ledgerTx <- submitTxConstraintsWith @AccountType lookups tx

  Monad.void $ awaitTxConfirmed $ L.txId ledgerTx

  Contract.logInfo @P.String $ printf "%s successfully created account" (P.show pkh)

collectFees :: AccountSettings -> Contract w s Text ()
collectFees accountSettings = do
  -- Get all accounts located at the address
  accounts <- findAccounts accountSettings

  case accounts of
    [] -> logInfo @P.String "Collect Fees - No accounts found"
    _ -> do
      -- Submit transaction with the defined constraints
      ledgerTx <- submitTxConstraintsWith @AccountType lookups tx

      -- Wait until the transaction is confirmed
      awaitTxConfirmed $ L.txId ledgerTx

      -- Log success message
      logInfo @P.String $
        "Collect Fees - Successfully collected from "
          ++ P.show (length accounts)
          ++ " account(s)"
      where
        lookups :: ScriptLookups AccountType
        lookups =
          Constraints.unspentOutputs
            (Map.fromList [(auiReference aui, auiOutTx aui) | aui <- accounts])
            P.<> Constraints.otherScript (accountValidator accountSettings)

        tx :: TxConstraints (RedeemerType AccountType) (DatumType AccountType)
        tx =
          M.mconcat
            [ Constraints.mustSpendScriptOutput
                (auiReference aui)
                $ Redeemer $ PlutusTx.toBuiltinData (ACollect (auiUser aui))
              | aui <- accounts
            ]
            P.<> M.mconcat
              [ Constraints.mustPayToOtherScript
                  (accountValidatorHash accountSettings)
                  (Datum $ PlutusTx.toBuiltinData (auiDatum aui))
                  (auiFees aui)
                | aui <- accounts
              ]

accountEndpoints :: Contract () AccountSchema Text ()
accountEndpoints =
  Monad.forever $
    handleError logError $
      awaitPromise $
        createAccount' `select` collectFees'
  where
    createAccount' = endpoint @"create-account" $ \ps -> createAccount ps
    collectFees' = endpoint @"collect-fees" $ \as -> collectFees as