{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Membership.OffChain.Utils where

import Control.Monad (forever)
-- import Membership.OnChain.Contract

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import Data.Monoid as M (Last (Last), Monoid (mconcat))
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Ledger
  ( Datum (Datum),
    PubKeyHash,
    Redeemer (Redeemer),
    TxOut (txOutValue),
    TxOutRef,
    TxOutTx (..),
    ValidatorHash,
    lookupDatum,
    pubKeyHash,
    txId,
  )
import Ledger.Constraints as Constraints
  ( ScriptLookups,
    TxConstraints,
    mustBeSignedBy,
    mustPayToOtherScript,
    mustPayToPubKey,
    mustPayToTheScript,
    mustSpendScriptOutput,
    otherScript,
    typedValidatorLookups,
    unspentOutputs,
  )
import Ledger.Typed.Scripts (ValidatorTypes (..))
import Ledger.Value
  ( AssetClass (..),
    CurrencySymbol,
    Value,
    assetClass,
    assetClassValue,
    assetClassValueOf,
    geq,
    singleton,
  )
import Membership.Account
  ( AccountDatum (..),
    AccountOffChainEssentials (..),
    AccountRedeemer (..),
    AccountType,
    addContract,
    addReview,
    contractCreationCAS,
    findAccountDatum,
    removeContract,
    signContractCAS,
  )
import Membership.Contract
import Membership.Logic
import Membership.OnChain.Account
import Membership.OnChain.Logic
import Membership.PlatformSettings
import Membership.Service
import Membership.Signature (findSignatories, findSignatory, makeSigToken)
import Plutus.Contract as Contract
  ( Contract,
    Endpoint,
    Promise (awaitPromise),
    awaitTxConfirmed,
    currentTime,
    endpoint,
    handleError,
    logError,
    logInfo,
    mapError,
    ownPubKey,
    select,
    submitTxConstraintsWith,
    tell,
    utxoAt,
    type (.\/),
  )
import Plutus.Contracts.Currency as Currency
  ( CurrencyError,
    OneShotCurrency,
    currencySymbol,
    mintContract,
    mintedValue,
  )
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AM
import PlutusTx.Prelude
  ( AdditiveGroup ((-)),
    Bool,
    BuiltinByteString,
    Eq ((==)),
    Integer,
    Maybe (..),
    filter,
    foldr,
    length,
    map,
    mempty,
    negate,
    otherwise,
    return,
    traceError,
    ($),
    (*),
    (++),
    (.),
    (/=),
    (<),
    (<$>),
    (>),
    (||),
  )
import qualified PlutusTx.Ratio as R
import Wallet.Emulator.Wallet ()
import Prelude (Semigroup (..), Show (..), String, uncurry)
import qualified Prelude as P

data AccountUTxOInfo = AccountUTxOInfo
  { auiReference :: TxOutRef,
    auiOutTx :: TxOutTx,
    auiDatum :: AccountDatum,
    auiFees :: Value,
    auiUser :: PubKeyHash
  }
  deriving (P.Show, Generic, FromJSON, ToJSON, P.Eq)

-- Returns all accounts, their Datum, the Value of fees they hold and their owners
findAccounts :: AccountSettings -> Contract w s Text [AccountUTxOInfo]
findAccounts accountSettings = do
  -- All UTxOs located at the account address
  utxos <- utxoAt (accountAddress accountSettings)

  -- Return these UTxOs altered
  return $ foldr filterUTxO [] (Map.toList utxos)
  where
    platformSettings :: PlatformSettings
    platformSettings = asPlatformSettings accountSettings

    sigSymbol :: CurrencySymbol
    sigSymbol = asSignatureSymbol accountSettings

    -- The number of DSET tokens located in a specific transaction output
    dsetTokens :: TxOutTx -> Integer
    dsetTokens o =
      assetClassValueOf
        (txOutValue $ txOutTxOut o)
        (psToken platformSettings)

    -- The public key hash from a user with a SIG token in this value
    sigPubKeys :: Value -> [PubKeyHash]
    sigPubKeys v = findSignatories sigSymbol v

    getAccountInfo :: (TxOutRef, TxOutTx) -> Maybe AccountUTxOInfo
    getAccountInfo (oref, outTx) = do
      pkh <-
        ( case sigPubKeys (txOutValue $ txOutTxOut outTx) of
            [p] -> Just p
            _ -> Nothing
          )
      accountDatum <- findAccountDatum (txOutTxOut outTx) (lookupDatum (txOutTxTx outTx))

      let reviewCreditValue :: Value
          reviewCreditValue =
            assetClassValue
              (psToken platformSettings)
              (dsetTokens outTx - adReviewCredit accountDatum)

          fees :: Value
          fees = txOutValue (txOutTxOut outTx) <> negate reviewCreditValue

      return
        AccountUTxOInfo
          { auiReference = oref,
            auiOutTx = outTx,
            auiDatum = accountDatum,
            auiFees = fees,
            auiUser = pkh
          }

    filterUTxO ::
      (TxOutRef, TxOutTx) ->
      [AccountUTxOInfo] ->
      [AccountUTxOInfo]
    filterUTxO (oref, o) acc = case getAccountInfo (oref, o) of
      Just aui -> aui : acc
      Nothing -> acc

-- Return the UTxO from the account with a PubKeyHash
findAccount :: PubKeyHash -> AccountSettings -> Contract w s Text (Maybe (TxOutRef, TxOutTx))
findAccount user accountSettings = do
  -- All UTxOs located at the account address
  utxos <- Map.filter f <$> utxoAt (accountAddress accountSettings)

  -- Return UTxO if there is only one UTxO in the filtered list
  return $ case Map.toList utxos of
    [(oref, o)] -> Just (oref, o)
    _ -> Nothing
  where
    -- Only return UTxOs that contain a SIG values, whose signatory is the given user
    f :: TxOutTx -> Bool
    f o = findSignatories (asSignatureSymbol accountSettings) (txOutValue $ txOutTxOut o) == [user]

getAccountOffChainEssentials :: AccountSettings -> PubKeyHash -> Contract w s Text (Maybe AccountOffChainEssentials)
getAccountOffChainEssentials accountSettings pkh = do
  -- Tries to get the account that belongs to this user
  maybeAccount <- findAccount pkh accountSettings

  case maybeAccount of
    Just (accountReference, accountOutTx) -> do
      let accountTx = txOutTxTx accountOutTx
          accountOut = txOutTxOut accountOutTx

          maybeAccountDatum = findAccountDatum accountOut (lookupDatum accountTx)

      case maybeAccountDatum of
        Just accountDatum -> do
          return $
            Just
              AccountOffChainEssentials
                { aoeAccountReference = accountReference,
                  aoeAccountOutTx = accountOutTx,
                  aoeAccountTx = accountTx,
                  aoeAccountOut = accountOut,
                  aoeAccountDatum = accountDatum
                }
        _ -> do
          logError @String "Get Account Off-Chain Essentials - Account Datum not found"
          return Nothing
    _ -> do
      logError @String "Get Account Off-Chain Essentials - Account not found"
      return Nothing

getContractOffChainEssentials :: AccountSettings -> AssetClass -> Contract w s Text (Maybe ContractOffChainEssentials)
getContractOffChainEssentials accountSettings contractNFT = do
  -- Tries to get the contract corresponding to this NFT
  maybeContract <- findContract contractNFT contrValHash

  case maybeContract of
    Just (contractReference, contractOutTx) -> do
      let contractTx = txOutTxTx contractOutTx
          contractOut = txOutTxOut contractOutTx

          maybeContractDatum = findContractDatum contractOut (lookupDatum contractTx)

      case maybeContractDatum of
        Just contractDatum -> do
          return $
            Just
              ContractOffChainEssentials
                { coeContractReference = contractReference,
                  coeContractOutTx = contractOutTx,
                  coeContractTx = txOutTxTx contractOutTx,
                  coeContractOut = txOutTxOut contractOutTx,
                  coeContractDatum = contractDatum,
                  coeContractSettings = ContractSettings
                    { csPlatformSettings = platformSettings,
                      csSignatureSymbol = sigSym
                    },
                  coeContractValidatorHash = contrValHash
                }
        _ -> do
          logError @String "Get Contract Off-Chain Essentials - Contract Datum not found"
          return Nothing
    _ -> do
      logError @String "Get Contract Off-Chain Essentials - Contract not found"
      return Nothing
  where
    contrValHash :: ValidatorHash
    contrValHash = asContractValidatorHash accountSettings

    platformSettings :: PlatformSettings
    platformSettings = asPlatformSettings accountSettings

    sigSym :: CurrencySymbol
    sigSym = asSignatureSymbol accountSettings

getLogicOffChainEssentials :: LogicSettings -> AssetClass -> Contract w s Text (Maybe LogicOffChainEssentials)
getLogicOffChainEssentials logicSettings shameToken = do
  -- The signer public key hash
  pkh <- pubKeyHash <$> Contract.ownPubKey

  -- Tries to get the logic corresponding to the logic creator and this validator hash
  maybeLogic <- findLogic shameToken logValHash

  case maybeLogic of
    Just (logicReference, logicOutTx) -> do
      let logicTx = txOutTxTx logicOutTx
          logicOut = txOutTxOut logicOutTx

          maybeLogicDatum = findLogicDatum logicOut (lookupDatum logicTx)

      case maybeLogicDatum of
        Just logicDatum -> do
          return $
            Just
              LogicOffChainEssentials
                { loeLogicReference = logicReference,
                  loeLogicOutTx = logicOutTx,
                  loeLogicTx = txOutTxTx logicOutTx,
                  loeLogicOut = txOutTxOut logicOutTx,
                  loeLogicDatum = logicDatum,
                  loeLogicValidatorHash = logValHash
                }
        _ -> do
          logError @String "Get Logic Off-Chain Essentials - Logic Datum not found"
          return Nothing
    _ -> do
      logError @String "Get Logic Off-Chain Essentials - Logic not found"
      return Nothing
  where
    logValHash :: ValidatorHash
    logValHash = logicValHash logicSettings

accountLookups ::
  AccountSettings ->
  TxOutRef ->
  TxOutTx ->
  ScriptLookups AccountType
accountLookups as oref oTx =
  Constraints.unspentOutputs (Map.singleton oref oTx)
    <> Constraints.typedValidatorLookups (typedAccountValidator as)
    <> Constraints.otherScript (accountValidator as)

spendAccount ::
  TxOutRef ->
  AccountRedeemer ->
  TxConstraints (RedeemerType AccountType) (DatumType AccountType)
spendAccount oref r = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData r)

spendContract ::
  TxOutRef ->
  ContractRedeemer ->
  TxConstraints (RedeemerType ContractType) (DatumType ContractType)
spendContract oref r = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData r)