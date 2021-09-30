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

module Membership.OffChain.Contract where

import Control.Monad (forever)
import qualified Data.Map as Map
import Data.Monoid as M (Last (Last), Monoid (mconcat))
import Data.Text (Text, pack)
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
import Membership.Contract
import Membership.OffChain.Utils
import Membership.OnChain.Account
import Membership.OnChain.Contract
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

createContract :: AccountSettings -> ContractDatum -> Contract (Last AssetClass) s Text ()
createContract accountSettings contractDatum = do
  pkh <- pubKeyHash <$> Contract.ownPubKey

  maybeAccountOffChainEssentials <- getAccountOffChainEssentials accountSettings pkh

  contractNFT <-
    mapError
      (pack . show)
      ( mintContract pkh [("contract-nft", 1)] ::
          Contract w s CurrencyError OneShotCurrency
      )

  case maybeAccountOffChainEssentials of
    Just aoe -> do
      -- Submit transaction
      ledgerTx <- submitTxConstraintsWith @AccountType lookups tx

      -- Update the last value to our currenct NFT created
      tell $ Last $ Just contractNFTAssetClass

      -- Wait until transaction is confirmed
      awaitTxConfirmed $ txId ledgerTx

      logInfo @String $
        "Create Contract - Contract succefully created " ++ show contractNFTAssetClass
      where
        sigSymbol :: CurrencySymbol
        sigSymbol = asSignatureSymbol accountSettings

        platformSettings :: PlatformSettings
        platformSettings = asPlatformSettings accountSettings

        accountReference :: TxOutRef
        accountReference = aoeAccountReference aoe

        accountDatum :: AccountDatum
        accountDatum = aoeAccountDatum aoe

        accountOutTx :: TxOutTx
        accountOutTx = aoeAccountOutTx aoe

        accountOutput :: TxOut
        accountOutput = aoeAccountOut aoe

        contractNFTSymbol :: CurrencySymbol
        contractNFTSymbol = Currency.currencySymbol contractNFT

        contractNFTAssetClass :: AssetClass
        contractNFTAssetClass = assetClass contractNFTSymbol "contract-nft"
        accValHash = accountValidatorHash accountSettings

        contrValHash :: ValidatorHash
        contrValHash = asContractValidatorHash accountSettings

        newAccountDatum :: AccountDatum
        newAccountDatum =
          addContract
            (contractCreationCAS (psCASMap platformSettings) accountDatum)
            contrValHash
            contractNFTAssetClass

        sigValue, txFeeValue, trustValue, contractValue, accountValue :: Value
        sigValue = singleton sigSymbol (makeSigToken pkh accValHash) 1
        txFeeValue = assetClassValue (psToken platformSettings) (psTxFee platformSettings)
        trustValue =
          assetClassValue
            (psToken platformSettings)
            (sTrust $ cdService contractDatum)
        contractValue =
          sigValue <> Currency.mintedValue contractNFT <> trustValue
        accountValue = txOutValue accountOutput <> txFeeValue <> negate sigValue

        lookups :: ScriptLookups AccountType
        lookups = accountLookups accountSettings accountReference accountOutTx

        tx :: TxConstraints (RedeemerType AccountType) (DatumType AccountType)
        tx =
          Constraints.mustBeSignedBy pkh
            <> spendAccount accountReference ACreateContract
            <> Constraints.mustPayToTheScript newAccountDatum accountValue
            <> Constraints.mustPayToOtherScript
              contrValHash
              (Datum $ PlutusTx.toBuiltinData contractDatum)
              contractValue
    Nothing -> logError @String "Create Contract - Contract Essentials failed"

signContract :: Role -> AccountSettings -> AssetClass -> Contract w s Text ()
signContract userRole accountSettings contractNFT = do
  -- The signer public key hash
  pkh <- pubKeyHash <$> Contract.ownPubKey

  -- Tries to get the account off-chain essentials
  maybeAccountOffChainEssentials <- getAccountOffChainEssentials accountSettings pkh

  -- Tries to get the contract off-chain essentials
  maybeContractOffChainEssentials <- getContractOffChainEssentials accountSettings contractNFT

  case (maybeAccountOffChainEssentials, maybeContractOffChainEssentials) of
    (Just aoe, Just coe) -> do
      -- Submit transaction
      ledgerTx <- submitTxConstraintsWith @ContractType lookups tx

      -- Wait until transaction is confirmed
      awaitTxConfirmed $ txId ledgerTx

      logInfo @String $
        "Sign Contract - Successfully signed contract "
          ++ show contractNFT
          ++ " by user "
          ++ show pkh
      where
        sigSymbol :: CurrencySymbol
        sigSymbol = asSignatureSymbol accountSettings

        platformSettings :: PlatformSettings
        platformSettings = asPlatformSettings accountSettings

        accValHash :: ValidatorHash
        accValHash = accountValidatorHash accountSettings

        contrValHash :: ValidatorHash
        contrValHash = coeContractValidatorHash coe

        accountOut, contractOut :: TxOut
        accountOut = aoeAccountOut aoe
        contractOut = coeContractOut coe

        accountOutTx, contractOutTx :: TxOutTx
        accountOutTx = aoeAccountOutTx aoe
        contractOutTx = coeContractOutTx coe

        accountDatum :: AccountDatum
        accountDatum = aoeAccountDatum aoe

        contractDatum :: ContractDatum
        contractDatum = coeContractDatum coe

        accountReference, contractReference :: TxOutRef
        accountReference = aoeAccountReference aoe
        contractReference = coeContractReference coe

        contractSett :: ContractSettings
        contractSett = coeContractSettings coe

        sigValue, trustValue, priceValue, txFeesValue :: Value
        sigValue = singleton sigSymbol (makeSigToken pkh accValHash) 1
        trustValue = assetClassValue (psToken platformSettings) (sTrust $ cdService contractDatum)
        priceValue = case sType (cdService contractDatum) of
          OneTime p _
            | userRole == Client -> p
          _ -> mempty
        txFeesValue = assetClassValue (psToken platformSettings) (psTxFee platformSettings)

        contractValue, accountValue :: Value
        contractValue = txOutValue contractOut <> trustValue <> sigValue <> priceValue
        accountValue = txOutValue accountOut <> txFeesValue <> negate sigValue

        newContractDatum :: ContractDatum
        newContractDatum = addUser pkh userRole contractDatum

        newAccountDatum :: AccountDatum
        newAccountDatum =
          signContractCAS
            (psCASMap $ asPlatformSettings accountSettings) 
            (addContract accountDatum contrValHash contractNFT)

        lookups :: ScriptLookups ContractType
        lookups =
          Constraints.unspentOutputs
            ( Map.fromList
                [ (accountReference, accountOutTx),
                  (contractReference, contractOutTx)
                ]
            )
            <> Constraints.otherScript (accountValidator accountSettings)
            <> Constraints.otherScript (contractValidator contractSett)
            <> Constraints.typedValidatorLookups (typedContractValidator contractSett)

        tx :: TxConstraints (RedeemerType ContractType) (DatumType ContractType)
        tx =
          Constraints.mustBeSignedBy pkh
            <> Constraints.mustSpendScriptOutput
              accountReference
              (Redeemer $ PlutusTx.toBuiltinData ASign)
            <> Constraints.mustSpendScriptOutput
              contractReference
              (Redeemer $ PlutusTx.toBuiltinData CSign)
            <> Constraints.mustPayToTheScript newContractDatum contractValue
            <> Constraints.mustPayToOtherScript
              accValHash
              (Datum $ PlutusTx.toBuiltinData newAccountDatum)
              accountValue
    _ -> logError @String "Sign Contract - Account or Contract Essentials failed"

-- TODO : Refactor this
cancelContract :: Bool -> AccountSettings -> AssetClass -> Contract w s Text ()
cancelContract involvedInAccusation accountSettings contractNFT = do
  -- The signer public key hash
  pkh <- pubKeyHash <$> Contract.ownPubKey

  -- Tries to get the account that belongs to this user
  maybeAccount <- findAccount pkh accountSettings

  -- Tries to get the contract corresponding to this NFT
  maybeContract <- findContract contractNFT (asContractValidatorHash accountSettings)

  case (maybeAccount, maybeContract) of
    (Just (accountReference, accountOutTx), Just (contractReference, contractOutTx)) -> do
      let accountTx = txOutTxTx accountOutTx
          accountOut = txOutTxOut accountOutTx
          contractTx = txOutTxTx contractOutTx
          contractOut = txOutTxOut contractOutTx

          maybeAccountDatum = findAccountDatum accountOut (lookupDatum accountTx)
          maybeContractDatum = findContractDatum contractOut (lookupDatum contractTx)

      case (maybeAccountDatum, maybeContractDatum) of
        (Just accountDatum, Just contractDatum) -> do
          let sigSym = asSignatureSymbol accountSettings
              platformSettings = asPlatformSettings accountSettings
              accValHash = accountValidatorHash accountSettings
              contrValHash = asContractValidatorHash accountSettings

              priceValue
                | involvedInAccusation = mempty
                | otherwise = case sType $ cdService contractDatum of
                  CConstant -> mempty
                  OneTime v _ -> v
              sigValue = singleton sigSym (makeSigToken pkh accValHash) 1
              trustValue
                | involvedInAccusation = mempty
                | otherwise = assetClassValue (psToken platformSettings) (sTrust $ cdService contractDatum)

              contractValue = txOutValue contractOut <> negate (sigValue <> trustValue <> priceValue)
              accountValue = txOutValue accountOut <> sigValue
              userValue = trustValue <> priceValue

              contractSett = ContractSettings
                    { csPlatformSettings = platformSettings,
                      csSignatureSymbol = sigSym
                    }
              newContractDatum = removeUser pkh contractDatum
              newAccountDatum = removeContract accountDatum contrValHash

              lookups =
                Constraints.unspentOutputs
                  ( Map.fromList
                      [ (accountReference, accountOutTx),
                        (contractReference, contractOutTx)
                      ]
                  )
                  <> Constraints.otherScript (accountValidator accountSettings)
                  <> Constraints.otherScript (contractValidator contractSett)
                  <> Constraints.typedValidatorLookups (typedContractValidator contractSett)
              tx =
                Constraints.mustBeSignedBy pkh
                  <> Constraints.mustSpendScriptOutput
                    accountReference
                    (Redeemer $ PlutusTx.toBuiltinData (AReturn ARTCancel))
                  <> Constraints.mustSpendScriptOutput
                    contractReference
                    (Redeemer $ PlutusTx.toBuiltinData CCancel)
                  <> Constraints.mustPayToTheScript
                    newContractDatum
                    contractValue
                  <> Constraints.mustPayToOtherScript
                    accValHash
                    (Datum $ PlutusTx.toBuiltinData newAccountDatum)
                    accountValue
                  <> Constraints.mustPayToPubKey pkh userValue
          _ <- submitTxConstraintsWith @ContractType lookups tx
          logInfo @String (show $ cdRoleMap newContractDatum)
          logInfo @String $
            "Cancel Contract - Successfully canceled contract " ++ show contractNFT
        _ -> logError @String "Cancel Contract - Account or contract datum not found"
    (Nothing, _) -> logError @String "Cancel Contract - Account not found"
    _ -> logError @String "Cancel Contract - Contract not found"



leaveContract :: AccountSettings -> AssetClass -> Contract w s Text ()
leaveContract accountSettings contractNFT = do
  -- The signer public key hash
  pkh <- pubKeyHash <$> Contract.ownPubKey

  -- Tries to get the account off-chain essentials
  maybeAccountOffChainEssentials <- getAccountOffChainEssentials accountSettings pkh

  -- Tries to get the contract off-chain essentials
  maybeContractOffChainEssentials <- getContractOffChainEssentials accountSettings contractNFT

  case (maybeAccountOffChainEssentials, maybeContractOffChainEssentials) of
    (Just aoe, Just coe) -> do
      -- Submit transaction
      ledgerTx <- submitTxConstraintsWith @ContractType lookups tx

      -- Wait until transaction is confirmed
      awaitTxConfirmed $ txId ledgerTx

      logInfo @String $
        "Leave Contract - Successfully left contract " ++ show contractNFT
      
      where
        sigSymbol :: CurrencySymbol
        sigSymbol = asSignatureSymbol accountSettings

        platformSettings :: PlatformSettings
        platformSettings = asPlatformSettings accountSettings

        accValHash, contrValHash :: ValidatorHash
        accValHash = accountValidatorHash accountSettings
        contrValHash = asContractValidatorHash accountSettings

        accountOut, contractOut :: TxOut
        accountOut = aoeAccountOut aoe
        contractOut = coeContractOut coe

        accountOutTx, contractOutTx :: TxOutTx
        accountOutTx = aoeAccountOutTx aoe
        contractOutTx = coeContractOutTx coe

        accountDatum :: AccountDatum
        accountDatum = aoeAccountDatum aoe

        contractDatum :: ContractDatum
        contractDatum = coeContractDatum coe

        accountReference, contractReference :: TxOutRef
        accountReference = aoeAccountReference aoe
        contractReference = coeContractReference coe

        contractSett :: ContractSettings
        contractSett = coeContractSettings coe

        sigValue, trustValue, txFeesValue :: Value
        sigValue = singleton sigSymbol (makeSigToken pkh accValHash) 1
        trustValue = assetClassValue (psToken platformSettings) (sTrust $ cdService contractDatum)
        txFeesValue = assetClassValue (psToken platformSettings) (psTxFee platformSettings)

        contractValue, accountValue :: Value
        contractValue = txOutValue contractOut <> negate trustValue <> negate sigValue
        accountValue = txOutValue accountOut <> sigValue <> txFeesValue

        newContractDatum :: ContractDatum
        newContractDatum = removeUser pkh contractDatum

        newAccountDatum :: AccountDatum
        newAccountDatum =
          leaveContractCAS
            (psCASMap platformSettings)
            (removeContract accountDatum contrValHash)

        lookups :: ScriptLookups ContractType
        lookups =
          Constraints.unspentOutputs
            ( Map.fromList
                [ (accountReference, accountOutTx),
                  (contractReference, contractOutTx)
                ]
            )
            <> Constraints.otherScript (accountValidator accountSettings)
            <> Constraints.otherScript (contractValidator contractSett)
            <> Constraints.typedValidatorLookups (typedContractValidator contractSett)

        tx :: TxConstraints (RedeemerType ContractType) (DatumType ContractType)
        tx =
          Constraints.mustBeSignedBy pkh
            <> Constraints.mustSpendScriptOutput
              accountReference
              (Redeemer $ PlutusTx.toBuiltinData (AReturn ARTLeave))
            <> Constraints.mustSpendScriptOutput
              contractReference
              (Redeemer $ PlutusTx.toBuiltinData CLeave)
            <> Constraints.mustPayToTheScript newContractDatum contractValue
            <> Constraints.mustPayToOtherScript
              accValHash
              (Datum $ PlutusTx.toBuiltinData newAccountDatum)
              accountValue

    _ -> logError @String "Leave Contract - Account or Contract Essentials failed"

reviewContract :: Integer -> BuiltinByteString -> AccountSettings -> AssetClass -> Contract w s Text ()
reviewContract review description accountSettings contractNFT = do
  logInfo @String "Review Contract - Incomplete"

type ContractSchema =
  Endpoint "create-contract" (AccountSettings, ContractDatum)
    .\/ Endpoint "sign" (Role, AccountSettings, AssetClass)
    .\/ Endpoint "cancel" (Bool, AccountSettings, AssetClass)
    .\/ Endpoint "leave" (AccountSettings, AssetClass)
    .\/ Endpoint "review" (Integer, BuiltinByteString, AccountSettings, AssetClass)

contractEndpoints :: Contract (Last AssetClass) ContractSchema Text ()
contractEndpoints =
  forever $
    handleError logError $
      awaitPromise $
        create' `select` sign' `select` cancel' `select` leave' `select` review'
  where
    create' = endpoint @"create-contract" $ \(as, dat) -> createContract as dat
    sign' = endpoint @"sign" $ \(r, as, ac) -> signContract r as ac
    cancel' = endpoint @"cancel" $ \(invInAcc, as, ac) -> cancelContract invInAcc as ac
    leave' = endpoint @"leave" $ uncurry leaveContract
    review' = endpoint @"review" $ \(rev, des, as, ac) -> reviewContract rev des as ac