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

module Membership.OnChain.Account where

import Ledger
  ( Address,
    PubKeyHash,
    ScriptContext (scriptContextTxInfo),
    TxInfo,
    TxOut (txOutValue),
    Validator,
    ValidatorHash,
    findDatum,
    ownHash,
    scriptAddress,
    txSignedBy,
    validatorHash,
  )
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value
  ( AssetClass (AssetClass),
    CurrencySymbol,
    TokenName (TokenName),
    Value,
    assetClassValue,
    assetClassValueOf,
    flattenValue,
    geq,
  )
import Membership.Account
  ( AccountDatum (..),
    AccountRedeemer (..),
    AccountReturnType (..),
    AccountType,
    addContract,
    removeContract,
    contractCreationCAS,
    declaredGuiltyCAS,
    findAccountDatum,
    signContractCAS,
    cancelContractCAS,
    leaveContractCAS
  )
import Membership.Contract
  ( Accusation (..),
    ContractDatum (..),
    findContractDatum,
    isInitial,
    findContractNFT
  )
import Membership.Logic
  ( LogicState (..),
    findLogicDatum,
    findShameTokenAssetClass,
  )
import Membership.OnChain.Utils
  ( sigTokenInContext,
    strictFindOutAndIn,
  )
import Membership.PlatformSettings
import Membership.Service
import Membership.Signature
import Membership.Utils
  ( strictFindInputWithValHash,
    strictFindOutputWithValHash,
  )
import qualified PlutusTx
import qualified PlutusTx.AssocMap as M
import PlutusTx.Prelude
  ( Bool (..),
    Eq ((==)),
    Integer,
    Maybe (..),
    find,
    fst,
    negate,
    return,
    snd,
    traceError,
    traceIfFalse,
    ($),
    (&&),
    (.),
    (||),
    otherwise,
    null,
  )
import Prelude (Semigroup (..))

{-# INLINEABLE contractNFTTokenName #-}
contractNFTTokenName :: TokenName
contractNFTTokenName = TokenName "contract-nft"

{-# INLINEABLE findContractInput #-}
findContractInput :: ValidatorHash -> TxInfo -> Maybe (ContractDatum, Value)
findContractInput contrValHash info = do
  o <- strictFindInputWithValHash contrValHash info
  d <- findContractDatum o (`findDatum` info)
  return (d, txOutValue o)

{-# INLINEABLE findContractOutput #-}
findContractOutput :: ValidatorHash -> TxInfo -> Maybe (ContractDatum, Value)
findContractOutput contrValHash info = do
  o <- strictFindOutputWithValHash contrValHash info
  d <- findContractDatum o (`findDatum` info)
  return (d, txOutValue o)

{-# INLINEABLE validateAccount #-}
validateAccount :: AccountSettings -> ScriptContext -> Bool
validateAccount (AccountSettings _ sigSymbol _ _) ctx =
  traceIfFalse "Account - SIG token missing in input" (sigTokenInContext sigToken ctx)
    && traceIfFalse "Account - Transaction not signed by account owner" (txSignedBy info pkh)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx

    pkh :: PubKeyHash
    pkh = case findSignatory sigSymbol (txOutValue ownInput) of
      Just pkh' -> pkh'
      Nothing -> traceError "Account - No SIG token found in input"

    sigToken :: AssetClass
    sigToken = signatureAssetClass sigSymbol pkh (ownHash ctx)

{-# INLINEABLE validateCreateContract #-}
validateCreateContract :: AccountSettings -> AccountDatum -> ScriptContext -> Bool
validateCreateContract accountSettings inputDatum ctx =
  traceIfFalse "Account - Transaction not signed by account owner" (txSignedBy info pkh)
    && traceIfFalse "Account Create - Invalid account value" validAccountValue
    && traceIfFalse "Account Create - Invalid account datum" validAccountDatum
    && traceIfFalse "Account Create - Invalid contract value" validContractValue
    && traceIfFalse "Account Create - Invalid contract datum" validContractDatum
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    platformSettings :: PlatformSettings
    platformSettings = asPlatformSettings accountSettings

    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    outputDatum :: AccountDatum
    outputDatum = case findAccountDatum ownOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError "Account Create - Output Datum not found"

    sigSymbol :: CurrencySymbol
    sigSymbol = asSignatureSymbol accountSettings

    ownSig :: Sig
    ownSig = case findSignatures sigSymbol (txOutValue ownInput) of
      [s]
        | sScript s == ownHash ctx -> s
        | otherwise -> traceError "Account Create - Invalid SIG hash"
      _ -> traceError "Account Create - Should only have one SIG"
    
    pkh :: PubKeyHash
    pkh = sUser ownSig
    
    sigValue :: Value
    sigValue = signatureValue' sigSymbol ownSig

    transactionFeeValue :: Value
    transactionFeeValue =
      assetClassValue
        (psToken platformSettings)
        (psTxFee platformSettings)
    
    contrValHash :: ValidatorHash
    contrValHash = asContractValidatorHash accountSettings

    contractOutput :: TxOut
    contractOutput = case strictFindOutputWithValHash contrValHash info of
      Just o -> o
      Nothing -> traceError "Account Create - Couldn't find an unique contract output"
    
    contractDatum :: ContractDatum
    contractDatum = case findContractDatum contractOutput (`findDatum` info) of
        Just dat -> dat
        Nothing -> traceError "Account Expelled - Contract Datum could not be found"

    contractNFT :: AssetClass
    contractNFT = case findContractNFT (txOutValue contractOutput) of
      Just ac -> ac
      Nothing -> traceError "Account Create - No NFT found inside contract"

    contractNFTValue :: Value
    contractNFTValue = assetClassValue contractNFT 1

    trustValue :: Value
    trustValue =
      assetClassValue
        (psToken platformSettings)
        (sTrust $ cdService contractDatum)

    expectedOutputDatum :: AccountDatum
    expectedOutputDatum =
      addContract
        (contractCreationCAS (psCASMap platformSettings) inputDatum)
        contrValHash
        contractNFT

    validAccountValue :: Bool
    validAccountValue =
      txOutValue ownOutput <> negate (txOutValue ownInput)
        == transactionFeeValue <> negate (signatureValue sigSymbol pkh (ownHash ctx))
    
    validAccountDatum :: Bool
    validAccountDatum = outputDatum == expectedOutputDatum

    validContractValue :: Bool
    validContractValue =
      txOutValue contractOutput
        == sigValue
        <> trustValue
        <> contractNFTValue
    
    validContractDatum :: Bool
    validContractDatum =
      null (cdAccusations contractDatum)
        && isInitial pkh contractDatum

{-# INLINEABLE validateSign #-}
validateSign :: AccountSettings -> AccountDatum -> ScriptContext -> Bool
validateSign accountSettings inputDatum ctx =
  traceIfFalse "Account Sign - Invalid Account" (validateAccount accountSettings ctx)
    && traceIfFalse "Account Sign - SIG token was not paid to the contract" sigPaid
    && traceIfFalse "Account Sign - Final value does not match" valueMatches
    && traceIfFalse "Account Sign - Wrong account datum" validAccountDatum
    && traceIfFalse "Account Sign - Invalid contract" validContract
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    outputDatum :: AccountDatum
    outputDatum = case findAccountDatum ownOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError "Account Sign - Could not find output account datum"

    sigSymbol :: CurrencySymbol
    sigSymbol = asSignatureSymbol accountSettings

    contrValHash :: ValidatorHash
    contrValHash = asContractValidatorHash accountSettings

    platformToken :: AssetClass
    platformToken = (psToken . asPlatformSettings) accountSettings

    transactionFee :: Integer
    transactionFee = (psTxFee . asPlatformSettings) accountSettings

    transactionFeeValue :: Value
    transactionFeeValue = assetClassValue platformToken transactionFee

    pkh :: PubKeyHash
    pkh = case findSignatory sigSymbol (txOutValue ownInput) of
      Just pkh' -> pkh'
      Nothing -> traceError "Account Sign - No SIG token found in input"

    sigToken :: AssetClass
    sigToken = signatureAssetClass sigSymbol pkh (ownHash ctx)

    contractInput :: Maybe (ContractDatum, Value)
    contractInput = findContractInput contrValHash info

    contractOutput :: Maybe (ContractDatum, Value)
    contractOutput = findContractOutput contrValHash info

    sigPaid :: Bool
    sigPaid = case contractOutput of
      Just (_, val) -> assetClassValueOf val sigToken == 1
      _ -> False

    maybeContractNFTInput :: Maybe AssetClass
    maybeContractNFTInput = do
      (_, contractValue) <- contractInput
      (cs, tn, _) <- find (\(_, tn, amt) -> tn == contractNFTTokenName && amt == 1) (flattenValue contractValue)
      return $ AssetClass (cs, tn)

    maybeContractNFTOutput :: Maybe AssetClass
    maybeContractNFTOutput = do
      (_, contractValue) <- contractOutput
      (cs, tn, _) <- find (\(_, tn, amt) -> tn == contractNFTTokenName && amt == 1) (flattenValue contractValue)
      return $ AssetClass (cs, tn)

    validContract :: Bool
    validContract = case (maybeContractNFTInput, maybeContractNFTOutput) of
      (Just _, Just _) -> True
      _ -> False

    valueMatches :: Bool
    valueMatches =
      txOutValue ownOutput
        == ( txOutValue ownInput
               <> transactionFeeValue
               <> negate (signatureValue sigSymbol pkh (ownHash ctx))
           )

    -- Increase contracts NFT list
    validAccountDatum :: Bool
    validAccountDatum = case maybeContractNFTInput of
      Just contractNFT -> 
        signContractCAS
          (psCASMap $ asPlatformSettings accountSettings) 
          (addContract inputDatum contrValHash contractNFT)
            == outputDatum
      Nothing -> traceError "Account Sign - Contract NFT not found"

{-# INLINABLE validateCollect #-}
validateCollect :: AccountSettings -> PubKeyHash -> ScriptContext -> Bool
validateCollect _ _ _ = traceError "Validate Collect - Incomplete"

{-# INLINABLE validateReview #-}
validateReview :: AccountSettings -> AccountDatum -> ScriptContext -> Bool
validateReview _ _ _ = traceError "Validate Review - Incomplete"

-- Being Expelled ->
--    Account should be valid (at least one sig token and all
-- from the same person and with the right validator hash); X
--    The contract it's consuming should be in our list and should contain the NFT; X
--    The logic we are consuming should be selected in the contract datum; X
--    The logic we are consuming should have the shame token X
--    The value difference should be equal to the user SIG token plus (maybe) the shame token;
--    The datum should be the same, except:
--       The CAS should decrease according to the CASMap;
--       The contract consumed should be removed from the list;

validateReturn ::
  AccountSettings ->
  AccountReturnType ->
  AccountDatum ->
  ScriptContext ->
  Bool
validateReturn accountSettings ARTExpelled inputDatum ctx =
  traceIfFalse "Account Expelled - Invalid Contract" validContract
    && traceIfFalse "Account Expelled - Invalid Logic" validLogic
    && traceIfFalse "Account Expelled - Invalid Logic State" validLogicState
    && traceIfFalse "Account Expelled - Invalid Account" validAccount
    && traceIfFalse "Account Expelled - Invalid Account Value" validAccountValue
    && traceIfFalse "Account Expelled - Invalid Account Datum" validAccountDatum
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    accountDatum :: AccountDatum
    accountDatum = case findAccountDatum ownOutput (`findDatum` info) of
      Just ad -> ad
      Nothing -> traceError "Account Expelled - Could not find account output datum"
  
    sigSymbol :: CurrencySymbol
    sigSymbol = asSignatureSymbol accountSettings

    ownSig :: Sig
    ownSig = case findSignatures sigSymbol (txOutValue ownInput) of
      [s]
        | sScript s == ownHash ctx -> s
        | otherwise -> traceError "Account Expelled - Invalid SIG hash"
      _ -> traceError "Account Expelled - Should only have one SIG"
    
    pkh :: PubKeyHash
    pkh = sUser ownSig
    
    sigValue :: Value
    sigValue = signatureValue' sigSymbol ownSig

    contrValHash :: ValidatorHash
    contrValHash = asContractValidatorHash accountSettings

    contractInput :: TxOut
    contractInput = case strictFindInputWithValHash contrValHash info of
      Just out -> out
      Nothing -> traceError "Account Expelled - No contract being consumed"
    
    contractDatum :: ContractDatum
    contractDatum = case findContractDatum contractInput (`findDatum` info) of
        Just dat -> dat
        Nothing -> traceError "Account Expelled - Contract Datum could not be found"
    
    contractNFT :: AssetClass
    contractNFT = case M.lookup contrValHash (adContracts inputDatum) of
      Just ac -> ac
      Nothing -> traceError "Account Expelled - Contract not registered"
    
    logValHash :: ValidatorHash
    logValHash = cdLogicScript contractDatum

    logicInput :: TxOut
    logicInput = case strictFindInputWithValHash logValHash info of
      Just out -> out
      Nothing -> traceError "Account Expelled - No logic being consumed"

    logicDatum :: LogicState
    logicDatum = case findLogicDatum logicInput (`findDatum` info) of
        Just state -> state
        Nothing -> traceError "Account Expelled - Logic datum not found"
    
    accusation :: Accusation
    accusation = case logicDatum of
      (LSWaitingEnd _ _ acc _) -> acc

    shameTokenAssetClass :: AssetClass
    shameTokenAssetClass = case findShameTokenAssetClass
      (psShameTokenSymbol $ asPlatformSettings accountSettings)
      (txOutValue logicInput) of
        Just ac -> ac
        Nothing -> traceError "Account Expelled - Shame Token could not be found"

    shameTokenValue :: Value
    shameTokenValue = assetClassValue shameTokenAssetClass 1

    validContract :: Bool
    validContract = case findContractNFT (txOutValue contractInput) of
      Just ac -> ac == contractNFT
      Nothing -> False
    
    validLogic :: Bool
    validLogic = txOutValue logicInput `geq` shameTokenValue
    
    validLogicState :: Bool
    validLogicState = case logicDatum of
      LSWaitingEnd {} -> True
      _ -> False
    
    validAccount :: Bool
    validAccount = pkh == fst (aAccused accusation)

    validAccountValue :: Bool
    validAccountValue = 
      (valueDifference == sigValue <> shameTokenValue)
        || (valueDifference == sigValue)
      where
        valueDifference :: Value
        valueDifference = txOutValue ownOutput <> negate (txOutValue ownInput)
    
    validAccountDatum :: Bool
    validAccountDatum =
      accountDatum == declaredGuiltyCAS
        (psCASMap $ asPlatformSettings accountSettings)
        (removeContract inputDatum contrValHash)

-- Leaving or Cancelling -> 
--    Transactions should be signed by the account owner
--    Account should be valid (at least one sig token and all
-- from the same person and with the right validator hash);
--    The contract it's consuming should be in our list and should contain the NFT;
--    The value difference should be equal to the user SIG token;
--    The datum should be the same, except:
--       The CAS should increase or decrease according to the CASMap and the type;
--       The contract consumed should be removed from the list;
validateReturn accountSettings returnType inputDatum ctx =
  traceIfFalse "Account Leave - Not signed by account owner" (txSignedBy info pkh)
    && traceIfFalse "Account Leave - Invalid Account" validAccount
    && traceIfFalse "Account Leave - Invalid Account Value" validAccountValue
    && traceIfFalse "Account Leave - Invalid Account Datum" validAccountDatum
    && traceIfFalse "Account Leave - Invalid Contract" validContract
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    platformSettings :: PlatformSettings
    platformSettings = asPlatformSettings accountSettings

    casMap :: CASMap
    casMap = psCASMap platformSettings

    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    accountDatum :: AccountDatum
    accountDatum = case findAccountDatum ownOutput (`findDatum` info) of
      Just ad -> ad
      Nothing -> traceError "Account Leave - Could not find account output datum"
  
    sigSymbol :: CurrencySymbol
    sigSymbol = asSignatureSymbol accountSettings

    ownSig :: Sig
    ownSig = case findSignatures sigSymbol (txOutValue ownInput) of
      [s]
        | sScript s == ownHash ctx -> s
        | otherwise -> traceError "Account Leave - Invalid SIG hash"
      _ -> traceError "Account Leave - Should only have one SIG"
    
    pkh :: PubKeyHash
    pkh = sUser ownSig
    
    sigValue :: Value
    sigValue = signatureValue' sigSymbol ownSig

    transactionFeeValue :: Value
    transactionFeeValue =
      assetClassValue
        (psToken platformSettings)
        (psTxFee platformSettings)

    contrValHash :: ValidatorHash
    contrValHash = asContractValidatorHash accountSettings

    contractInput :: TxOut
    contractInput = case strictFindInputWithValHash contrValHash info of
      Just out -> out
      Nothing -> traceError "Account Leave - No contract being consumed"
    
    contractNFT :: AssetClass
    contractNFT = case M.lookup contrValHash (adContracts inputDatum) of
      Just ac -> ac
      Nothing -> traceError "Account Leave - Contract not registered"

    validAccount :: Bool
    validAccount = True -- Verified by creating ownSig already

    validAccountValue :: Bool
    validAccountValue =
      txOutValue ownOutput <> negate (txOutValue ownInput)
        == sigValue <> transactionFeeValue
    
    validAccountDatum :: Bool
    validAccountDatum = case returnType of
      ARTLeave -> accountDatum == leaveContractCAS casMap (removeContract inputDatum contrValHash)
      ARTCancel -> accountDatum == cancelContractCAS casMap (removeContract inputDatum contrValHash)
    
    validContract :: Bool
    validContract = case findContractNFT (txOutValue contractInput) of
      Just ac -> ac == contractNFT
      Nothing -> False

{-# INLINEABLE mkAccountValidator #-}
mkAccountValidator ::
  AccountSettings ->
  AccountDatum ->
  AccountRedeemer ->
  ScriptContext ->
  Bool
mkAccountValidator as dat ACreateContract ctx = validateCreateContract as dat ctx
mkAccountValidator as dat ASign ctx = validateSign as dat ctx
mkAccountValidator as _ (ACollect pkh) ctx = validateCollect as pkh ctx
mkAccountValidator as dat (AReturn t) ctx = validateReturn as t dat ctx

typedAccountValidator :: AccountSettings -> Scripts.TypedValidator AccountType
typedAccountValidator as =
  Scripts.mkTypedValidator @AccountType
    ($$(PlutusTx.compile [||mkAccountValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode as)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @AccountDatum @AccountRedeemer

accountValidator :: AccountSettings -> Validator
accountValidator = Scripts.validatorScript . typedAccountValidator

accountValidatorHash :: AccountSettings -> ValidatorHash
accountValidatorHash = validatorHash . accountValidator

accountAddress :: AccountSettings -> Ledger.Address
accountAddress = scriptAddress . accountValidator