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

module Membership.OnChain.Contract where

import Ledger
  ( Address,
    PubKeyHash (..),
    ScriptContext (scriptContextTxInfo),
    TxInfo (txInfoValidRange),
    TxOut (txOutValue),
    ValidatorHash,
    contains,
    findDatum,
    from,
    member,
    scriptAddress,
    txSignedBy,
  )
import Ledger.Typed.Scripts as Scripts
  ( TypedValidator,
    Validator,
    mkTypedValidator,
    validatorScript,
    wrapValidator,
  )
import Ledger.Value
  ( AssetClass,
    CurrencySymbol (..),
    TokenName,
    Value (Value),
    assetClass,
    assetClassValue,
    flattenValue,
    geq,
  )
import Membership.Contract
import Membership.Logic
import Membership.OnChain.Utils
import Membership.PlatformSettings
import Membership.Service
import Membership.ShameToken
import Membership.Signature
import Membership.Utils
import qualified PlutusTx
import PlutusTx.AssocMap as Map (delete, lookup)
import PlutusTx.Prelude
  ( Bool (..),
    BuiltinString,
    Eq ((==)),
    Integer,
    Maybe (..),
    Monoid (mempty),
    Semigroup ((<>)),
    elem,
    find,
    fst,
    isJust,
    negate,
    not,
    otherwise,
    return,
    snd,
    traceError,
    traceIfFalse,
    ($),
    (&&),
    (.),
    (||),
  )

{-# INLINEABLE getContractEssentials #-}
getContractEssentials :: BuiltinString -> ContractSettings -> ScriptContext -> ContractEssentials
getContractEssentials validatorName cst ctx =
  ContractEssentials
    { ceInfo = info,
      ceOwnInput = ownInput,
      ceOwnOutput = ownOutput,
      ceOutputDatum = outputDatum,
      ceSigSymbol = sigSymbol,
      ceInputSigs = inputSignatures,
      ceOutputSigs = outputSignatures,
      ceOwnSig = ownSig,
      ceSigValue = sigValue,
      cePresentWhere = presentWhere
    }
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    outputDatum :: ContractDatum
    outputDatum = case findContractDatum ownOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError (createError validatorName "No Output Datum found")

    sigSymbol :: CurrencySymbol
    sigSymbol = csSignatureSymbol cst

    inputSignatures :: [Sig]
    inputSignatures = findSignatures sigSymbol (txOutValue ownInput)

    outputSignatures :: [Sig]
    outputSignatures = findSignatures sigSymbol (txOutValue ownOutput)

    ownInputSig :: Maybe Sig
    ownInputSig = whoSigned' info inputSignatures

    ownOutputSig :: Maybe Sig
    ownOutputSig = whoSigned' info outputSignatures

    ownSig :: Sig
    ownSig = case (ownInputSig, ownOutputSig) of
      (Just s, _) -> s
      (_, Just s) -> s
      _ -> traceError $ createError validatorName "Sig token was not found"

    -- A value representing a single sig token from our user
    sigValue :: Value
    sigValue = signatureValue sigSymbol (sUser ownSig) (sScript ownSig)

    presentWhere :: PresentWhere
    presentWhere = case (ownInputSig, ownOutputSig) of
      (Just _, Just _) -> PWBoth
      (Just _, Nothing) -> PWInput
      (Nothing, Just _) -> PWOutput
      (Nothing, Nothing) -> traceError $ createError validatorName "Sig token was not found"

{-# INLINEABLE validateContract #-}
validateContract :: ContractSettings -> ScriptContext -> Bool
validateContract contractSettings ctx =
  traceIfFalse
    "Contract - Inconsistent signatures"
    ( consistentSigs outputSignatures
        && consistentSigs inputSignatures
    )
    && traceIfFalse "Contract - NFT is not in both input and output" continuingNFT
  where
    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    sigSymbol :: CurrencySymbol
    sigSymbol = csSignatureSymbol contractSettings

    continuingNFT :: Bool
    continuingNFT = isJust $ do
      inputNFT <- find f (flattenValue $ txOutValue ownInput)
      outputNFT <- find f (flattenValue $ txOutValue ownOutput)
      return (inputNFT, outputNFT)
      where
        f :: (CurrencySymbol, TokenName, Integer) -> Bool
        f (_, tn, amt) = tn == contractNFTTokenName && amt == 1
        f _ = False

    inputSignatures :: [Sig]
    inputSignatures = findSignatures sigSymbol (txOutValue ownInput)

    outputSignatures :: [Sig]
    outputSignatures = findSignatures sigSymbol (txOutValue ownOutput)

{-# INLINEABLE sufficientValue #-}
sufficientValue :: ScriptContext -> Value -> Bool
sufficientValue ctx requiredValue = valueDifference == requiredValue
  where
    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    valueDifference :: Value
    valueDifference = txOutValue ownOutput <> negate (txOutValue ownInput)

{-# INLINEABLE createError #-}
createError :: BuiltinString -> BuiltinString -> BuiltinString
createError validatorName validatorError =
  validatorName
    <> " - "
    <> validatorError

{-# INLINEABLE abstractValidator #-}
abstractValidator :: BuiltinString -> ContractDatum -> Value -> ScriptContext -> Bool
abstractValidator validatorName expectedDatum expectedValueDifference ctx =
  traceIfFalse (createError validatorName "Invalid Datum") validDatum
    && traceIfFalse (createError validatorName "Invalid Value") validValue
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    outputDatum :: ContractDatum
    outputDatum = case findContractDatum ownOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError (createError validatorName "No Output Datum found")

    validDatum :: Bool
    validDatum = expectedDatum == outputDatum

    validValue :: Bool
    validValue =
      txOutValue ownOutput <> negate (txOutValue ownInput)
        == expectedValueDifference

-- Signing should only validate if
--    * The contract role map increases correctly with
--        * The added user having signed the transaction
--        * If the user is a mediator, he is in the contract list
--        * The user is not a publisher
--    * The necessary value is paid to the contract, being
--        * If the user is a mediator,
--            * The SIG token plus the collateral
--        * If the user is a client
--            * The SIG token plus the collateral plus the price (if it's a one-time contract)
--    * The Datum stays exactly the same (except for the role map)
--    * The Contract is valid
{-# INLINEABLE handleSigning #-}
handleSigning :: ContractSettings -> ContractEssentials -> ContractDatum -> ScriptContext -> Bool
handleSigning cst ce inputDatum ctx =
  validateContract cst ctx
    && traceIfFalse "Contract Signing - Invalid Contract Value" validValue
    && traceIfFalse "Contract Signing - Invalid Contract Datum" validDatum
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    outputDatum :: ContractDatum
    outputDatum = case findContractDatum ownOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError "Contract Signing - No Output Datum found"

    -- The public key hash derived from the SIG token found
    pkh :: PubKeyHash
    pkh = sUser $ ceOwnSig ce

    -- Tries to find our user's role by subtracting the output's role map from the input's one
    userRole :: Role
    userRole = case subtractMaps (cdRoleMap $ ceOutputDatum ce) (cdRoleMap inputDatum) of
      -- If our user is a mediator, he must be in the contract judges list
      Just (pkh', Mediator)
        | pkh' == pkh ->
          if pkh `elem` jsPubKeyHashes (cdJudges inputDatum)
            then Mediator
            else traceError "Contract Signing - Mediator not in the list"
      Just (pkh', r)
        | pkh' == pkh -> r
      _ -> traceError "Contract Signing - No role added"

    -- The collateral hold in this contract
    trustValue :: Value
    trustValue = assetClassValue (psToken $ csPlatformSettings cst) (sTrust $ cdService inputDatum)

    -- The price that will be paid to the service provider if everything goes well
    -- If this is a constant contract, there is no price
    priceValue :: Value
    priceValue = case sType $ cdService inputDatum of
      CConstant -> mempty
      OneTime v _ -> v

    -- The value that must result from the subtraction between output and input values
    -- If our user is a client, he must provide his sig token,
    --  his collateral and the price for the service (if it's one-time)
    -- If our user is a mediator, he must only provide his sig value and a collateral
    -- If our user is a publisher, the validation should fail since there can only
    --  be one publisher (the one that created the contract)
    expectedValueDifference :: Value
    expectedValueDifference = case userRole of
      Client -> ceSigValue ce <> trustValue <> priceValue
      Mediator -> ceSigValue ce <> trustValue
      Publisher -> traceError "Contract Signing - Publisher cannot sign the contract again"

    validValue :: Bool
    validValue =
      txOutValue ownOutput <> negate (txOutValue ownInput)
        == expectedValueDifference

    validDatum :: Bool
    validDatum = outputDatum == addUser pkh userRole inputDatum

{-# INLINEABLE handleAccusation #-}
handleAccusation ::
  ContractSettings ->
  ContractDatum ->
  (PubKeyHash, PubKeyHash) ->
  ScriptContext ->
  Bool
handleAccusation cst inputDatum (accuser, accused) ctx =
  validateContract cst ctx
    && traceIfFalse "Contract Accusation - Invalid Contract Value" validContractValue
    && traceIfFalse "Contract Accusation - Invalid Contract Datum" validContractDatum
    && traceIfFalse "Contract Accusation - Invalid Logic" validLogicScript
    && traceIfFalse
      "Contract Accusation - Logic Script did not receive collateral"
      validLogicScriptValue
    && traceIfFalse "Contract Accusation - Transaction not signed by accuser" signedByAccuser
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx
  
    outputDatum :: ContractDatum
    outputDatum = case findContractDatum ownOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError "Contract Accusation - No output datum found"

    signatureSymbol, shameTokenSymbol :: CurrencySymbol
    shameTokenSymbol = psShameTokenSymbol $ csPlatformSettings cst
    signatureSymbol = csSignatureSymbol cst

    logicScriptValHash :: ValidatorHash
    logicScriptValHash = cdLogicScript inputDatum

    logicScriptInput, logicScriptOutput :: TxOut
    logicScriptInput = case strictFindInputWithValHash logicScriptValHash info of
      Just o -> o
      Nothing -> traceError "Contract Accusation - Couldn't find an unique logic script input"
    logicScriptOutput = case strictFindOutputWithValHash logicScriptValHash info of
      Just o -> o
      Nothing -> traceError "Contract Accusation - Couldn't find an unique logic script output"

    st :: ShameToken
    st = case findShameToken shameTokenSymbol (txOutValue logicScriptInput) of
      Just st' -> st'
      Nothing -> traceError "Contract Accusation - Couldn't find a shame token in the logic script input"
    
    shameTokenAssetClass :: AssetClass
    shameTokenAssetClass = assetClass shameTokenSymbol (stTokenName st)

    sigs :: [Sig]
    sigs = findSignatures signatureSymbol (txOutValue ownInput)

    -- A nice data type to represent the accuser and accuse sig tokens
    accuserSig, accusedSig, judgeSig :: Sig
    accuserSig = case findSig accuser sigs of
      Just s -> s
      Nothing -> traceError "Contract Accusation - Accuser SIG token missing"
    accusedSig = case findSig accused sigs of
      Just s -> s
      Nothing -> traceError "Contract Accusation - Accused SIG token missing"
    judgeSig = case firstValidJudge (jsPubKeyHashes $ cdJudges inputDatum) sigs of
      Just s -> sig s (sScript accuserSig)
      Nothing -> traceError "Contract Accusation - No available judge"

    accuserRole, accusedRole :: Role
    accuserRole = case Map.lookup accuser (cdRoleMap inputDatum) of
      Just r -> r
      Nothing -> traceError "Contract Accusation - Accuser not registered"
    accusedRole = case Map.lookup accused (cdRoleMap inputDatum) of
      Just r -> r
      Nothing -> traceError "Contract Accusation - Accused not registered"

    accusedSigValue, judgeSigValue :: Value
    accusedSigValue = signatureValue' signatureSymbol accusedSig
    judgeSigValue = signatureValue' signatureSymbol judgeSig

    judgeValue, trustValue, shameTokenValue :: Value
    -- The price a judge will receive after mediating the conflict
    judgeValue = assetClassValue (psToken $ csPlatformSettings cst) (jsPrice $ cdJudges inputDatum)
    -- The value that serves as collateral in case someone breaks the rules
    trustValue = assetClassValue (psToken $ csPlatformSettings cst) (sTrust $ cdService inputDatum)
    -- The value that uniquely identifies the logic script
    shameTokenValue = assetClassValue shameTokenAssetClass 1

    validContractValue :: Bool
    validContractValue =
      txOutValue ownOutput <> negate (txOutValue ownInput)
        == negate (trustValue <> trustValue <> accusedSigValue <> judgeSigValue)

    -- If the output Datum matches the expected values
    validContractDatum :: Bool
    validContractDatum =
      validJudges && validLogic && validService && validRoleMap && validAccusations
          && traceIfFalse
            "Contract Accusation - Datum accusation was not added correctly"
            validAccusations
        where
          validJudges :: Bool
          validJudges = traceIfFalse
            "Contract Accusation - Datum judges are not the same"
            (cdJudges inputDatum == cdJudges outputDatum)

          validLogic :: Bool
          validLogic = traceIfFalse
            "Contract Accusation - Datum logic scripts are not the same"
            (cdLogicScript inputDatum == cdLogicScript outputDatum)

          validService :: Bool
          validService = traceIfFalse
            "Contract Accusation - Datum services are not the same"
            (cdService inputDatum == cdService outputDatum)

          validRoleMap :: Bool
          validRoleMap = traceIfFalse
            "Contract Accusation - Datum role maps are not the same"
            (cdRoleMap inputDatum == cdRoleMap outputDatum)

          validAccusations :: Bool
          validAccusations = traceIfFalse
            "Contract Accusation - Datum accusation was not added correctly"
            (case cdAccusations outputDatum of
              ((
                Accusation
                (accuser', accuserRole')
                (accused', accusedRole')
                accusationTime
               ) : accusations) ->
                accusationTime `member` txInfoValidRange info -- Accusation is being made now
                  && accuser == accuser'
                  && accuserRole == accuserRole'
                  && accused == accused'
                  && accusedRole == accusedRole'
                  && accusations == cdAccusations inputDatum
              _ -> False)

    validLogicScript :: Bool
    validLogicScript =
      txOutValue logicScriptInput `geq` shameTokenValue
        && txOutValue logicScriptOutput `geq` shameTokenValue

    validLogicScriptValue :: Bool
    validLogicScriptValue =
      txOutValue logicScriptOutput <> negate (txOutValue logicScriptInput)
        == ( trustValue
               <> trustValue
               <> accusedSigValue
               <> judgeSigValue
               <> judgeValue
           )

    signedByAccuser :: Bool
    signedByAccuser = txSignedBy info accuser

{-# INLINEABLE handleMediation #-}
handleMediation ::
  ContractSettings ->
  ContractDatum ->
  ScriptContext ->
  Bool
handleMediation cst inputDatum ctx =
  validateContract cst ctx
    && traceIfFalse "Contract Mediation - Invalid Contract" validContract
    && traceIfFalse "Contract Mediation - Invalid Logic" validLogic
    && traceIfFalse "Contract Mediation - Invalid contract value" validContractValue
    && traceIfFalse "Contract Mediation - Invalid contract datum" validContractDatum
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    outputDatum :: ContractDatum
    outputDatum = case findContractDatum ownOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError "Contract Mediation - No output datum found"

    logicValHash :: ValidatorHash
    logicValHash = cdLogicScript inputDatum

    logicInput :: TxOut
    logicInput = case strictFindInputWithValHash logicValHash info of
      Just out -> out
      Nothing -> traceError "Contract Mediation - Couldn't find an unique logic script input"

    logicDatum :: LogicState
    logicDatum = case findLogicDatum logicInput (`findDatum` info) of
      Just (LSWaitingEnd nft jdg acc ver) -> LSWaitingEnd nft jdg acc ver
      Just _ -> traceError "Contract Mediation - Wrong logic state"
      Nothing -> traceError "Contract Mediation - Logic datum not found"

    contractNFT :: AssetClass
    contractNFT = case logicDatum of
      (LSWaitingEnd nft _ _ _) -> nft

    contractNFTValue :: Value
    contractNFTValue = assetClassValue contractNFT 1

    judgeInfo :: Judge
    judgeInfo = case logicDatum of
      (LSWaitingEnd _ jdg _ _) -> jdg

    accusation :: Accusation
    accusation = case logicDatum of
      (LSWaitingEnd _ _ acc _) -> acc

    sigSymbol :: CurrencySymbol
    sigSymbol = csSignatureSymbol cst

    judge :: PubKeyHash
    judge = jPubKeyHash judgeInfo

    accused :: PubKeyHash
    accused = fst $ aAccused accusation

    sigs :: [Sig]
    sigs = findSignatures sigSymbol (txOutValue ownOutput)

    accountValHash :: ValidatorHash
    accountValHash = case sigs of
      (x : _) -> sScript x
      [] -> traceError "Contract Mediate - No SIGs found"

    judgeSig, accusedSig :: Sig
    judgeSig = sig judge accountValHash
    accusedSig = sig accused accountValHash

    judgeSigValue, accusedSigValue, trustValue :: Value
    judgeSigValue = signatureValue' sigSymbol judgeSig
    accusedSigValue = signatureValue' sigSymbol accusedSig
    trustValue = assetClassValue (psToken $ csPlatformSettings cst) (sTrust $ cdService inputDatum)

    priceValue :: Value
    priceValue = case sType (cdService inputDatum) of
      OneTime v _
        | (snd (aAccuser accusation) == Client)
            && (snd (aAccused accusation) == Publisher) ->
          v
      _ -> mempty

    guilty :: Bool
    guilty
      | ownValueDifference == innocentValue = False
      | ownValueDifference == guiltyValue = True
      | otherwise = traceError "Contract Mediation - Invalid contract value"
      where
        ownValueDifference :: Value
        ownValueDifference = txOutValue ownOutput <> negate (txOutValue ownInput)

        innocentValue :: Value
        innocentValue =
          judgeSigValue
            <> accusedSigValue
            <> trustValue
            <> trustValue

        guiltyValue :: Value
        guiltyValue = judgeSigValue <> trustValue <> negate priceValue

    shameTokenAssetClass :: AssetClass
    shameTokenAssetClass = case findShameTokenAssetClass
      (psShameTokenSymbol $ csPlatformSettings cst)
      (txOutValue logicInput) of
      Just ac -> ac
      Nothing -> traceError "Contract Mediation - Shame Token could not be found"

    shameTokenValue :: Value
    shameTokenValue = assetClassValue shameTokenAssetClass 1

    sameJudges :: Bool
    sameJudges = cdJudges inputDatum == cdJudges outputDatum

    sameLogicScript :: Bool
    sameLogicScript = cdLogicScript inputDatum == cdLogicScript outputDatum

    sameService :: Bool
    sameService = cdService inputDatum == cdService outputDatum

    validRoleMap :: Bool
    validRoleMap =
      if guilty
        then cdRoleMap outputDatum == Map.delete accused (cdRoleMap inputDatum)
        else cdRoleMap outputDatum == cdRoleMap inputDatum

    validContract :: Bool
    validContract =
      txOutValue ownInput `geq` contractNFTValue
        && txOutValue ownOutput `geq` contractNFTValue

    validContractValue :: Bool
    validContractValue = True -- The "guilty" variable is already checking that for us
    validLogic :: Bool
    validLogic = txOutValue logicInput `geq` shameTokenValue

    validContractDatum :: Bool
    validContractDatum =
      traceIfFalse "Contract Accusation - Datum judges changed" sameJudges
        && traceIfFalse "Contract Accusation - Datum logic script changed" sameLogicScript
        && traceIfFalse "Contract Accusation - Datum service changed" sameService
        && traceIfFalse "Contract Accusation - Invalid datum role map" validRoleMap
        && traceIfFalse
          "Contract Accusation - Datum accusation was not removed correctly"
          (cdAccusations outputDatum == cdAccusations (removeAccusation accusation inputDatum))

-- Cancellation should only validate if
--    * The contract is valid
--    * The Datum stays the same except for the role map, which should remove our user from it
--    * The output minus the input values is equal to our sig token (negative)
--    * There is an account in this transaction's input and output
--    * The account is valid
--    * The account receives the SIG token from the user
{-# INLINEABLE handleCancellation #-}
handleCancellation ::
  ContractSettings ->
  ContractDatum ->
  ScriptContext ->
  Bool
handleCancellation cst inputDatum ctx =
  validateContract cst ctx
    && traceIfFalse "Contract Cancellation - Invalid Account" validAccount
    && traceIfFalse "Contract Cancellation - Account did not receive SIG token" validAccountValue
    && traceIfFalse "Contract Cancellation - Invalid Contract Value" validContractValue
    && traceIfFalse "Contract Cancellation - Invalid Contract Datum" validContractDatum
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    sigSymbol :: CurrencySymbol
    sigSymbol = csSignatureSymbol cst

    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    outputDatum :: ContractDatum
    outputDatum = case findContractDatum ownOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError "Contract Cancellation - No Output Datum found"

    sigs :: [Sig]
    sigs = findSignatures sigSymbol (txOutValue ownInput)

    ownSig :: Sig
    ownSig = case whoSigned' info sigs of
      Just s -> s
      Nothing -> traceError "Contract Cancellation - Sig not found"

    pkh :: PubKeyHash
    pkh = sUser ownSig

    accValHash :: ValidatorHash
    accValHash = sScript ownSig

    sigValue :: Value
    sigValue = signatureValue' sigSymbol ownSig

    accountInput, accountOutput :: TxOut
    accountInput = case strictFindInputWithValHash accValHash info of
      Just o -> o
      Nothing -> traceError "Contract Cancellation - Couldn't find an unique account input"
    accountOutput = case strictFindOutputWithValHash accValHash info of
      Just o -> o
      Nothing -> traceError "Contract Cancellation - Couldn't find an unique account output"

    validAccount :: Bool
    validAccount =
      txOutValue accountInput `geq` sigValue
        && txOutValue accountOutput `geq` sigValue

    validAccountValue :: Bool
    validAccountValue =
      txOutValue accountOutput <> negate (txOutValue accountInput)
        == sigValue

    validContractDatum :: Bool
    validContractDatum = outputDatum == removeUser pkh inputDatum
    
    validContractValue :: Bool
    validContractValue =
      txOutValue ownOutput <> negate (txOutValue ownInput)
        == negate sigValue

{-# INLINEABLE handleLeave #-}
handleLeave ::
  ContractSettings ->
  ContractDatum ->
  ScriptContext ->
  Bool
handleLeave cst inputDatum ctx =
  validateContract cst ctx
    && traceIfFalse "Contract Leave - Invalid Contract Value" validContractValue
    && traceIfFalse "Contract Leave - Invalid Contract Datum" validContractDatum
    && traceIfFalse "Contract Leave - Invalid account" validAccount
    && traceIfFalse
      "Contract Leave - Account did not receive SIG token"
      validAccountValue
    && traceIfFalse
      "Contract Leave - Not allowed to leave the contract now"
      allowedToLeave
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    sigSymbol :: CurrencySymbol
    sigSymbol = csSignatureSymbol cst

    platformSettings :: PlatformSettings
    platformSettings = csPlatformSettings cst

    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    outputDatum :: ContractDatum
    outputDatum = case findContractDatum ownOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError "Contract Leave - No Output Datum found"
    
    sigs :: [Sig]
    sigs = findSignatures sigSymbol (txOutValue ownInput)

    ownSig :: Sig
    ownSig = case whoSigned' info sigs of
      Just s -> s
      Nothing -> traceError "Contract Leave - Sig not found"

    pkh :: PubKeyHash
    pkh = sUser ownSig

    accValHash :: ValidatorHash
    accValHash = sScript ownSig

    userRole :: Role
    userRole = case Map.lookup pkh (cdRoleMap inputDatum) of
      Just r -> r
      Nothing -> traceError "Contract Leave - User not registered"

    accountInput, accountOutput :: TxOut
    accountInput = case strictFindInputWithValHash accValHash info of
      Just o -> o
      Nothing -> traceError "Contract Leaving - Couldn't find an unique account input"
    accountOutput = case strictFindOutputWithValHash accValHash info of
      Just o -> o
      Nothing -> traceError "Contract Leaving - Couldn't find an unique account output"

    trustValue, priceValue, sigValue :: Value
    -- The collateral held in this contract
    trustValue = assetClassValue (psToken $ csPlatformSettings cst) (sTrust $ cdService inputDatum)
    -- The price that will be paid to the service provider if everything goes well
    -- If this is a constant contract, there is no price
    priceValue = case sType $ cdService inputDatum of
      OneTime v _
        | userRole == Publisher -> v
      _ -> mempty
    sigValue = signatureValue' sigSymbol ownSig
    transactionFeeValue =
      assetClassValue
        (psToken platformSettings)
        (psTxFee platformSettings)

    publisherPresent :: Bool
    publisherPresent = sigIn (sPublisher $ cdService inputDatum) sigs

    validContractValue :: Bool
    validContractValue =
      txOutValue ownOutput <> negate (txOutValue ownInput)
        == negate (trustValue <> sigValue <> priceValue)

    validContractDatum :: Bool
    validContractDatum = outputDatum == removeUser pkh inputDatum

    validAccount :: Bool
    validAccount =
      txOutValue accountInput `geq` sigValue
        && txOutValue accountOutput `geq` sigValue

    validAccountValue :: Bool
    validAccountValue =
      txOutValue accountOutput <> negate (txOutValue accountInput)
        == sigValue <> transactionFeeValue

    accusations :: [Accusation]
    accusations = cdAccusations inputDatum

    allowedToLeave :: Bool
    allowedToLeave =
      case sType $ cdService inputDatum of
        OneTime _ dln
          | not (involvedInAccusation pkh accusations) ->
            (from dln `contains` txInfoValidRange info)
              || not publisherPresent
        CConstant -> not (involvedInAccusation pkh accusations)

{-# INLINEABLE handleReview #-}
handleReview ::
  ContractSettings ->
  Review ->
  PubKeyHash ->
  ContractDatum ->
  ScriptContext ->
  Bool
handleReview _ _ _ _ _ = traceError "Contract Review - Incomplete"

{-# INLINEABLE mkContractValidator #-}
mkContractValidator :: ContractSettings -> ContractDatum -> ContractRedeemer -> ScriptContext -> Bool
mkContractValidator cst dat CSign ctx = handleSigning cst ce dat ctx
  where
    ce :: ContractEssentials
    ce = getContractEssentials "Contract Signature" cst ctx
mkContractValidator cst dat (CAccuse acr acd) ctx = handleAccusation cst dat (acr, acd) ctx
mkContractValidator cst dat CMediate ctx = handleMediation cst dat ctx
mkContractValidator cst dat CCancel ctx = handleCancellation cst dat ctx
mkContractValidator cst dat CLeave ctx = handleLeave cst dat ctx

typedContractValidator :: ContractSettings -> Scripts.TypedValidator ContractType
typedContractValidator ps =
  Scripts.mkTypedValidator @ContractType
    ($$(PlutusTx.compile [||mkContractValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode ps)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @ContractDatum @ContractRedeemer

contractValidator :: ContractSettings -> Validator
contractValidator = Scripts.validatorScript . typedContractValidator

contractAddress :: ContractSettings -> Ledger.Address
contractAddress = scriptAddress . contractValidator