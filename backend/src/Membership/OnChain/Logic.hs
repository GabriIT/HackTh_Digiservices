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

module Membership.OnChain.Logic where

import Ledger
  ( POSIXTime,
    PubKeyHash,
    ScriptContext (scriptContextTxInfo),
    TxInInfo (txInInfoResolved),
    TxInfo (txInfoValidRange),
    TxOut (txOutValue),
    Validator,
    ValidatorHash,
    Value,
    contains,
    findDatum,
    findOwnInput,
    from,
    getContinuingOutputs,
    to,
    txSignedBy,
    validatorHash,
    valuePaidTo,
  )
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value (AssetClass, assetClassValue, assetClassValueOf, geq)
import Membership.Account
  ( AccountDatum,
    declaredGuiltyCAS,
    findAccountDatum,
    removeContract,
  )
import Membership.Contract
import Membership.Logic
import Membership.OnChain.Utils (strictFindOutAndIn)
import Membership.PlatformSettings
  ( PlatformSettings (..),
  )
import Membership.Service
import Membership.Signature
import Membership.Utils
  ( strictFindInputWithValHash,
    strictFindOutputWithValHash,
  )
import qualified PlutusTx
import PlutusTx.Prelude
  ( AdditiveSemigroup ((+)),
    Bool (..),
    Eq ((==)),
    Integer,
    Maybe (..),
    find,
    fst,
    isJust,
    mempty,
    negate,
    not,
    null,
    otherwise,
    snd,
    traceError,
    traceIfFalse,
    ($),
    (&&),
    (/=),
    (<>),
    (||),
  )

-- TODO: Verify validity and consistency of SIG tokens

-- The validator that is triggered when a user want's to accuse someone
{-# INLINEABLE mkLogicValidator #-}
mkLogicValidator :: LogicSettings -> LogicState -> LogicRedeemer -> ScriptContext -> Bool
mkLogicValidator logicSettings LSWaitingStart (LRAccuse accusation) ctx =
  traceIfFalse "Logic Accusation - Not signed by accuser" signedByAccuser
    && traceIfFalse "Logic Accusation - Accuser cannot accuse himself" accuserIsNotAccused
    && traceIfFalse "Logic Accusation - Invalid logic" validLogic
    && traceIfFalse "Logic Accusation - Invalid logic datum" validLogicDatum
    && traceIfFalse "Logic Accusation - Invalid logic value" validLogicValue
    && traceIfFalse "Logic Accusation - Invalid contract" validContract
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- The logic input and output (which will be in the
    -- blockchain if this transaction is validated)
    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    -- The future logic datum (logic state) if this tx is validated
    logicDatum :: LogicState
    logicDatum = case findLogicDatum ownOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError "Logic Accusation - Logic Datum could not be found"

    -- The asset class from the shame token taken from our input
    shameToken :: AssetClass
    shameToken =
      case findShameTokenAssetClass
        (psShameTokenSymbol $ lsPlatformSettings logicSettings)
        (txOutValue ownInput) of
        Just ac -> ac
        Nothing -> traceError "Logic Accusation - Shame Token could not be found"

    -- A value (with amount of one) based on the shame token found in our input
    shameTokenValue :: Value
    shameTokenValue = assetClassValue shameToken 1

    -- Tries to find the contract input and output based on the contract
    -- validator hash in our logic settings
    contractInput, contractOutput :: TxOut
    contractInput = case strictFindInputWithValHash (lsContract logicSettings) info of
      Just o -> o
      Nothing -> traceError "Logic Accusation - Couldn't find an unique contract input"
    contractOutput = case strictFindOutputWithValHash (lsContract logicSettings) info of
      Just o -> o
      Nothing -> traceError "Logic Accusation - Couldn't find an unique contract output"

    -- If the contract input was found, tries to get the contract datum
    contractDatum :: ContractDatum
    contractDatum = case findContractDatum contractInput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError "Logic Accusation - Contract Datum could not be found"

    -- Searches for the nft that identifies our contract inside the contract
    -- input value
    contractNFTAssetClass :: AssetClass
    contractNFTAssetClass = case findContractNFT $ txOutValue contractInput of
      Just ac -> ac
      Nothing -> traceError "Logic Accusation - Could not find contract nft"

    contractNFT :: Value
    contractNFT = assetClassValue contractNFTAssetClass 1

    judges :: Judges
    judges = cdJudges contractDatum

    -- The value that needs to be paid in collateral by each party
    trustValue :: Value
    trustValue =
      assetClassValue
        (psToken $ lsPlatformSettings logicSettings)
        (sTrust $ cdService contractDatum)

    -- The value a judge will receive as a reward
    judgeValue :: Value
    judgeValue =
      assetClassValue
        (psToken $ lsPlatformSettings logicSettings)
        (jsPrice judges)

    -- All the SIGs that could be found inside our contract input value
    contractSigs :: [Sig]
    contractSigs =
      findSignatures
        (lsSignatureSymbol logicSettings)
        (txOutValue contractInput)

    -- The expected public key hash
    pkh :: PubKeyHash
    pkh = fst (aAccuser accusation)

    -- Tries to find our own sig inside the contract sigs, since
    -- we should be the accusator
    ownSig :: Sig
    ownSig = case findSig pkh contractSigs of
      Just s -> s
      Nothing -> traceError "Logic Accusation - SIG token missing"

    -- The account validator hash according to the SIG token
    accountValHash :: ValidatorHash
    accountValHash = sScript ownSig

    -- The current selected judge public key hash
    judgePKH :: PubKeyHash
    judgePKH = case firstValidJudge (jsPubKeyHashes judges) contractSigs of
      Just pkh' -> pkh'
      Nothing -> traceError "Logic Accusation - No available judge"

    -- Join all important information about our judge (his pubkeyhash,
    -- price and deadline) into one variable
    judge :: Judge
    judge =
      Judge
        { jPubKeyHash = judgePKH,
          jPrice = jsPrice judges,
          jMaxDuration = jsMaxDuration judges
        }

    -- Make judge pub key hash into a sig value
    judgeSigValue :: Value
    judgeSigValue =
      signatureValue
        (lsSignatureSymbol logicSettings)
        judgePKH
        accountValHash

    -- Make accused pub key hash into a sig value
    accusedSigValue :: Value
    accusedSigValue =
      signatureValue
        (lsSignatureSymbol logicSettings)
        (fst $ aAccused accusation)
        accountValHash

    -- Verifies if the accuser actually signed the transaction
    signedByAccuser :: Bool
    signedByAccuser = txSignedBy info pkh

    -- Verifies if the accuser is accusing himself
    accuserIsNotAccused :: Bool
    accuserIsNotAccused = aAccuser accusation /= aAccused accusation

    -- Verifies if the logic has a shame token in both it's input and output
    -- This is done to uniquely identify the logic script
    validLogic :: Bool
    validLogic =
      txOutValue ownInput `geq` shameTokenValue
        && txOutValue ownOutput `geq` shameTokenValue

    -- Verifies if the output logic datum matches the next expected state
    validLogicDatum :: Bool
    validLogicDatum =
      logicDatum == LSWaitingVerdict contractNFTAssetClass judge accusation

    -- From the logic input to the output, we should receive:
    --    The judge trust value in case he does not mediate the conflict
    -- within the deadline;
    --    The accused trust value that will be distributed according to
    -- the logic and the verdict;
    --    The judge price, which can be found in the contract datum;
    --    The judge and accused sig value, used to prove compliance
    validLogicValue :: Bool
    validLogicValue =
      txOutValue ownOutput <> negate (txOutValue ownInput)
        == trustValue -- The accused collateral
        <> trustValue -- The judge collateral
        <> judgeValue -- The judge price
        <> judgeSigValue
        <> accusedSigValue

    -- In the same way as the logic, a contract is only valid if it
    -- has an nft in the input and output. This, again, is done to
    -- uniquely identify the contract
    validContract :: Bool
    validContract =
      txOutValue contractInput `geq` contractNFT
        && txOutValue contractOutput `geq` contractNFT
mkLogicValidator
  logicSettings
  (LSWaitingVerdict contractNFT judge accusation)
  (LRMediate verdict)
  ctx =
    traceIfFalse "Logic Mediate - Invalid logic" validLogic
      && traceIfFalse "Logic Mediate - Invalid logic datum" validLogicDatum
      && traceIfFalse "Logic Mediate - Invalid logic value" validLogicValue
      && traceIfFalse "Logic Mediate - Deadline passed" withinDeadline
      && traceIfFalse "Logic Mediate - Not signed by judge" signedByJudge
      && traceIfFalse "Logic Mediate - Invalid verdict" possibleVerdict
    where
      info :: TxInfo
      info = scriptContextTxInfo ctx

      ownInput, ownOutput :: TxOut
      ownInput = fst $ strictFindOutAndIn ctx
      ownOutput = snd $ strictFindOutAndIn ctx

      logicDatum :: LogicState
      logicDatum = case findLogicDatum ownOutput (`findDatum` info) of
        Just dat -> dat
        Nothing -> traceError "Logic Mediate - Logic Datum could not be found"

      shameToken :: AssetClass
      shameToken =
        case findShameTokenAssetClass
          (psShameTokenSymbol $ lsPlatformSettings logicSettings)
          (txOutValue ownInput) of
          Just ac -> ac
          Nothing -> traceError "Logic Mediate - Shame Token could not be found"

      shameTokenValue :: Value
      shameTokenValue = assetClassValue shameToken 1

      deadline :: POSIXTime
      deadline = aTime accusation + jMaxDuration judge

      validLogic :: Bool
      validLogic =
        txOutValue ownInput `geq` shameTokenValue
          && txOutValue ownOutput `geq` shameTokenValue

      -- TODO: In the future there should be an appeal option and the judge
      -- TODO: should only be allowed to leave when a certain deadline has passed
      validLogicDatum :: Bool
      validLogicDatum = logicDatum == LSWaitingEnd contractNFT judge accusation verdict

      -- In this case, we only care about the verdict sent, the logic
      -- value shouldn't change
      validLogicValue :: Bool
      validLogicValue = txOutValue ownOutput == txOutValue ownInput

      -- The judge should have a deadline to send his verdict, if the deadline passes,
      -- The judge's trust should be split between accuser and accused, and the next
      -- judge should be the current now

      -- TODO: Handle what happens when the deadline passes
      withinDeadline :: Bool
      withinDeadline = not $ from deadline `contains` txInfoValidRange info

      -- The mediation should only be done by the judge and,
      -- specifically, the current judge
      signedByJudge :: Bool
      signedByJudge = txSignedBy info (jPubKeyHash judge)

      -- Verify if the sent verdict answers all possible inputs
      possibleVerdict :: Bool
      possibleVerdict = validVerdict (lsLogic logicSettings) verdict
mkLogicValidator
  logicSettings
  (LSWaitingEnd contractNFT judge accusation verdict)
  LRConsume
  ctx =
    traceIfFalse "Logic Consume - Invalid logic" validLogic
      && traceIfFalse "Logic Consume - Logic UTxO did not disappear" logicDisappeared
      && traceIfFalse "Logic Consume - Invalid contract" validContract
      && traceIfFalse "Logic Consume - Invalid contract value" validContractValue
      && traceIfFalse "Logic Consume - Invalid tokens distribution" validDistribution
      && traceIfFalse "Logic Consume - Invalid accused account" validAccount
      && traceIfFalse "Logic Consume - Accused account did not receive shame token" validAccountValue
      && traceIfFalse "Logic Consume - Invalid accused account datum" validAccountDatum
      && traceIfFalse "Logic Consume - You need to wait" validTime
    where
      info :: TxInfo
      info = scriptContextTxInfo ctx

      -- The DSET token asset class
      platformToken :: AssetClass
      platformToken = psToken $ lsPlatformSettings logicSettings

      logic :: Logic
      logic = lsLogic logicSettings

      ownInput :: TxOut
      ownInput = case findOwnInput ctx of
        Nothing -> traceError "Logic Consume - Logic input missing"
        Just i -> txInInfoResolved i

      shameToken :: AssetClass
      shameToken =
        case findShameTokenAssetClass
          (psShameTokenSymbol $ lsPlatformSettings logicSettings)
          (txOutValue ownInput) of
          Just ac -> ac
          Nothing -> traceError "Logic Consume - Shame Token could not be found"

      shameTokenValue :: Value
      shameTokenValue = assetClassValue shameToken 1

      contractInput, contractOutput :: TxOut
      contractInput = case strictFindInputWithValHash (lsContract logicSettings) info of
        Just o -> o
        Nothing -> traceError "Logic Consume - Couldn't find an unique contract input"
      contractOutput = case strictFindOutputWithValHash (lsContract logicSettings) info of
        Just o -> o
        Nothing -> traceError "Logic Consume - Couldn't find an unique contract output"

      contractDatum :: ContractDatum
      contractDatum = case findContractDatum contractInput (`findDatum` info) of
        Just dat -> dat
        Nothing -> traceError "Logic Consume - Contract Datum could not be found"

      contractNFTValue :: Value
      contractNFTValue = assetClassValue contractNFT 1

      -- All the SIGs which can be found in our input
      sigs :: [Sig]
      sigs = findSignatures (lsSignatureSymbol logicSettings) (txOutValue ownInput)

      -- Tries to find the judge SIG within "sigs"
      judgeSig :: Sig
      judgeSig = case find (\s -> sUser s == jPubKeyHash judge) sigs of
        Just s -> s
        Nothing -> traceError "Logic Consume - Judge sig not found"

      -- Tries to find the accused SIG within "sigs"
      accusedSig :: Sig
      accusedSig = case find (\s -> sUser s == fst (aAccused accusation)) sigs of
        Just s -> s
        Nothing -> traceError "Logic Consume - Accused sig not found"

      -- Derives the account validator hash based on the judge SIG
      accountValHash :: ValidatorHash
      accountValHash = sScript judgeSig

      judgeSigValue :: Value
      judgeSigValue =
        signatureValue'
          (lsSignatureSymbol logicSettings)
          judgeSig

      accusedSigValue :: Value
      accusedSigValue =
        signatureValue'
          (lsSignatureSymbol logicSettings)
          accusedSig

      -- We will only need account input and output if our accused is
      -- guilty, so we get a "Maybe" version of them
      accountInput, accountOutput :: Maybe TxOut
      accountInput = strictFindInputWithValHash accountValHash info
      accountOutput = strictFindOutputWithValHash accountValHash info

      maybeInputAccountDatum, maybeOutputAccountDatum :: Maybe AccountDatum
      maybeInputAccountDatum = do
        acc <- accountInput
        findAccountDatum acc (`findDatum` info)
      maybeOutputAccountDatum = do
        acc <- accountOutput
        findAccountDatum acc (`findDatum` info)

      trust :: Integer
      trust = sTrust $ cdService contractDatum

      trustValue :: Value
      trustValue =
        assetClassValue
          (psToken $ lsPlatformSettings logicSettings)
          trust

      -- The price of the service, if there's one and if the accuser
      -- is a client and the accused a publisher, since, in that case,
      -- we want to give the client's money back
      priceValue :: Value
      priceValue = case sType (cdService contractDatum) of
        OneTime v _
          | (snd (aAccuser accusation) == Client)
              && (snd (aAccused accusation) == Publisher) ->
            v
        _ -> mempty

      proportion :: Maybe Proportion
      proportion = failedProportion (snd $ aAccused accusation) verdict logic

      -- If any condition failed, meaning the accused should be removed from
      -- the contract and an arbitrary proportion should be used to distribute
      -- his trust tokens (collateral)
      failed :: Bool
      failed = isJust proportion

      -- If the accused must receive a shame token or not
      guilty :: Bool
      guilty = isGuilty (snd $ aAccused accusation) (lsLogic logicSettings) verdict

      -- The value that should be given or taken from the contract
      expectedContractValueDifference :: Value
      expectedContractValueDifference =
        if failed
          then -- If any condition failed, the contract should receive the
          -- judge sig value back, the judge's collateral and loose the
          -- price (since that should go back to the client, if the conditions
          -- are met)

            judgeSigValue
              <> trustValue
              <> negate priceValue
          else -- If our accused is innocent, the contract should receive both
          -- the accused and judge sig values and trust (collaterals). In
          -- other words, nothing should happen

            accusedSigValue
              <> judgeSigValue
              <> trustValue
              <> trustValue

      validGuiltyDistribution :: Proportion -> Bool
      validGuiltyDistribution p =
        traceIfFalse
          "Logic Consume - Wrong accuser distribution"
          (paidToAccuser `geq` expectedPaidToAccuser)
          && traceIfFalse
            "Logic Consume - Wrong Accused distribution"
            (paidToAccused `geq` expectedPaidToAccused)
          && traceIfFalse
            "Logic Consume - Wrong Judge distribution"
            (paidToJudge `geq` expectedPaidToJudge)
        where
          paidToAccuser :: Value
          paidToAccuser = valuePaidTo info (fst $ aAccuser accusation)

          -- We expect that the accuser receive the distributed amount
          -- of trust tokens decided by the logic plus the price value
          -- if there was one
          expectedPaidToAccuser :: Value
          expectedPaidToAccuser =
            assetClassValue
              platformToken
              (fst (trustProportion p trust))
              <> priceValue

          paidToAccused :: Value
          paidToAccused = valuePaidTo info (fst $ aAccused accusation)

          -- We expect that the accused receive the distributed amount
          -- of trust tokens decided by the logic
          expectedPaidToAccused :: Value
          expectedPaidToAccused = assetClassValue platformToken (snd (trustProportion p trust))

          paidToJudge :: Value
          paidToJudge = valuePaidTo info (jPubKeyHash judge)

          -- The judge should receive his reward
          expectedPaidToJudge :: Value
          expectedPaidToJudge = assetClassValue platformToken (jPrice judge)

      -- If our accused is declared innocent, the only one that
      -- should receive something is the judge, who should receive
      -- his reward
      validInnocentDistribution :: Bool
      validInnocentDistribution = paidToJudge == jPrice judge
        where
          paidToJudge :: Integer
          paidToJudge =
            assetClassValueOf
              (valuePaidTo info (jPubKeyHash judge))
              (psToken $ lsPlatformSettings logicSettings)

      validLogic :: Bool
      validLogic = txOutValue ownInput `geq` shameTokenValue

      -- After the conflict has been mediated and the tokens have been
      -- distributed, the logic script should disappear
      logicDisappeared :: Bool
      logicDisappeared = null (getContinuingOutputs ctx)

      validContract :: Bool
      validContract =
        txOutValue contractInput `geq` contractNFTValue
          && txOutValue contractOutput `geq` contractNFTValue

      validContractValue :: Bool
      validContractValue =
        txOutValue contractOutput
          <> negate (txOutValue contractInput)
          == expectedContractValueDifference

      -- Our logic is executed based on the verdict sent by the judge,
      -- if it fails at any time, it should return the corresponding
      -- proportion, which would be provided to our validGuiltyDistribution
      -- function. Otherwise, we assume our accused is innocent and we can
      -- verify if the distribution is valid with validInnocentDistribution
      validDistribution :: Bool
      validDistribution = case proportion of
        Just p -> validGuiltyDistribution p
        Nothing -> validInnocentDistribution

      -- For all account validations, we should find an input and output
      -- if we are guilty. Othewise, it's naturally validated

      -- An account is considered valid only if it contains it's owner's
      -- sig token in both it's input and output
      validAccount :: Bool
      validAccount = case (accountInput, accountOutput) of
        (Just ai, Just ao) ->
          txOutValue ai `geq` accusedSigValue
            && txOutValue ao `geq` accusedSigValue
        _ ->
          not failed || traceError "Logic Consume - Could not find accused account"

      -- Our account should receive
      validAccountValue :: Bool
      validAccountValue = case (accountInput, accountOutput) of
        (Just ai, Just ao)
          | guilty -> txOutValue ao <> negate (txOutValue ai) == accusedSigValue <> shameTokenValue
          | failed -> txOutValue ao <> negate (txOutValue ai) == accusedSigValue
          | otherwise -> traceError "Logic Consume - Can not consume account for innocent user"
        _ -> not failed || traceError "Logic Consume - Could not find accused account"

      validAccountDatum :: Bool
      validAccountDatum = case (maybeInputAccountDatum, maybeOutputAccountDatum) of
        (Just inputAccountDatum, Just outputAccountDatum) ->
          outputAccountDatum
            == declaredGuiltyCAS
              (psCASMap $ lsPlatformSettings logicSettings)
              (removeContract inputAccountDatum (lsContract logicSettings))
        _ -> not failed || traceError "Logic Consume - Could not find accused account datum"

      -- Should be the deadline that a user has to appeal
      deadline :: POSIXTime
      deadline = aTime accusation + jMaxDuration judge + jMaxDuration judge

      validTime :: Bool
      validTime = not $ to deadline `contains` txInfoValidRange info

data LogicType

instance Scripts.ValidatorTypes LogicType where
  type DatumType LogicType = LogicState
  type RedeemerType LogicType = LogicRedeemer

typedLogicValidator :: LogicSettings -> Scripts.TypedValidator LogicType
typedLogicValidator logicSettings =
  Scripts.mkTypedValidator @LogicType
    ( $$(PlutusTx.compile [||mkLogicValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode logicSettings
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @LogicState @LogicRedeemer

logicValidator :: LogicSettings -> Validator
logicValidator logicSettings = Scripts.validatorScript $ typedLogicValidator logicSettings

logicValHash :: LogicSettings -> Ledger.ValidatorHash
logicValHash logicSettings = validatorHash (logicValidator logicSettings)