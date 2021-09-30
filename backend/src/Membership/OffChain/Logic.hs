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

module Membership.OffChain.Logic where

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
    mintingPolicy,
    mustBeSignedBy,
    mustMintValue,
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
    AccountReturnType (..),
    AccountType,
    addContract,
    addReview,
    contractCreationCAS,
    declaredGuiltyCAS,
    findAccountDatum,
    removeContract,
    signContractCAS,
  )
import Membership.Contract
import Membership.Logic
import Membership.OffChain.Utils
import Membership.OnChain.Account
import Membership.OnChain.Contract
import Membership.OnChain.Logic
import Membership.OnChain.ShameToken
import Membership.PlatformSettings
import Membership.Service
import Membership.ShameToken
import Membership.Signature
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
    fst,
    isJust,
    isNothing,
    length,
    map,
    mempty,
    negate,
    otherwise,
    return,
    snd,
    traceError,
    ($),
    (&&),
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
import Prelude (Semigroup (..), Show (..), String, uncurry)

-- There should be one unique logic for this user with the same hash,
-- since different logics and inputs would make the hash different
createLogic :: AccountSettings -> LogicSettings -> BuiltinByteString -> Contract (Last AssetClass) s Text ()
createLogic accountSettings logicSettings key = do
  -- The signer public key hash
  pkh <- pubKeyHash <$> Contract.ownPubKey

  let platformSettings :: PlatformSettings
      platformSettings = asPlatformSettings accountSettings

      sigSymbol, stSymbol :: CurrencySymbol
      sigSymbol = asSignatureSymbol accountSettings
      stSymbol = shameTokenCurrencySymbol

      logValHash :: ValidatorHash
      logValHash = logicValHash logicSettings

      logicDatum :: LogicState
      logicDatum = LSWaitingStart

      st :: ShameToken
      st = shameToken pkh logValHash key

      shameTokenAssetClass :: AssetClass
      shameTokenAssetClass = shameAssetClass stSymbol pkh logValHash key

      shameTokenValue :: Value
      shameTokenValue = shameValue stSymbol st

      logicValue :: Value
      logicValue = shameTokenValue

      lookups :: ScriptLookups AccountType
      lookups =
        Constraints.mintingPolicy shameTokenPolicy

      tx :: TxConstraints (RedeemerType AccountType) (DatumType AccountType)
      tx =
        Constraints.mustBeSignedBy pkh
          <> Constraints.mustMintValue shameTokenValue
          <> Constraints.mustPayToOtherScript
            logValHash
            (Datum $ PlutusTx.toBuiltinData logicDatum)
            logicValue

  -- Submit transaction
  ledgerTx <- submitTxConstraintsWith @AccountType lookups tx

  -- -- Wait until transaction is confirmed
  awaitTxConfirmed $ txId ledgerTx

  tell $ Last $ Just shameTokenAssetClass

  logInfo @String $
    "Create Logic - Logic succefully created"

-- ShameToken should be an NFT
accuse ::
  PubKeyHash ->
  AccountSettings ->
  LogicSettings ->
  AssetClass ->
  AssetClass ->
  Contract w s Text ()
accuse accused accountSettings logicSettings contractNFT shameTokenAssetClass = do
  -- The accuser public key hash
  accuser <- pubKeyHash <$> Contract.ownPubKey

  -- Current POSIXTime
  curTime <- Contract.currentTime

  -- Tries to get the contract off-chain essentials
  maybeContractOffChainEssentials <- getContractOffChainEssentials accountSettings contractNFT

  -- Tries to get the logic off-chain essentials
  maybeLogicOffChainEssentials <- getLogicOffChainEssentials logicSettings shameTokenAssetClass

  case (maybeContractOffChainEssentials, maybeLogicOffChainEssentials) of
    (Just coe, Just loe) -> do
      case tx of
        Just tx' -> do
          -- Submit transaction
          ledgerTx <- submitTxConstraintsWith @LogicType lookups tx'

          -- Wait until transaction is confirmed
          awaitTxConfirmed $ txId ledgerTx

          logInfo @String $ "Accuse - Successfuly accused user " ++ show accused
        Nothing
          | isNothing maybeAccuserRole -> logError @String "Accuse - Accuser not registered"
          | isNothing maybeAccusedRole -> logError @String "Accuse - Accused not registered"
          | isNothing maybeJudgePKH -> logError @String "Accuse - No valid judge found"
          | otherwise -> logError @String "Accuse - Accusation failed for unknown reasons"
      where
        -- ! Do not trace errors here, it's impossible to debug

        sigSymbol :: CurrencySymbol
        sigSymbol = asSignatureSymbol accountSettings

        key :: BuiltinByteString
        key = stKey $ makeShameToken (snd $ unAssetClass shameTokenAssetClass)

        platformSettings :: PlatformSettings
        platformSettings = asPlatformSettings accountSettings

        platformToken :: AssetClass
        platformToken = psToken platformSettings

        accValHash :: ValidatorHash
        accValHash = accountValidatorHash accountSettings

        contrValHash :: ValidatorHash
        contrValHash = asContractValidatorHash accountSettings

        logValHash :: ValidatorHash
        logValHash = loeLogicValidatorHash loe

        logicReference, contractReference :: TxOutRef
        logicReference = loeLogicReference loe
        contractReference = coeContractReference coe

        logicOut, contractOut :: TxOut
        logicOut = loeLogicOut loe
        contractOut = coeContractOut coe

        logicOutTx, contractOutTx :: TxOutTx
        logicOutTx = loeLogicOutTx loe
        contractOutTx = coeContractOutTx coe

        contractDatum :: ContractDatum
        contractDatum = coeContractDatum coe

        maybeAccuserRole :: Maybe Role
        maybeAccuserRole = AM.lookup accuser (cdRoleMap contractDatum)

        maybeAccusedRole :: Maybe Role
        maybeAccusedRole = AM.lookup accused (cdRoleMap contractDatum)

        maybeNewContractDatum :: Maybe ContractDatum
        maybeNewContractDatum = do
          accuserRole <- maybeAccuserRole
          accusedRole <- maybeAccusedRole
          return $
            accuseUser
              (accuser, accuserRole)
              (accused, accusedRole)
              curTime
              contractDatum

        contractSett :: ContractSettings
        contractSett = coeContractSettings coe

        sigs :: [Sig]
        sigs = findSignatures (lsSignatureSymbol logicSettings) (txOutValue contractOut)

        judges :: Judges
        judges = cdJudges contractDatum

        maybeJudgePKH :: Maybe PubKeyHash
        maybeJudgePKH = firstValidJudge (jsPubKeyHashes judges) sigs

        maybeJudge :: Maybe Judge
        maybeJudge = do
          judgePKH <- maybeJudgePKH
          return $
            Judge
              { jPubKeyHash = judgePKH,
                jPrice = jsPrice judges,
                jMaxDuration = jsMaxDuration judges
              }

        maybeAccusation :: Maybe Accusation
        maybeAccusation = do
          accuserRole <- maybeAccuserRole
          accusedRole <- maybeAccusedRole
          return $
            Accusation
              { aAccuser = (accuser, accuserRole),
                aAccused = (accused, accusedRole),
                aTime = curTime
              }

        maybeLogicDatum :: Maybe LogicState
        maybeLogicDatum = do
          judge <- maybeJudge
          accusation <- maybeAccusation
          return $ LSWaitingVerdict contractNFT judge accusation

        judgePrice :: Integer
        judgePrice = jsPrice $ cdJudges contractDatum

        maybeJudgeSigValue :: Maybe Value
        maybeJudgeSigValue = do
          judgePKH <- maybeJudgePKH
          return $ singleton sigSymbol (makeSigToken judgePKH accValHash) 1

        accusedSigValue :: Value
        accusedSigValue = singleton sigSymbol (makeSigToken accused accValHash) 1

        judgeValue, trustValue :: Value
        judgeValue = assetClassValue platformToken judgePrice
        trustValue = assetClassValue platformToken (sTrust $ cdService contractDatum)

        maybeContractValue, maybeLogicValue :: Maybe Value
        maybeContractValue = do
          judgeSigValue <- maybeJudgeSigValue
          return $
            txOutValue contractOut
              <> negate
                ( trustValue
                    <> trustValue
                    <> judgeSigValue
                    <> accusedSigValue
                )
        maybeLogicValue = do
          judgeSigValue <- maybeJudgeSigValue
          return $
            txOutValue logicOut
              <> trustValue
              <> trustValue
              <> judgeValue
              <> judgeSigValue
              <> accusedSigValue

        lookups :: ScriptLookups LogicType
        lookups =
          Constraints.unspentOutputs
            ( Map.fromList
                [ (contractReference, contractOutTx),
                  (logicReference, logicOutTx)
                ]
            )
            <> Constraints.otherScript (contractValidator contractSett)
            <> Constraints.otherScript (logicValidator logicSettings)
            <> Constraints.typedValidatorLookups (typedLogicValidator logicSettings)

        tx :: Maybe (TxConstraints (RedeemerType LogicType) (DatumType LogicType))
        tx = do
          contractValue <- maybeContractValue
          logicValue <- maybeLogicValue
          accusation <- maybeAccusation
          logicDatum <- maybeLogicDatum
          newContractDatum <- maybeNewContractDatum

          return $
            Constraints.mustBeSignedBy accuser
              <> Constraints.mustSpendScriptOutput
                contractReference
                (Redeemer $ PlutusTx.toBuiltinData $ CAccuse accuser accused)
              <> Constraints.mustSpendScriptOutput
                logicReference
                (Redeemer $ PlutusTx.toBuiltinData $ LRAccuse accusation)
              <> Constraints.mustPayToOtherScript
                contrValHash
                (Datum $ PlutusTx.toBuiltinData newContractDatum)
                contractValue
              <> Constraints.mustPayToTheScript logicDatum logicValue
    _ -> logError @String "Accuse - Contract or Logic essentials not found"

mediate ::
  Verdict ->
  AccountSettings ->
  LogicSettings ->
  AssetClass ->
  Contract w s Text ()
mediate verdict accountSettings logicSettings shameTokenAssetClass = do
  -- The user that called this endpoint should be the judge
  judge <- pubKeyHash <$> Contract.ownPubKey

  -- Tries to get the logic off-chain essentials
  maybeLogicOffChainEssentials <- getLogicOffChainEssentials logicSettings shameTokenAssetClass

  case maybeLogicOffChainEssentials of
    Just loe -> do
      let logicDatum :: LogicState
          logicDatum = loeLogicDatum loe

      case logicDatum of
        LSWaitingVerdict contractNFT judgeInfo accusation -> do
          -- Submit transaction
          ledgerTx <- submitTxConstraintsWith @LogicType lookups tx

          -- Wait until transaction is confirmed
          awaitTxConfirmed $ txId ledgerTx

          logInfo @String $ "Mediate - Conflict successfully mediated"
          where
            sigSymbol :: CurrencySymbol
            sigSymbol = asSignatureSymbol accountSettings

            platformSettings :: PlatformSettings
            platformSettings = asPlatformSettings accountSettings

            platformToken :: AssetClass
            platformToken = psToken platformSettings

            accValHash :: ValidatorHash
            accValHash = accountValidatorHash accountSettings

            contrValHash :: ValidatorHash
            contrValHash = asContractValidatorHash accountSettings

            logValHash :: ValidatorHash
            logValHash = loeLogicValidatorHash loe

            logicReference :: TxOutRef
            logicReference = loeLogicReference loe

            logicOut :: TxOut
            logicOut = loeLogicOut loe

            logicOutTx :: TxOutTx
            logicOutTx = loeLogicOutTx loe

            newLogicDatum :: LogicState
            newLogicDatum = LSWaitingEnd contractNFT judgeInfo accusation verdict

            logicValue :: Value
            logicValue = txOutValue logicOut

            lookups :: ScriptLookups LogicType
            lookups =
              Constraints.unspentOutputs
                ( Map.fromList [(logicReference, logicOutTx)]
                )
                <> Constraints.otherScript (logicValidator logicSettings)
                <> Constraints.typedValidatorLookups (typedLogicValidator logicSettings)

            tx :: TxConstraints (RedeemerType LogicType) (DatumType LogicType)
            tx =
              Constraints.mustBeSignedBy judge
                <> Constraints.mustSpendScriptOutput
                  logicReference
                  (Redeemer $ PlutusTx.toBuiltinData $ LRMediate verdict)
                <> Constraints.mustPayToTheScript newLogicDatum logicValue
        _ -> logError @String "Mediate - Logic in wrong state"
    Nothing -> logError @String "Mediate - Logic essentials not found"

collect ::
  AccountSettings ->
  LogicSettings ->
  AssetClass ->
  AssetClass ->
  Contract w s Text ()
collect accountSettings logicSettings contractNFT shameTokenAssetClass = do
  -- The public key hash from the user that is trying
  -- to collect the tokens from the logic script
  pkh <- pubKeyHash <$> Contract.ownPubKey

  -- Tries to get the contract off-chain essentials
  maybeContractOffChainEssentials <- getContractOffChainEssentials accountSettings contractNFT

  -- Tries to get the logic off-chain essentials
  maybeLogicOffChainEssentials <- getLogicOffChainEssentials logicSettings shameTokenAssetClass

  case (maybeContractOffChainEssentials, maybeLogicOffChainEssentials) of
    (Just coe, Just loe) -> do
      let logicDatum :: LogicState
          logicDatum = loeLogicDatum loe

      case logicDatum of
        LSWaitingEnd contractNFT judge accusation verdict -> do
          let sigSymbol :: CurrencySymbol
              sigSymbol = asSignatureSymbol accountSettings

              platformSettings :: PlatformSettings
              platformSettings = asPlatformSettings accountSettings

              platformToken :: AssetClass
              platformToken = psToken platformSettings

              accValHash :: ValidatorHash
              accValHash = accountValidatorHash accountSettings

              contrValHash :: ValidatorHash
              contrValHash = coeContractValidatorHash coe

              contractOut, logicOut :: TxOut
              contractOut = coeContractOut coe
              logicOut = loeLogicOut loe

              contractOutTx, logicOutTx :: TxOutTx
              contractOutTx = coeContractOutTx coe
              logicOutTx = loeLogicOutTx loe

              contractDatum :: ContractDatum
              contractDatum = coeContractDatum coe

              contractReference, logicReference :: TxOutRef
              contractReference = coeContractReference coe
              logicReference = loeLogicReference loe

              contractSett :: ContractSettings
              contractSett = coeContractSettings coe

              trust :: Integer
              trust = sTrust $ cdService contractDatum

              accuser :: PubKeyHash
              accuser = fst $ aAccuser accusation

              accused :: PubKeyHash
              accused = fst $ aAccused accusation

              proportion :: Maybe Proportion
              proportion = failedProportion (snd $ aAccused accusation) verdict (lsLogic logicSettings)

              failed :: Bool
              failed = isJust proportion

              judgeValue :: Value
              judgeValue = assetClassValue platformToken (jPrice judge)

              accusedSigValue, judgeSigValue, trustValue, shameTokenValue :: Value
              accusedSigValue = singleton sigSymbol (makeSigToken accused accValHash) 1
              judgeSigValue = singleton sigSymbol (makeSigToken (jPubKeyHash judge) accValHash) 1
              trustValue = assetClassValue (psToken platformSettings) trust
              shameTokenValue = assetClassValue shameTokenAssetClass 1

              priceValue :: Value
              priceValue = case sType (cdService contractDatum) of
                OneTime v _
                  | (snd (aAccuser accusation) == Client)
                      && (snd (aAccused accusation) == Publisher) ->
                    v
                _ -> mempty

              guilty :: Bool
              guilty = isGuilty (snd $ aAccused accusation) (lsLogic logicSettings) verdict

              expectedContractValueDifference :: Value
              expectedContractValueDifference =
                if failed
                  then
                    judgeSigValue
                      <> trustValue
                      <> negate priceValue
                  else
                    accusedSigValue
                      <> judgeSigValue
                      <> trustValue
                      <> trustValue

              contractValue :: Value
              contractValue = txOutValue contractOut <> expectedContractValueDifference

              newContractDatum :: ContractDatum
              newContractDatum =
                ContractDatum
                  { cdJudges = cdJudges contractDatum,
                    cdLogicScript = cdLogicScript contractDatum,
                    cdAccusations = cdAccusations (removeAccusation accusation contractDatum),
                    cdService = cdService contractDatum,
                    cdRoleMap =
                      if failed
                        then AM.delete accused (cdRoleMap contractDatum)
                        else cdRoleMap contractDatum
                  }

              distributionConstraints :: TxConstraints (RedeemerType LogicType) (DatumType LogicType)
              distributionConstraints =
                case proportion of
                  Just p ->
                    Constraints.mustPayToPubKey (jPubKeyHash judge) judgeValue
                      <> Constraints.mustPayToPubKey accuser accuserValue
                      <> Constraints.mustPayToPubKey accused accusedValue
                    where
                      accuserValue :: Value
                      accuserValue =
                        assetClassValue
                          platformToken
                          (fst $ trustProportion p trust)
                          <> priceValue

                      accusedValue :: Value
                      accusedValue = assetClassValue platformToken (snd $ trustProportion p trust)
                  Nothing -> Constraints.mustPayToPubKey (jPubKeyHash judge) judgeValue

              innocentLookups :: ScriptLookups LogicType
              innocentLookups =
                Constraints.unspentOutputs
                  ( Map.fromList
                      [ (contractReference, contractOutTx),
                        (logicReference, logicOutTx)
                      ]
                  )
                  <> Constraints.otherScript (contractValidator contractSett)
                  <> Constraints.otherScript (logicValidator logicSettings)
                  <> Constraints.typedValidatorLookups (typedLogicValidator logicSettings)

              innocentTx :: TxConstraints (RedeemerType LogicType) (DatumType LogicType)
              innocentTx =
                Constraints.mustBeSignedBy pkh
                  <> distributionConstraints
                  <> Constraints.mustSpendScriptOutput
                    contractReference
                    (Redeemer $ PlutusTx.toBuiltinData CMediate)
                  <> Constraints.mustSpendScriptOutput
                    logicReference
                    (Redeemer $ PlutusTx.toBuiltinData LRConsume)
                  <> Constraints.mustPayToOtherScript
                    contrValHash
                    (Datum $ PlutusTx.toBuiltinData newContractDatum)
                    contractValue

          if guilty
            then do
              -- Tries to get the account off-chain essentials
              maybeAccountOffChainEssentials <-
                getAccountOffChainEssentials
                  accountSettings
                  (fst $ aAccused accusation)

              case maybeAccountOffChainEssentials of
                Just aoe -> do
                  let accountOut :: TxOut
                      accountOut = aoeAccountOut aoe

                      accountOutTx :: TxOutTx
                      accountOutTx = aoeAccountOutTx aoe

                      accountDatum :: AccountDatum
                      accountDatum = aoeAccountDatum aoe

                      accountReference :: TxOutRef
                      accountReference = aoeAccountReference aoe

                      accountValue :: Value
                      accountValue = txOutValue accountOut <> shameTokenValue <> accusedSigValue

                      guiltyLookups :: ScriptLookups LogicType
                      guiltyLookups =
                        Constraints.unspentOutputs
                          ( Map.fromList
                              [ (accountReference, accountOutTx),
                                (contractReference, contractOutTx),
                                (logicReference, logicOutTx)
                              ]
                          )
                          <> Constraints.otherScript (accountValidator accountSettings)
                          <> Constraints.otherScript (contractValidator contractSett)
                          <> Constraints.otherScript (logicValidator logicSettings)
                          <> Constraints.typedValidatorLookups (typedLogicValidator logicSettings)

                      guiltyTx :: TxConstraints (RedeemerType LogicType) (DatumType LogicType)
                      guiltyTx =
                        Constraints.mustBeSignedBy pkh
                          <> distributionConstraints
                          <> Constraints.mustSpendScriptOutput
                            accountReference
                            (Redeemer $ PlutusTx.toBuiltinData (AReturn ARTExpelled))
                          <> Constraints.mustSpendScriptOutput
                            contractReference
                            (Redeemer $ PlutusTx.toBuiltinData CMediate)
                          <> Constraints.mustSpendScriptOutput
                            logicReference
                            (Redeemer $ PlutusTx.toBuiltinData LRConsume)
                          <> Constraints.mustPayToOtherScript
                            contrValHash
                            (Datum $ PlutusTx.toBuiltinData newContractDatum)
                            contractValue
                          <> Constraints.mustPayToOtherScript
                            accValHash
                            ( Datum $
                                PlutusTx.toBuiltinData
                                  ( declaredGuiltyCAS
                                      (psCASMap platformSettings)
                                      (removeContract accountDatum contrValHash)
                                  )
                            )
                            accountValue

                  -- Submit transaction
                  ledgerTx <- submitTxConstraintsWith @LogicType guiltyLookups guiltyTx

                  -- Wait until transaction is confirmed
                  awaitTxConfirmed $ txId ledgerTx

                  logInfo @String $ "Logic Collect - Collaterals and rewards successfully distributed"
            else do
              -- Submit transaction
              ledgerTx <- submitTxConstraintsWith @LogicType innocentLookups innocentTx

              -- Wait until transaction is confirmed
              awaitTxConfirmed $ txId ledgerTx

              logInfo @String $ "Logic Collect - Collaterals and rewards successfully distributed"
          where
        _ -> logError @String "Logic Collect - Logic in wrong state"
    _ -> logError @String "Logic Collect - Account, Contract or Logic essentials not found"

type LogicSchema =
  Endpoint "create-logic" (AccountSettings, LogicSettings, BuiltinByteString)
    .\/ Endpoint "accuse" (PubKeyHash, AccountSettings, LogicSettings, AssetClass, AssetClass)
    .\/ Endpoint "mediate" (Verdict, AccountSettings, LogicSettings, AssetClass)
    .\/ Endpoint "collect" (AccountSettings, LogicSettings, AssetClass, AssetClass)

logicEndpoints :: Contract (Last AssetClass) LogicSchema Text ()
logicEndpoints =
  forever $
    handleError logError $
      awaitPromise $
        createLogic' `select` accuse' `select` mediate' `select` collect'
  where
    createLogic' = endpoint @"create-logic" $ \(as, ls, key) -> createLogic as ls key
    accuse' = endpoint @"accuse" $ \(acd, as, ls, cAC, lAC) -> accuse acd as ls cAC lAC
    mediate' = endpoint @"mediate" $ \(ver, as, ls, lAC) -> mediate ver as ls lAC
    collect' = endpoint @"collect" $ \(as, ls, cAC, lAC) -> collect as ls cAC lAC