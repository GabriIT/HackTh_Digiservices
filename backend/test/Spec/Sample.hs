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

module Spec.Sample where

import Control.Monad (void)
import Control.Monad.Freer.Extras as Extras (logError, logInfo)
import Data.Default (Default (..))
import qualified Data.Map as Map
import Data.Monoid (Last (..))
import Data.Text (Text, pack)
import Ledger
import Ledger.Ada as Ada (lovelaceValueOf)
import Ledger.TimeSlot
import Ledger.Value (AssetClass (AssetClass), assetClass, assetClassValue)
import Membership.Contract
import Membership.Logic
import Membership.OffChain.Account
import Membership.OffChain.Contract
import Membership.OffChain.Logic
import Membership.OnChain.Contract
import Membership.OnChain.Logic
import Membership.OnChain.ShameToken
import Membership.OnChain.Signature
import Membership.PlatformSettings
import Membership.Service
import Membership.ShameToken
import Plutus.Contract.Test (Wallet (Wallet), walletPubKey)
import Plutus.Trace.Emulator as Emulator
  ( ContractHandle,
    EmulatorConfig (EmulatorConfig),
    EmulatorTrace,
    activateContractWallet,
    callEndpoint,
    observableState,
    runEmulatorTraceIO',
    waitNSlots,
  )
import PlutusTx.AssocMap as M
import PlutusTx.Prelude
  ( Bool (..),
    BuiltinByteString,
    Either (Left),
    Integer,
    Maybe (Just, Nothing),
    Semigroup ((<>)),
    ($),
    (++),
    (-),
    (*)
  )
import qualified PlutusTx.Ratio as R
import Prelude (IO, Show (..), String)
import qualified Prelude

{-# INLINEABLE platformToken #-}
platformToken :: AssetClass
platformToken = AssetClass ("aa", "DSET")

{-# INLINABLE samplePrice #-}
samplePrice :: Value
samplePrice = assetClassValue platformToken 100_000

{-# INLINABLE sampleTime #-}
sampleTime :: POSIXTime
sampleTime = POSIXTime (10 * 86400) -- 10 Days

{-# INLINEABLE sampleService #-}
sampleService :: Service
sampleService =
  Service
    { sPublisher = pubKeyHash $ walletPubKey $ Wallet 1,
      sTitle = "Title",
      sDescription = "Description",
      sTrust = 50_000,
      sType = OneTime samplePrice sampleTime
    }

{-# INLINEABLE beginningOfTime #-}
beginningOfTime :: Integer
beginningOfTime = 1596059091000

{-# INLINEABLE sampleEntranceFee #-}
sampleEntranceFee :: Integer
sampleEntranceFee = 300_000

{-# INLINEABLE sampleTxFee #-}
sampleTxFee :: Integer
sampleTxFee = 1_000

{-# INLINEABLE key #-}
key :: BuiltinByteString
key = "aaab"

{-# INLINEABLE judges #-}
judges :: Judges
judges =
  Judges
    { jsPubKeyHashes = [pubKeyHash $ walletPubKey $ Wallet w | w <- [3 .. 10]],
      jsPrice = 10_000,
      jsMaxDuration = slotToBeginPOSIXTime def 20 - POSIXTime beginningOfTime -- 20 slots to complete mediation
    }

{-# INLINEABLE platformSettings #-}
platformSettings :: PlatformSettings
platformSettings =
  PlatformSettings
    { psVersion = 1,
      psToken = platformToken,
      psShameTokenSymbol = shameTokenCurrencySymbol,
      psCASMap = currentCASMap,
      psEntranceFee = sampleEntranceFee,
      psTxFee = sampleTxFee
    }

{-# INLINABLE accountSettings #-}
accountSettings :: AccountSettings
accountSettings = getAccountSettings platformSettings

{-# INLINEABLE signatureSymbol #-}
signatureSymbol :: CurrencySymbol
signatureSymbol = signatureCurrencySymbol platformSettings

{-# INLINEABLE sampleContractSettings #-}
sampleContractSettings :: ContractSettings
sampleContractSettings =
  ContractSettings
    { csPlatformSettings = platformSettings,
      csSignatureSymbol = signatureSymbol
    }

{-# INLINEABLE contractValidatorHash #-}
contractValidatorHash :: ValidatorHash
contractValidatorHash = validatorHash $ contractValidator sampleContractSettings

{-# INLINEABLE input1 #-}
input1 :: Input
input1 =
  Input
    { iRoles = [Publisher],
      iQuestion = "Did the accused actually write and deliver a book?",
      iExpectedAnswer = True,
      iGuiltEnabled = True
    }

{-# INLINEABLE input2 #-}
input2 :: Input
input2 =
  Input
    { iRoles = [Publisher],
      iQuestion = "Did the book written by the accused have more than 200 pages?",
      iExpectedAnswer = True,
      iGuiltEnabled = True
    }

{-# INLINEABLE input3 #-}
input3 :: Input
input3 =
  Input
    { iRoles = [Publisher],
      iQuestion = "Did the book written by the accused have more than 100,000 readers?",
      iExpectedAnswer = True,
      iGuiltEnabled = True
    }

{-# INLINEABLE input4 #-}
input4 :: Input
input4 =
  Input
    { iRoles = [Client],
      iQuestion = "Was the accused collaborative?",
      iExpectedAnswer = True,
      iGuiltEnabled = True
    }

{-# INLINEABLE input5 #-}
input5 :: Input
input5 =
  Input
    { iRoles = [Client, Publisher],
      iQuestion = "Was the accused disrespectful?",
      iExpectedAnswer = False,
      iGuiltEnabled = True
    }

{-# INLINEABLE sampleLogic #-}
sampleLogic :: Logic
sampleLogic =
  M.fromList
    [ ( CAny
          [ CCondition input1,
            CCondition input2,
            CCondition input3,
            CCondition input4,
            CCondition input5
          ],
        Proportion 1 0
      ),
      (CAny [], Proportion 0 1)
    ]

{-# INLINEABLE logicSettings #-}
logicSettings :: LogicSettings
logicSettings =
  LogicSettings
    { lsPlatformSettings = platformSettings,
      lsSignatureSymbol = signatureSymbol,
      lsContract = contractValidatorHash,
      lsLogic = sampleLogic
    }

{-# INLINEABLE sampleLogicValHash #-}
sampleLogicValHash :: ValidatorHash
sampleLogicValHash = logicValHash logicSettings

-- ! We need to make sure that the shame token has the publisher pkh in it's token name
-- ! as well as the contract validator has
sampleContractDatum :: ContractDatum
sampleContractDatum =
  ContractDatum
    { cdJudges = judges,
      cdLogicScript = sampleLogicValHash,
      cdAccusations = [],
      cdService = sampleService,
      cdRoleMap = M.fromList [(pubKeyHash $ walletPubKey $ Wallet 1, Publisher)]
    }