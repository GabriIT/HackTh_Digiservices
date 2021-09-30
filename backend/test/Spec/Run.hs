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

module Spec.Run where

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
  )
import qualified PlutusTx.Ratio as R
import Test.Tasty ()
import Prelude (IO, Show (..), String)
import qualified Prelude
import Spec.Example
import Spec.Sample
import Membership.OnChain.Account

-- An example of an account being created
runCreateAccountExample :: IO ()
runCreateAccountExample = runEmulatorTraceIO' def (abstractConfig 1) createAccountExample

-- An example of a contract being created
runCreateContractExample :: IO ()
runCreateContractExample = runEmulatorTraceIO' def (abstractConfig 1) createContractExample

-- An example of a logic being created
runCreateLogicExample :: IO ()
runCreateLogicExample = runEmulatorTraceIO' def (abstractConfig 1) createLogicExample

-- An example of a contract being signed
runSignContractExample :: IO ()
runSignContractExample = runEmulatorTraceIO' def (abstractConfig 2) signContractExample

-- An example of a user being accused
runAccuseExample :: IO ()
runAccuseExample = runEmulatorTraceIO' def (abstractConfig 3) accuseExample

-- An example of a conflict being mediated

-- At this moment, the values don't actually change, but the datum
-- does, preparing the script for the next step (collect), where tokens
-- will be distributed according to the verdict
runMediateExample :: IO ()
runMediateExample = runEmulatorTraceIO' def (abstractConfig 3) mediateExample

runLogicCollectExample :: IO ()
runLogicCollectExample = runEmulatorTraceIO' def (abstractConfig 3) logicCollectExample

runLeaveContractExample :: IO ()
runLeaveContractExample = runEmulatorTraceIO' def (abstractConfig 3) leaveContractExample

abstractConfig :: Integer -> EmulatorConfig
abstractConfig n = EmulatorConfig (Left $ Map.fromList [(Wallet w, setValue w) | w <- [1 .. n]]) def def
  where
    setValue :: Integer -> Value
    setValue _ =
      Ada.lovelaceValueOf 1_000_000_000
        <> assetClassValue platformToken 1_000_000