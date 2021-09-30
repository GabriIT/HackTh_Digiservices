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

module Spec.Trace where

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
import Spec.Sample
import Membership.OnChain.Account

createAccountTrace :: Wallet -> EmulatorTrace ()
createAccountTrace w = do
  h <- activateContractWallet w accountEndpoints
  callEndpoint @"create-account" h platformSettings

createAccountsTrace :: [Wallet] -> EmulatorTrace ()
createAccountsTrace [] = void $ Emulator.waitNSlots 5
createAccountsTrace (w : ws) = do
  createAccountTrace w
  createAccountsTrace ws

collectFeesTrace :: ContractHandle () AccountSchema Text -> AccountSettings -> EmulatorTrace ()
collectFeesTrace handle as = do
  callEndpoint @"collect-fees" handle as
  void $ Emulator.waitNSlots 3

createContractTrace :: AccountSettings -> ContractDatum -> Wallet -> EmulatorTrace (Maybe AssetClass)
createContractTrace as cd w = do
  h <- activateContractWallet w contractEndpoints
  callEndpoint @"create-contract" h (as, cd)

  void $ Emulator.waitNSlots 3

  Last maybeContractNFT <- observableState h
  Prelude.return maybeContractNFT

signTrace ::
  ContractHandle (Last AssetClass) ContractSchema Text ->
  AccountSettings ->
  AssetClass ->
  Role ->
  EmulatorTrace ()
signTrace handle as contractNFT role = do
  callEndpoint @"sign" handle (role, as, contractNFT)
  void $ Emulator.waitNSlots 3

cancelTrace ::
  ContractHandle (Last AssetClass) ContractSchema Text ->
  AccountSettings ->
  AssetClass ->
  Bool ->
  EmulatorTrace ()
cancelTrace handle as contractNFT invInAcc = do
  callEndpoint @"cancel" handle (invInAcc, as, contractNFT)
  void $ Emulator.waitNSlots 3

leaveTrace ::
  ContractHandle (Last AssetClass) ContractSchema Text ->
  AccountSettings ->
  AssetClass ->
  EmulatorTrace ()
leaveTrace handle as contractNFT = do
  callEndpoint @"leave" handle (as, contractNFT)
  void $ Emulator.waitNSlots 3

reviewTrace ::
  ContractHandle (Last AssetClass) ContractSchema Text ->
  AccountSettings ->
  AssetClass ->
  Integer ->
  BuiltinByteString ->
  EmulatorTrace ()
reviewTrace handle as contractNFT rev des = do
  callEndpoint @"review" handle (rev, des, as, contractNFT)
  void $ Emulator.waitNSlots 3

createLogicTrace ::
  AccountSettings ->
  LogicSettings ->
  BuiltinByteString ->
  Wallet ->
  EmulatorTrace (Maybe AssetClass)
createLogicTrace as ls key w = do
  h <- activateContractWallet w logicEndpoints
  callEndpoint @"create-logic" h (as, ls, key)

  void $ Emulator.waitNSlots 3

  Last maybeShameToken <- observableState h
  Prelude.return maybeShameToken

accuseTrace ::
  ContractHandle (Last AssetClass) LogicSchema Text ->
  PubKeyHash ->
  AccountSettings ->
  LogicSettings ->
  AssetClass ->
  AssetClass ->
  EmulatorTrace ()
accuseTrace handle accused as ls contractNFT shame = do
  callEndpoint @"accuse" handle (accused, as, ls, contractNFT, shame)
  void $ Emulator.waitNSlots 3

mediateTrace ::
  ContractHandle (Last AssetClass) LogicSchema Text ->
  Verdict ->
  AccountSettings ->
  LogicSettings ->
  AssetClass ->
  EmulatorTrace ()
mediateTrace handle verdict as ls shame = do
  callEndpoint @"mediate" handle (verdict, as, ls, shame)
  void $ Emulator.waitNSlots 3

logicCollectTrace ::
  ContractHandle (Last AssetClass) LogicSchema Text ->
  AccountSettings ->
  LogicSettings ->
  AssetClass ->
  AssetClass ->
  EmulatorTrace ()
logicCollectTrace handle as ls contractNFT shame = do
  callEndpoint @"collect" handle (as, ls, contractNFT, shame)
  void $ Emulator.waitNSlots 3

mintingTrace :: EmulatorTrace ()
mintingTrace = createAccountsTrace [Wallet 1, Wallet 2]