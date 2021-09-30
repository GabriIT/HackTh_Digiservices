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

module Membership.PlatformSettings where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger.Ada as Ada ()
import Ledger.Constraints as Constraints ()
import Ledger.Crypto (PubKeyHash)
import Ledger.Scripts (ValidatorHash)
import Ledger.Value (AssetClass, CurrencySymbol)
import Plutus.Contract as Contract ()
import Plutus.Contract.StateMachine ()
import qualified PlutusTx
import PlutusTx.Prelude (Eq, Integer, (&&), (==))
import Prelude (Show (..))
import qualified Prelude
import qualified PlutusTx.Ratio as R

-- CASMap is a data type that contains every possible percentage of difference in CAS

-- If casContractCreation is equal to 5%, for example, the account validator should
-- only allow the contract creation if the CAS score in the output datum receives an
-- increase of 5% (it's actually a more complex formula, but the idea is the same)
data CASMap = CASMap
  { casContractCreation :: R.Rational
  , casContractSigning :: R.Rational
  , casDeclaredGuilty :: R.Rational
  , casLeaveContract :: R.Rational
  , casCancelContract :: R.Rational
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq CASMap where
  {-# INLINABLE (==) #-}
  (CASMap cc cs dg lc clc) == (CASMap cc' cs' dg' lc' clc') =
    cc == cc' && cs == cs' && dg == dg' && lc == lc' && clc == clc'

PlutusTx.unstableMakeIsData ''CASMap
PlutusTx.makeLift ''CASMap

-- The CASMap that should be used in this official DigiServices version
{-# INLINABLE currentCASMap #-}
currentCASMap :: CASMap
currentCASMap = CASMap
  { casContractCreation = 5 R.% 1000 -- 0.5%
  , casContractSigning = 1 R.% 1000 -- 0.1%
  , casDeclaredGuilty = (-5) R.% 100 -- -5%
  , casLeaveContract = 1 R.% 1000 -- 0.1%
  , casCancelContract = (-2) R.% 100 -- -2%
  }

-- The platform settings are the global variables that will
-- be used all over the platform. It has the following arguments:

-- The version is important to differentiate the multiple
-- versions of DigiServices, of course, it's not enough
-- and user's should always compare the hashes 

-- The token is the asset class corresponding to DSET, which is
-- used for every operation inside the platform

-- The ShameToken Symbol is the currency symbol from the token
-- that will signify someone once broke some contract's rules

-- The entrance fee is the amount of DSET tokens that will be needed
-- to enter the platform. Important to avoid fake accounts.

-- The tx fee is amount of DSET tokens that will be necessary
-- to complete some transactions
data PlatformSettings = PlatformSettings
  { psVersion :: !Integer,
    psToken :: !AssetClass,
    psShameTokenSymbol :: !CurrencySymbol,
    psCASMap :: !CASMap,
    psEntranceFee :: !Integer,
    psTxFee :: !Integer
  }
  deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq PlatformSettings where
  {-# INLINEABLE (==) #-}
  PlatformSettings ver tok sts cm ef txf == PlatformSettings ver' tok' sts' cm' ef' txf' =
    ver == ver'
      && tok == tok'
      && sts == sts'
      && cm == cm'
      && ef == ef'
      && txf == txf'

PlutusTx.makeLift ''PlatformSettings

-- The account settings are those variables used by the account validator

-- The platform settings are, as explained, the variables concerning the
-- whole platform

-- The signature symbol is the currency symbol from the SIG token

-- The contract validator hash is the validator hash from the "official" contracts.
-- Only these contracts will be able to receive SIG tokens when requested.

-- The collectors will not be in the main version and are the public key hashes
-- from the platform "owners" (of course this is against decentralisation and
-- should be removed)
data AccountSettings = AccountSettings
  { asPlatformSettings :: !PlatformSettings,
    asSignatureSymbol :: !CurrencySymbol,
    asContractValidatorHash :: !ValidatorHash,
    asCollectors :: ![PubKeyHash]
  }
  deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''AccountSettings

-- The contract settings are those variables used by the contract validator
data ContractSettings = ContractSettings
  { csPlatformSettings :: PlatformSettings,
    csSignatureSymbol :: CurrencySymbol
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq ContractSettings where
  {-# INLINEABLE (==) #-}
  ContractSettings ps ss == ContractSettings ps' ss' =
    ps == ps' && ss == ss'

PlutusTx.makeLift ''ContractSettings