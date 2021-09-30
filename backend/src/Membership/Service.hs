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

module Membership.Service where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger (POSIXTime, PubKeyHash, Value)
import qualified PlutusTx
import PlutusTx.Prelude (Bool (False, True), BuiltinByteString, Eq, Integer, (&&), (==))
import Prelude (Show (..))
import qualified Prelude

-- A data type that determines if a service is constant  or one-time.
-- The former needs no arguments, the latter needs the service price
-- and how much time will be needed to complete it
data ServiceType = CConstant | OneTime Value POSIXTime
  deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq ServiceType where
  {-# INLINEABLE (==) #-}
  CConstant == CConstant = True
  OneTime p d == OneTime p' d' = p == p' && d == d'
  _ == _ = False

PlutusTx.unstableMakeIsData ''ServiceType

-- The essential information about a contract, including the price if it has one
data Service = Service
  { sPublisher :: PubKeyHash, -- The pkh of the person that published this service
    sTitle :: BuiltinByteString, -- Brief description of the sevice
    sDescription :: BuiltinByteString, -- In-depth description
    sTrust :: Integer, -- Amount of DSET tokens hold as collateral
    sType :: ServiceType
  }
  deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq Service where
  {-# INLINEABLE (==) #-}
  (Service pu ti d tr tp) == (Service pu' ti' d' tr' tp') =
    pu == pu' && ti == ti' && d == d' && tr == tr' && tp == tp'

PlutusTx.unstableMakeIsData ''Service