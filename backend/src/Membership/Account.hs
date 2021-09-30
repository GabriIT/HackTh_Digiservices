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

module Membership.Account where

import Ledger
  ( Datum (Datum),
    DatumHash,
    PubKeyHash,
    Tx (..),
    TxOut (..),
    TxOutRef,
    TxOutTx,
    txOutDatum,
  )
import Ledger.Scripts (ValidatorHash)
import Ledger.Typed.Scripts as Scripts (ValidatorTypes (..))
import Ledger.Value (AssetClass, Value, assetClassValue)
import Membership.Contract
import Membership.PlatformSettings
import qualified PlutusTx
import qualified PlutusTx.AssocMap as M
import PlutusTx.Prelude
  ( AdditiveGroup ((-)),
    AdditiveSemigroup ((+)),
    Bool (..),
    Eq (..),
    Integer,
    Maybe (..),
    MultiplicativeSemigroup ((*)),
    ($),
    (&&),
    negate
  )
import qualified PlutusTx.Ratio as R
import qualified Prelude
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- The datatype that represents the account information on-chain
data AccountDatum = AccountDatum
  { adCAS :: Integer, -- Between 0 and 100,000
    adReviewCredit :: Integer, -- Number of DSET tokens the user received from reviews
    adReviews :: [Review],
    adContracts :: M.Map ValidatorHash AssetClass -- A list of the contract NFTs
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq AccountDatum where
  {-# INLINEABLE (==) #-}
  (AccountDatum cas rc rvs ctrs) == (AccountDatum cas' rc' rvs' ctrs') =
    cas == cas' && rc == rc' && rvs == rvs' && ctrs == ctrs'

PlutusTx.unstableMakeIsData ''AccountDatum

{-# INLINEABLE initDatum #-}
initDatum :: AccountDatum
initDatum = AccountDatum 60_000 0 [] (M.fromList [])

data AccountReturnType = ARTLeave | ARTCancel | ARTExpelled
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq AccountReturnType where
  {-# INLINABLE (==) #-}
  ARTLeave == ARTLeave = True
  ARTCancel == ARTCancel = True
  ARTExpelled == ARTExpelled = True
  _ == _ = False

PlutusTx.unstableMakeIsData ''AccountReturnType

data AccountRedeemer = ACreateContract
                     | ASign
                     | ACollect PubKeyHash
                     | AReview
                     | AReturn AccountReturnType
  deriving (Prelude.Show)

PlutusTx.unstableMakeIsData ''AccountRedeemer

data AccountOffChainEssentials = AccountOffChainEssentials
  { aoeAccountReference :: TxOutRef,
    aoeAccountOutTx :: TxOutTx,
    aoeAccountTx :: Tx,
    aoeAccountOut :: TxOut,
    aoeAccountDatum :: AccountDatum
  }
  deriving (Prelude.Show, Prelude.Eq)

data AccountType

instance Scripts.ValidatorTypes AccountType where
  type DatumType AccountType = AccountDatum
  type RedeemerType AccountType = AccountRedeemer

-- Given the asset class that represent's the platform token and an account datum,
-- returns the corresponding value of the review credit in DSET
{-# INLINEABLE userReviewCredit #-}
userReviewCredit :: AssetClass -> AccountDatum -> Value
userReviewCredit ac ad = assetClassValue ac (adReviewCredit ad)

{-# INLINEABLE userReviewCredit' #-}
userReviewCredit' :: PlatformSettings -> AccountDatum -> Value
userReviewCredit' ps = userReviewCredit (psToken ps)

-- Given the old CAS score and a percentage, return's the new score
{-# INLINEABLE calculateNewScore #-}
calculateNewScore :: Integer -> R.Rational -> Integer
calculateNewScore oldScore percentage =
  R.round $ R.fromInteger oldScore + percentage * R.fromInteger (100_000 - oldScore)

-- Increases CAS score based on a coefficient of 5% (the current value for contract creation)
{-# INLINEABLE contractCreationCAS #-}
contractCreationCAS :: CASMap -> AccountDatum -> AccountDatum
contractCreationCAS casMap (AccountDatum cas rc rvs ctrs) =
  AccountDatum (calculateNewScore cas (casContractCreation casMap)) rc rvs ctrs

-- Increases CAS score based on a coefficient of 2% (the current value for contract signing)
{-# INLINEABLE signContractCAS #-}
signContractCAS :: CASMap -> AccountDatum -> AccountDatum
signContractCAS casMap (AccountDatum cas rc rvs ctrs) =
  AccountDatum (calculateNewScore cas (casContractSigning casMap)) rc rvs ctrs

-- Apply the CAS difference in the Account Datum after a user is declared guilty
{-# INLINEABLE declaredGuiltyCAS #-}
declaredGuiltyCAS :: CASMap -> AccountDatum -> AccountDatum
declaredGuiltyCAS casMap (AccountDatum cas rc rvs ctrs) =
  AccountDatum (calculateNewScore cas (negate (casDeclaredGuilty casMap))) rc rvs ctrs

-- Apply the CAS difference in the Account Datum after a user
-- left a contract (without being involved in any accusation
-- or service offering)
{-# INLINEABLE leaveContractCAS #-}
leaveContractCAS :: CASMap -> AccountDatum -> AccountDatum
leaveContractCAS casMap (AccountDatum cas rc rvs ctrs) =
  AccountDatum (calculateNewScore cas (negate (casLeaveContract casMap))) rc rvs ctrs

-- Apply the CAS difference in the Account Datum after a user
-- cancelled a contract (while being involved in some activity)
{-# INLINEABLE cancelContractCAS #-}
cancelContractCAS :: CASMap -> AccountDatum -> AccountDatum
cancelContractCAS casMap (AccountDatum cas rc rvs ctrs) =
  AccountDatum (calculateNewScore cas (negate (casCancelContract casMap))) rc rvs ctrs

-- Given an old account datum, the contract validator hash and the asset class from
-- the NFT that identifies this contract, return's a new account datum with the
-- given contract added to the list of contracts
{-# INLINEABLE addContract #-}
addContract :: AccountDatum -> ValidatorHash -> AssetClass -> AccountDatum
addContract inputDatum contractHash contractNFT =
  AccountDatum
    { adCAS = adCAS inputDatum,
      adReviewCredit = adReviewCredit inputDatum,
      adReviews = adReviews inputDatum,
      adContracts = M.insert contractHash contractNFT (adContracts inputDatum)
    }

-- Given an old account datum, the contract validator hash and the asset class from
-- the NFT that identifies this contract, return's a new account datum with the
-- given contract removed from the list of contracts
{-# INLINEABLE removeContract #-}
removeContract :: AccountDatum -> ValidatorHash -> AccountDatum
removeContract inputDatum contractHash =
  AccountDatum
    { adCAS = adCAS inputDatum,
      adReviewCredit = adReviewCredit inputDatum,
      adReviews = adReviews inputDatum,
      adContracts = M.delete contractHash (adContracts inputDatum)
    }

-- TODO: Add to the review credit as well
-- Given an old account datum, and a review, return's a new account datum with the
-- given review being added to the list of reviews
{-# INLINEABLE addReview #-}
addReview :: AccountDatum -> Review -> AccountDatum
addReview inputDatum review =
  AccountDatum
    { adCAS = adCAS inputDatum,
      adReviewCredit = adReviewCredit inputDatum,
      adReviews = review : adReviews inputDatum,
      adContracts = adContracts inputDatum
    }

{-# INLINEABLE findAccountDatum #-}
findAccountDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe AccountDatum
findAccountDatum o f = do
  dh <- txOutDatum o
  Datum d <- f dh
  PlutusTx.fromBuiltinData d