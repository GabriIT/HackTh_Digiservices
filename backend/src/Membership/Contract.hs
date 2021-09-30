{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Membership.Contract where

import Control.Monad (Monad (return))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger
  ( Datum (Datum),
    DatumHash,
    POSIXTime,
    POSIXTimeRange,
    PubKeyHash,
    TokenName,
    Tx,
    TxInfo,
    TxOut (TxOut, txOutValue),
    TxOutRef,
    TxOutTx (txOutTxOut),
    ValidatorHash,
    contains,
    scriptHashAddress,
    to,
    txOutDatum,
  )
import Ledger.Typed.Scripts as Scripts (ValidatorTypes (..))
import Ledger.Value
  ( AssetClass,
    CurrencySymbol,
    TokenName (..),
    Value,
    assetClassValueOf,
    flattenValue, assetClass
  )
import Membership.PlatformSettings (ContractSettings)
import Membership.Service (Service (..))
import Membership.Signature (Sig, sigTokenToUser)
import Membership.Utils (tripleSnd)
import Plutus.Contract as Contract (Contract, utxoAt)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as M
import PlutusTx.Builtins (BuiltinByteString)
import PlutusTx.Prelude
  ( AdditiveSemigroup ((+)),
    Bool (..),
    Eq (..),
    Integer,
    Maybe (..),
    any,
    elem,
    filter,
    find,
    map,
    not,
    null,
    ($),
    (&&),
    (.),
    (<$>),
    (||),
    (/=)
  )
import qualified Prelude

-- A data type that represents what the user function is
data Role = Publisher | Client | Mediator
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq Role where
  {-# INLINEABLE (==) #-}
  Publisher == Publisher = True
  Client == Client = True
  Mediator == Mediator = True
  _ == _ = False

PlutusTx.unstableMakeIsData ''Role
PlutusTx.makeLift ''Role

-- A data type that contains the essential information needed for
-- a Judge to either accept or reject mediating a contract

-- The public key hash is the PubKeyHash from the judge that is being requested

-- The price is the amount of DSET tokens the judge will receive if he
-- accepts the job and sends a valid verdict before the deadline

-- The max duretion is how much time, after an accusation is made, a judge
-- has to send a verdict
data Judge = Judge
  { jPubKeyHash :: PubKeyHash,
    jPrice :: Integer,
    jMaxDuration :: POSIXTime
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq Judge where
  {-# INLINEABLE (==) #-}
  Judge pkh prc md == Judge pkh' prc' md' =
    pkh' == pkh && prc == prc' && md == md'

PlutusTx.unstableMakeIsData ''Judge

-- Judges is the same thing as Judge, but accepting more than one judge (or none)
data Judges = Judges
  { jsPubKeyHashes :: [PubKeyHash], -- The PubKeyHash of those responsible for mediating conflicts
    jsPrice :: Integer, -- The price each judge will receive if he accepts
    jsMaxDuration :: POSIXTime -- The maximum time a judge will have to make a decision
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq Judges where
  {-# INLINEABLE (==) #-}
  Judges pkhs prc md == Judges pkhs' prc' md' =
    pkhs' == pkhs && prc == prc' && md == md'

PlutusTx.unstableMakeIsData ''Judges

-- A data type containing the information concerning an accusation

-- Accuser is a tuple containing the public key hash of the
-- accuser as the first element and his role as the second

-- Accused is a tuple containing the public key hash of the
-- accused as the first element and his role as the second

-- Time is the moment this accusation was sent in POSIXTime
data Accusation = Accusation
  { aAccuser :: (PubKeyHash, Role),
    aAccused :: (PubKeyHash, Role),
    aTime :: POSIXTime -- The time this accusation was sent
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq Accusation where
  {-# INLINEABLE (==) #-}
  Accusation acr acd time == Accusation acr' acd' time' =
    acr' == acr && acd == acd' && time == time'

PlutusTx.unstableMakeIsData ''Accusation

-- Contract Datum contains all the data needed for a contract to be understood

-- Judge, along with the other information already explained, contains the list
-- of all judges that are being requested to mediate any conflicts that may occur
-- concerning this contract

-- Logic Script is a tuple containing the validator hash from the logic that will handle
-- the distribution of the collateral based on the judges inputs and the shame token
-- currency symbol used to identify this logic

-- Accusations is a list of all accusation that are happening right now

-- Service is the extra details concering this contract, such as title, description, etc

-- Role Map is a Map associating every user that signed this contract to his role

data ContractDatum = ContractDatum
  { cdJudges :: Judges,
    cdLogicScript :: ValidatorHash,
    cdAccusations :: [Accusation],
    cdService :: Service,
    cdRoleMap :: M.Map PubKeyHash Role
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq ContractDatum where
  {-# INLINEABLE (==) #-}
  (ContractDatum jds ls acc svc rm)
    == (ContractDatum jds' ls' acc' svc' rm') =
      jds == jds'
        && ls == ls'
        && acc == acc'
        && svc == svc'
        && rm == rm'

PlutusTx.unstableMakeIsData ''ContractDatum

data Review = Review
  { rScore :: Integer,
    rTokensAmount :: Integer,
    rDescription :: BuiltinByteString
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq Review where
  {-# INLINEABLE (==) #-}
  Review sc ta dsc == Review sc' ta' dsc' =
    sc == sc'
      && ta == ta'
      && dsc == dsc'

PlutusTx.unstableMakeIsData ''Review

data ContractRedeemer
  = CSign -- Someone want's to make part of this contract
  | CAccuse PubKeyHash PubKeyHash -- Some user allegadily broke the rules
  | CMediate
  | CCancel -- A user want's to leave this contract before the service being completed
  | CLeave -- A user want's to leave this contract after everything has been dealt with
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq ContractRedeemer where
  {-# INLINEABLE (==) #-}
  CSign == CSign = True
  CAccuse acr acd == CAccuse acr' acd' = acr == acr' && acd == acd'
  CCancel == CCancel = True
  CLeave == CLeave = True
  _ == _ = False

PlutusTx.unstableMakeIsData ''ContractRedeemer

-- A data type to indicate where something is present
data PresentWhere = PWInput | PWOutput | PWBoth
  deriving (Prelude.Show, Generic, Prelude.Eq)

PlutusTx.unstableMakeIsData ''PresentWhere

-- A bunch of variables that are constantly being used in on-chain
-- code and, therefore, were all united in this data type
data ContractEssentials = ContractEssentials
  { ceInfo :: TxInfo,
    ceOwnInput :: TxOut,
    ceOwnOutput :: TxOut,
    ceOutputDatum :: ContractDatum,
    ceSigSymbol :: CurrencySymbol,
    ceInputSigs :: [Sig],
    ceOutputSigs :: [Sig],
    ceOwnSig :: Sig,
    ceSigValue :: Value,
    cePresentWhere :: PresentWhere
  }
  deriving (Prelude.Show, Generic, Prelude.Eq)

PlutusTx.unstableMakeIsData ''ContractEssentials

-- A bunch of variables that are constantly being used in off-chain
-- code and, therefore, were all united in this data type
data ContractOffChainEssentials = ContractOffChainEssentials
  { coeContractReference :: TxOutRef,
    coeContractOutTx :: TxOutTx,
    coeContractTx :: Tx,
    coeContractOut :: TxOut,
    coeContractDatum :: ContractDatum,
    coeContractSettings :: ContractSettings,
    coeContractValidatorHash :: ValidatorHash
  }
  deriving (Prelude.Show, Prelude.Eq)

data ContractType

instance Scripts.ValidatorTypes ContractType where
  type DatumType ContractType = ContractDatum
  type RedeemerType ContractType = ContractRedeemer

-- For ease of use, a tuple that contains both a contract datum and value
type DigiContract = (ContractDatum, Value)

{-# INLINEABLE contractNFTName #-}
contractNFTName :: BuiltinByteString
contractNFTName = "contract-nft"

{-# INLINEABLE contractNFTTokenName #-}
contractNFTTokenName :: TokenName
contractNFTTokenName = TokenName contractNFTName

{-# INLINEABLE digiContract #-}
digiContract :: ContractDatum -> TxOut -> DigiContract
digiContract cd (TxOut _ v _) = (cd, v)

-- Given a user, a role and an old datum, add this user to
-- the role map and return a new datum
{-# INLINEABLE addUser #-}
addUser :: PubKeyHash -> Role -> ContractDatum -> ContractDatum
addUser pkh userRole oldContractDatum =
  ContractDatum
    { cdJudges = cdJudges oldContractDatum,
      cdLogicScript = cdLogicScript oldContractDatum,
      cdAccusations = cdAccusations oldContractDatum,
      cdService = cdService oldContractDatum,
      cdRoleMap = M.insert pkh userRole (cdRoleMap oldContractDatum)
    }

-- Given a user and an old datum, remove this user from
-- the role map and return the new datum
{-# INLINEABLE removeUser #-}
removeUser :: PubKeyHash -> ContractDatum -> ContractDatum
removeUser pkh oldContractDatum =
  ContractDatum
    { cdJudges = cdJudges oldContractDatum,
      cdLogicScript = cdLogicScript oldContractDatum,
      cdAccusations = cdAccusations oldContractDatum,
      cdService = cdService oldContractDatum,
      cdRoleMap = M.delete pkh (cdRoleMap oldContractDatum)
    }

-- Given two users (the accuser and accused), their roles, and the time
-- this function is being called, construct an accusation and add it to
-- the list of the old datum, returning the new one
{-# INLINABLE accuseUser #-}
accuseUser ::
  (PubKeyHash, Role) ->
  (PubKeyHash, Role) ->
  POSIXTime ->
  ContractDatum ->
  ContractDatum
accuseUser accuser accused currentTime oldContractDatum =
  ContractDatum
    { cdJudges = cdJudges oldContractDatum,
      cdLogicScript = cdLogicScript oldContractDatum,
      cdAccusations = Accusation accuser accused currentTime : cdAccusations oldContractDatum,
      cdService = cdService oldContractDatum,
      cdRoleMap = cdRoleMap oldContractDatum
    }

-- Given an accusation and an input contract datum, removes the accusation
-- from the accusations list and returns a new datum
{-# INLINABLE removeAccusation #-}
removeAccusation :: Accusation -> ContractDatum -> ContractDatum
removeAccusation acc oldContractDatum =
  ContractDatum
    { cdJudges = cdJudges oldContractDatum,
      cdLogicScript = cdLogicScript oldContractDatum,
      cdAccusations = newAccusations,
      cdService = cdService oldContractDatum,
      cdRoleMap = cdRoleMap oldContractDatum
    }
  where
    newAccusations :: [Accusation]
    newAccusations = filter (acc /=) (cdAccusations oldContractDatum)

-- Get's the judge that is supposed to mediate a conflict in case there's one
{-# INLINEABLE currentJudge #-}
currentJudge :: CurrencySymbol -> Accusation -> POSIXTimeRange -> DigiContract -> Maybe PubKeyHash
currentJudge cs (Accusation _ _ accTime) curTime (cd, val) = findJudge authenticatedJudges dln
  where
    judges :: [PubKeyHash]
    judges = jsPubKeyHashes $ cdJudges cd

    authenticatedJudges :: [PubKeyHash]
    authenticatedJudges = map (sigTokenToUser . tripleSnd) $ filter f (flattenValue val)
      where
        f :: (CurrencySymbol, TokenName, Integer) -> Bool
        f (cs', tn, _) = (cs' == cs) && (sigTokenToUser tn `elem` judges)

    dln :: POSIXTime
    dln = accTime + jsMaxDuration (cdJudges cd)

    findJudge :: [PubKeyHash] -> POSIXTime -> Maybe PubKeyHash
    findJudge [] _ = Nothing
    findJudge (x : xs) deadline =
      if to deadline `contains` curTime
        then Just x
        else findJudge xs (deadline + dln)

-- Returns wheter the given user is involved in any of the accusations given
involvedInAccusation :: PubKeyHash -> [Accusation] -> Bool
involvedInAccusation pkh =
  any
    (\Accusation {aAccuser = (p, _), aAccused = (p', _), aTime = _} -> p == pkh || p' == pkh)

-- Verifies if a contract datum is in the "initial state"
{-# INLINEABLE isInitial #-}
isInitial :: PubKeyHash -> ContractDatum -> Bool
isInitial pkh (ContractDatum (Judges jds _ _) _ acc _ rm) =
  not (null jds)
    && null acc
    && M.toList rm == [(pkh, Publisher)]

-- Get's the first UTxO sitting at a specific contract validator hash
{-# INLINEABLE findContract #-}
findContract :: AssetClass -> ValidatorHash -> Contract w s Text (Maybe (TxOutRef, TxOutTx))
findContract nft contrValHash = do
  utxos <- Map.filter f <$> utxoAt (scriptHashAddress contrValHash)
  return $ case Map.toList utxos of
    [(oref, o)] -> Just (oref, o)
    _ -> Nothing
  where
    f :: TxOutTx -> Bool
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) nft == 1

-- Given a transaction output and a function, tries to get a contract datum
{-# INLINEABLE findContractDatum #-}
findContractDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe ContractDatum
findContractDatum o f = do
  dh <- txOutDatum o
  Datum d <- f dh
  PlutusTx.fromBuiltinData d

{-# INLINABLE findContractNFT #-}
findContractNFT :: Value -> Maybe AssetClass
findContractNFT val = do
  (cs, tn, _) <- find (\(_, tn, amt) -> tn == contractNFTTokenName && amt == 1) (flattenValue val)
  return $ assetClass cs tn