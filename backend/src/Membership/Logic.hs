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

module Membership.Logic where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map as HaskellMap (filter, toList)
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger
  ( Datum (Datum),
    DatumHash,
    PubKeyHash,
    Tx,
    TxOut,
    TxOutRef,
    TxOutTx,
    ValidatorHash,
    scriptHashAddress,
    txOutDatum,
    txOutTxOut,
    txOutValue,
  )
import Ledger.Value (AssetClass, CurrencySymbol, Value, assetClass, assetClassValueOf, getValue)
import Membership.Contract ( Accusation, Judge, Role )
import Membership.PlatformSettings (PlatformSettings)
import Membership.Signature (Sig, sigIn)
import Plutus.Contract as Contract (Contract, utxoAt)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AM
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Prelude
  ( AdditiveSemigroup ((+)),
    Bool (..),
    BuiltinByteString,
    Eq (..),
    Integer,
    Maybe (..),
    MultiplicativeSemigroup ((*)),
    all,
    any,
    elem,
    find,
    isJust,
    length,
    return,
    traceError,
    ($),
    (&&),
    (<$>),
    (/=)
  )
import qualified PlutusTx.Ratio as R
import qualified Prelude

-- Input is basically a question and it's expected answer. It's a way to connect
-- the real world, which is messy and subjective, with the blockchain, which should
-- be deterministic and objective

-- Roles is a list of the roles that this input affects. This is done because some
-- questions do not apply to every role. For example, "Did the accused completed the
-- service?" is a question that should only affect the publisher, since he is the one
-- who should complete the service.

-- Question is the string it self

-- Expected answer is what this question should be (true or false) in order for the
-- accused to be innocent. If the verdict answer is different from this, the logic
-- considers the accused guilty and can trigger the proportion defined

data Input = Input
  { iRoles :: [Role],
    iQuestion :: BuiltinByteString,
    iExpectedAnswer :: Bool,
    iGuiltEnabled :: Bool
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.unstableMakeIsData ''Input
PlutusTx.makeLift ''Input

instance Eq Input where
  {-# INLINEABLE (==) #-}
  Input r q a g == Input r' q' a' g' =
    r == r'
      && q == q'
      && a == a'
      && g == g'

-- Conditions are input requirements. For example,
--    CAny [CCondition "Was the book well received?",
--          CCondition "Did the book have more than 200 pages?"]
--    would mean that if any of these conditions are not met
--    (the verdict doesn't correspond to the expected input),
--    the accused would be declared guilty

-- There should only be conditions included in the "inputs" map
data Conditions
  = CAny [Conditions] -- Trigger proportion if any of the conditions fail
  | CAll [Conditions] -- Trigger proportion if all conditions fail
  | CCondition Input -- Trigger proportion if this condition fail
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq Conditions where
  {-# INLINEABLE (==) #-}
  CCondition inp == CCondition inp' = inp == inp'
  CAll cds == CAll cds' = all (`elem` cds') cds && length cds == length cds'
  CAny cds == CAny cds' = all (`elem` cds') cds && length cds == length cds'
  CAny [cd] == CAll [cd'] = cd == cd'
  CAny [] == CAll [] = True
  _ == _ = False

PlutusTx.unstableMakeIsData ''Conditions
PlutusTx.makeLift ''Conditions

type AccuserAmt = Integer

type AccusedAmt = Integer

-- ! TODO: Cannot divide by zero

-- 1 : 1 would mean only half would be taken from the accused's trust collateral amount
data Proportion = Proportion AccuserAmt AccusedAmt
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.unstableMakeIsData ''Proportion
PlutusTx.makeLift ''Proportion

-- Conditions and the poportion, by which the trust token will be
-- distributed, if these conditions are not met
type Logic = Map.Map Conditions Proportion

-- Should contain a map corresponding every input to it's actual answer 
type Verdict = Map.Map Input Bool

-- The settings essential in order for a logic script to work
data LogicSettings = LogicSettings
  { lsPlatformSettings :: PlatformSettings,
    lsSignatureSymbol :: CurrencySymbol,
    lsContract :: ValidatorHash,
    lsLogic :: Logic
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''LogicSettings

-- The datum that will be stored in the logic script

-- Waiting Start means that no one has been accused yet

-- Waiting Verdict means that someone has been accused, but the judge
-- still needs to send his verdict

-- Waiting End means that the verdict was already sent and the involved
-- parties can now distribute their tokens according to the logic executed

data LogicState = LSWaitingStart
                | LSWaitingVerdict AssetClass Judge Accusation
                | LSWaitingEnd AssetClass Judge Accusation Verdict
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq LogicState where
  {-# INLINEABLE (==) #-}
  LSWaitingStart == LSWaitingStart = True
  (LSWaitingVerdict ac j acc) == (LSWaitingVerdict ac' j' acc') =
    ac == ac' && j == j' && acc == acc'
  (LSWaitingEnd ac j a v) == (LSWaitingEnd ac' j' a' v') =
    ac == ac' && j == j' && a == a' && v == v'
  _ == _ = False

PlutusTx.unstableMakeIsData ''LogicState

data LogicRedeemer
  = LRAccuse Accusation
  | LRMediate Verdict
  | LRConsume
  | LRContest
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.unstableMakeIsData ''LogicRedeemer

-- The essential information off-chain code will need about logic
data LogicOffChainEssentials = LogicOffChainEssentials
  { loeLogicReference :: TxOutRef,
    loeLogicOutTx :: TxOutTx,
    loeLogicTx :: Tx,
    loeLogicOut :: TxOut,
    loeLogicDatum :: LogicState,
    loeLogicValidatorHash :: ValidatorHash
  }
  deriving (Prelude.Show, Prelude.Eq)

-- Given a proportion and the trust amount, returns a tuple indicating
-- the amount of tokens from this trust that should be given to the
-- accuser and accused
{-# INLINEABLE trustProportion #-}
trustProportion :: Proportion -> Integer -> (AccuserAmt, AccusedAmt)
trustProportion (Proportion acr acd) t = (accuserAmt, accusedAmt)
  where
    rTrust :: R.Rational
    rTrust = R.fromInteger t

    total :: Integer
    total = acr + acd

    accuserAmt :: Integer
    accuserAmt = R.round $ rTrust * (acr R.% total)

    accusedAmt :: Integer
    accusedAmt = R.round $ rTrust * (acd R.% total)

-- Verifies if a certain input can be found on the verdict.
-- A valid verdict should contain all inputs
{-# INLINEABLE possibleCondition #-}
possibleCondition :: Input -> Verdict -> Bool
possibleCondition inp ver = isJust $ Map.lookup inp ver

-- Given an input and verdict, verifies if the expected answer from
-- this input is different from the actual answer (which can be found
-- in the verdict)
{-# INLINEABLE conditionFailed #-}
conditionFailed :: Input -> Verdict -> Bool
conditionFailed inp ver = case Map.lookup inp ver of
  Just ans -> iExpectedAnswer inp /= ans
  Nothing -> traceError "Logic - Input not found inside verdict"

-- Given an input and verdict, verifies if the expected answer from
-- this input is different from the actual answer and if this specific
-- input has guilt enabled
{-# INLINEABLE conditionWithGuilt #-}
conditionWithGuilt :: Input -> Verdict -> Bool
conditionWithGuilt inp ver = case Map.lookup inp ver of
  Just ans -> iGuiltEnabled inp && iExpectedAnswer inp == ans
  Nothing -> traceError "Logic - Input not found inside verdict"

-- Given a group of conditions and a verdict, verify if the verdict
-- contains all answers
{-# INLINEABLE possibleConditions #-}
possibleConditions :: Conditions -> Verdict -> Bool
possibleConditions (CCondition inp) ver = possibleCondition inp ver
possibleConditions (CAny cs) ver = all (`possibleConditions` ver) cs
possibleConditions (CAll cs) ver = all (`possibleConditions` ver) cs

-- Verifies if the conditions logic failed

-- If Conditions type is CCondition, it will have failed
-- if the user role is present in the input and the input
-- expected answer is different from the actual answer given
-- by the judge in the verdict

-- If Conditions type is CAny, they will have failed
-- if any of the conditions in the type's list fails

-- If Conditions type is CAll, they will have failed
-- if all of the conditions in the type's list failed
{-# INLINEABLE conditionsFailed #-}
conditionsFailed :: Role -> Conditions -> Verdict -> Bool
conditionsFailed r (CCondition inp) ver = if r `elem` iRoles inp
                                          then conditionFailed inp ver
                                          else False
conditionsFailed r (CAny cs) ver = any (\cs' -> conditionsFailed r cs' ver) cs
conditionsFailed r (CAll cs) ver = all (\cs' -> conditionsFailed r cs' ver) cs

-- Verifies if the conditions logic failed and if what failed had
-- guilt enabled
{-# INLINEABLE conditionsWithGuilt #-}
conditionsWithGuilt :: Role -> Conditions -> Verdict -> Bool
conditionsWithGuilt r (CCondition inp) ver = if r `elem` iRoles inp
                                             then conditionWithGuilt inp ver
                                             else False
conditionsWithGuilt r (CAny cs) ver = any (\cs' -> conditionsWithGuilt r cs' ver) cs
conditionsWithGuilt r (CAll cs) ver = all (\cs' -> conditionsWithGuilt r cs' ver) cs

-- Given a logic and a verdict, makes sure all condions in that
-- logic have been answered by the verdict
{-# INLINABLE validVerdict #-}
validVerdict :: Logic -> Verdict -> Bool
validVerdict log ver = all (`possibleConditions` ver) (Map.keys log)

-- Returns the proportion of the collateral distribution
-- based on the accused user role, the mediator verdict and
-- the contract logic only if one of the logic conditions failed
{-# INLINEABLE failedProportion #-}
failedProportion :: Role -> Verdict -> Logic -> Maybe Proportion
failedProportion r ver log = do
  cds <- find (\cs -> conditionsFailed r cs ver) (Map.keys log)
  Map.lookup cds log

{-# INLINEABLE isGuilty #-}
isGuilty :: Role -> Logic -> Verdict -> Bool
isGuilty r log ver = any (\cs -> conditionsWithGuilt r cs ver) (Map.keys log)

-- Returns the proportion of the collateral distribution
-- based on the accused user role, the mediator verdict and
-- the contract logic even if none of the logic conditions failed
{-# INLINEABLE resultingProportion #-}
resultingProportion :: Role -> Logic -> Verdict -> Proportion
resultingProportion r log ver = case find (\(cs, _) -> conditionsWithGuilt r cs ver) (Map.toList log) of
  Just (_, p) -> p
  Nothing -> Proportion 0 1

-- Given a list of judges and a list of sigs, returns a sig
-- if one of the judges is in that list
{-# INLINEABLE firstValidJudge #-}
firstValidJudge :: [PubKeyHash] -> [Sig] -> Maybe PubKeyHash
firstValidJudge judges sigs = find (`sigIn` sigs) judges

-- Searches for the shame token asset class on a value, given it's
-- currency symbol
{-# INLINEABLE findShameTokenAssetClass #-}
findShameTokenAssetClass :: CurrencySymbol -> Value -> Maybe AssetClass
findShameTokenAssetClass cs v = do
  tokens <- AM.lookup cs (getValue v)
  case AM.toList tokens of
    [(tn, amt)]
      | amt == 1 -> Just $ assetClass cs tn
    _ -> Nothing

-- Get's the first UTxO sitting at a specific logic validator hash
{-# INLINEABLE findLogic #-}
findLogic :: AssetClass -> ValidatorHash -> Contract w s Text (Maybe (TxOutRef, TxOutTx))
findLogic shameToken logValHash = do
  utxos <- HaskellMap.filter f <$> utxoAt (scriptHashAddress logValHash)
  return $ case HaskellMap.toList utxos of
    [(oref, o)] -> Just (oref, o)
    _ -> Nothing
  where
    f :: TxOutTx -> Bool
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) shameToken == 1

{-# INLINEABLE findLogicDatum #-}
findLogicDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe LogicState
findLogicDatum o f = do
  dh <- txOutDatum o
  Datum d <- f dh
  PlutusTx.fromBuiltinData d