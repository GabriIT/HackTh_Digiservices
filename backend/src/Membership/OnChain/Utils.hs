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

module Membership.OnChain.Utils where

import Ledger
  ( ScriptContext,
    TxInInfo (txInInfoResolved),
    TxOut (txOutValue),
    findOwnInput,
    getContinuingOutputs,
  )
import Ledger.Value
  ( AssetClass,
    Value,
    assetClassValueOf,
  )
import PlutusTx.Prelude
  ( Bool,
    Integer,
    Maybe (Just, Nothing),
    Ord ((>)),
    fst,
    snd,
    traceError,
    ($),
    (&&),
  )

-- Find output and input from context as long as there is only one script output
{-# INLINEABLE strictFindOutAndIn #-}
strictFindOutAndIn :: ScriptContext -> (TxOut, TxOut)
strictFindOutAndIn ctx = (ownInput, ownOutput)
  where
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
      Nothing -> traceError "Input missing"
      Just i -> txInInfoResolved i

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
      [o] -> o
      _ -> traceError "Expected exactly one output"

-- Given the asset class corresponding to the SIG token we wanna search for
-- and a script context, determine wheter this SIG token is present or not
{-# INLINEABLE sigTokenInContext #-}
sigTokenInContext :: AssetClass -> ScriptContext -> Bool
sigTokenInContext sig ctx =
  sigTokenIn sig (txOutValue ownInput)
    && sigTokenIn sig (txOutValue ownOutput)
  where
    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

-- Given the asset class corresponding to the SIG token we wanna search for
-- and a value, determine wheter this SIG token is present or not
sigTokenIn :: AssetClass -> Value -> Bool
sigTokenIn sig val = tokens > 0
  where
    tokens :: Integer
    tokens = assetClassValueOf val sig