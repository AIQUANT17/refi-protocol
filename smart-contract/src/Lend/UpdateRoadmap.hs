{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lend.UpdateRoadmap
  ( PlastiksDatum (..),
    PlastiksRedeemer (..),
    validate,
    wrapped,
    validator,
  )
where

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude (Show)

-------------------------------------------------
-- Datum and Redeemer
-------------------------------------------------

data PlastiksDatum = PlastiksDatum
  { preId :: BuiltinByteString,
    roadmapId :: BuiltinByteString,
    roadmapName :: BuiltinByteString,
    roadmapDescription :: BuiltinByteString,
    progress :: Integer,
    adminsPkh :: [PubKeyHash],
    prePkh :: PubKeyHash,
    preSkh :: PubKeyHash,
    totalPlasticCredits :: Integer,
    soldPlasticCredits :: Integer,
    totalPlasticTokens :: Integer,
    sentPlasticTokens :: Integer,
    totalPlastic :: Integer,
    recoverPlastic :: Integer,
    createdAt :: BuiltinByteString
  }
  deriving stock (Show)

PlutusTx.unstableMakeIsData ''PlastiksDatum

data PlastiksRedeemer
  = UpdateProgress Integer
  | Release
  | Archived
  | FundUSDM
  deriving stock (Show)

PlutusTx.unstableMakeIsData ''PlastiksRedeemer

-------------------------------------------------
-- Helper Functions
-------------------------------------------------

{-# INLINEABLE isSignedByAnyAdmin #-}
isSignedByAnyAdmin :: TxInfo -> [PubKeyHash] -> Bool
isSignedByAnyAdmin _ [] = False
isSignedByAnyAdmin info (pkh : rest) =
  txSignedBy info pkh || isSignedByAnyAdmin info rest

-------------------------------------------------
-- Validator Logic
-------------------------------------------------

{-# INLINEABLE validate #-}
validate :: PlastiksDatum -> PlastiksRedeemer -> ScriptContext -> Bool
validate datum redeemer ctx =
  case redeemer of
    UpdateProgress newProgress ->
      traceIfFalse "No admin signed tx" (isSignedByAnyAdmin info (adminsPkh datum))
        && traceIfFalse "Invalid progress update" (newProgress > progress datum && newProgress <= 10000) -- Scaled to 100.00%
    Release ->
      traceIfFalse "No admin signed tx" (isSignedByAnyAdmin info (adminsPkh datum))
        && traceIfFalse "Progress is not completed" (progress datum == 10000) -- Must reach 100.00%
    Archived ->
      traceIfFalse "No admin signed tx" (isSignedByAnyAdmin info (adminsPkh datum))
    FundUSDM ->
      traceIfFalse "No admin signed tx" (isSignedByAnyAdmin info (adminsPkh datum))
  where
    info = scriptContextTxInfo ctx

-------------------------------------------------
-- Wrap Validator
-------------------------------------------------

{-# INLINEABLE wrapped #-}
wrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapped d r c =
  check $
    validate
      (unsafeFromBuiltinData d)
      (unsafeFromBuiltinData r)
      (unsafeFromBuiltinData c)

validator :: Validator
validator = mkValidatorScript $$(compile [||wrapped||])