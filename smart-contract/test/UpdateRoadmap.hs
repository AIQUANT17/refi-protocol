{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS8
import qualified Lend.UpdateRoadmap as RM
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified PlutusTx.AssocMap as AssocMap
import qualified PlutusTx.Builtins as Builtins
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

-- Sample admin
admin1 :: PubKeyHash
admin1 = PubKeyHash (Builtins.toBuiltin (BS8.pack "admin"))

-- Sample Datum
baseDatum :: RM.PlastiksDatum
baseDatum =
  RM.PlastiksDatum
    { RM.preId = "pre001",
      RM.roadmapId = "rm001",
      RM.roadmapName = "Test Roadmap",
      RM.roadmapDescription = "Testing roadmap progress",
      RM.progress = 5000,
      RM.adminsPkh = [admin1],
      RM.prePkh = admin1,
      RM.preSkh = admin1,
      RM.totalPlasticCredits = 1000,
      RM.soldPlasticCredits = 200,
      RM.totalPlasticTokens = 10000,
      RM.sentPlasticTokens = 500,
      RM.totalPlastic = 3000,
      RM.recoverPlastic = 100,
      RM.createdAt = "2024-01-01"
    }

-- Mock context builder
mkCtx :: Bool -> ScriptContext
mkCtx signed =
  ScriptContext
    { scriptContextTxInfo =
        TxInfo
          { txInfoInputs = [],
            txInfoReferenceInputs = [],
            txInfoOutputs = [],
            txInfoFee = mempty,
            txInfoMint = mempty,
            txInfoDCert = [],
            txInfoWdrl = AssocMap.empty,
            txInfoValidRange = always,
            txInfoSignatories = if signed then [admin1] else [],
            txInfoRedeemers = AssocMap.empty,
            txInfoData = AssocMap.empty,
            txInfoId = TxId "tx0"
          },
      scriptContextPurpose = Spending (TxOutRef (TxId "tx0") 0)
    }

-- Wrap validator logic for testing
mockValidator :: RM.PlastiksDatum -> RM.PlastiksRedeemer -> ScriptContext -> Bool
mockValidator = RM.validate

-- Test cases
tests :: TestTree
tests =
  testGroup
    "RoadMapValidator Tests"
    [ testGroup
        "UpdateProgress"
        [ testCase "Valid progress update by admin" $
            assertBool "Should pass" $
              mockValidator baseDatum (RM.UpdateProgress 8000) (mkCtx True),
          testCase "Invalid progress update (not increasing)" $
            assertBool "Should fail" $
              not $ mockValidator baseDatum (RM.UpdateProgress 4000) (mkCtx True),
          testCase "Invalid progress update (no admin signature)" $
            assertBool "Should fail" $
              not $ mockValidator baseDatum (RM.UpdateProgress 8000) (mkCtx False)
        ],
      testGroup
        "Release"
        [ testCase "Valid release when progress is 100%" $
            let completedDatum = baseDatum {RM.progress = 10000}
             in assertBool "Should pass" $
                  mockValidator completedDatum RM.Release (mkCtx True),
          testCase "Invalid release when progress < 100%" $
            assertBool "Should fail" $
              not $ mockValidator baseDatum RM.Release (mkCtx True)
        ],
      testGroup
        "Archived"
        [ testCase "Valid archive by admin" $
            assertBool "Should pass" $
              mockValidator baseDatum RM.Archived (mkCtx True),
          testCase "Invalid archive without admin signature" $
            assertBool "Should fail" $
              not $ mockValidator baseDatum RM.Archived (mkCtx False)
        ],
      testGroup
        "FundUSDM"
        [ testCase "Valid fund USDM by admin" $
            assertBool "Should pass" $
              mockValidator baseDatum RM.FundUSDM (mkCtx True),
          testCase "Invalid fund USDM without admin signature" $
            assertBool "Should fail" $
              not $ mockValidator baseDatum RM.FundUSDM (mkCtx False)
        ]
    ]

main :: IO ()
main = defaultMain tests
