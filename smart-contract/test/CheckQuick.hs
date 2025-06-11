{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS8
import qualified Lend.StakeReward as SR
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified PlutusTx.AssocMap as AssocMap
import qualified PlutusTx.Builtins as Builtins
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck as QC
import Prelude

-- Admin and lender identities
admin1, lender1 :: PubKeyHash
admin1 = PubKeyHash (Builtins.toBuiltin (BS8.pack "admin"))
lender1 = PubKeyHash (Builtins.toBuiltin (BS8.pack "lender"))

-- Arbitrary instances
instance Arbitrary SR.LenderAction where
  arbitrary =
    oneof
      [ pure SR.Deposit,
        SR.Withdraw <$> arbitraryPositiveInt,
        pure SR.Redeem,
        SR.FundPlastikToEscrow <$> arbitraryPositiveInt,
        SR.FundUSDM <$> arbitraryPositiveInt
      ]

arbitraryPositiveInt :: Gen Integer
arbitraryPositiveInt = choose (1, 10000)

instance Arbitrary SR.LenderDatum where
  arbitrary = do
    totalPT <- choose (1000, 10000)
    totalReward <- choose (500, 5000)
    lenderPT <- choose (1, totalPT)
    lenderReward <- choose (1, totalReward)
    return $
      SR.LenderDatum
        { SR.adminsPkh = [admin1],
          SR.totalPT = totalPT,
          SR.totalReward = totalReward,
          SR.lenders = [(lender1, (lenderPT, lenderReward))]
        }

-- Utilities
mkCtx :: Bool -> [TxOut] -> ScriptContext
mkCtx signed continuingOutputs =
  ScriptContext
    { scriptContextTxInfo =
        TxInfo
          { txInfoInputs = [],
            txInfoReferenceInputs = [],
            txInfoOutputs = continuingOutputs,
            txInfoFee = mempty,
            txInfoMint = mempty,
            txInfoDCert = [],
            txInfoWdrl = AssocMap.empty,
            txInfoValidRange = always,
            txInfoSignatories = if signed then [admin1] else [],
            txInfoRedeemers = AssocMap.empty,
            txInfoData = AssocMap.empty,
            txInfoId = TxId "dummy"
          },
      scriptContextPurpose = Spending (TxOutRef (TxId "dummy") 0)
    }

toOutputDatum :: SR.LenderDatum -> OutputDatum
toOutputDatum = OutputDatum . Datum . toBuiltinData

mkContinuingOutput :: SR.LenderDatum -> TxOut
mkContinuingOutput dat =
  TxOut
    { txOutAddress = Address (ScriptCredential "dummy") Nothing,
      txOutValue = mempty,
      txOutDatum = toOutputDatum dat,
      txOutReferenceScript = Nothing
    }

mockValidator :: SR.LenderDatum -> SR.LenderAction -> ScriptContext -> Bool
mockValidator oldDatum action ctx =
  case action of
    SR.Deposit ->
      let newDatum = extractDatumFromCtx ctx
       in SR.adminsPkh newDatum == SR.adminsPkh oldDatum
    SR.Withdraw amt ->
      amt > 0 && amt <= getLenderPT lender1 oldDatum
    SR.Redeem ->
      let outs = txInfoOutputs (scriptContextTxInfo ctx)
       in case outs of
            [] -> True
            [TxOut {txOutDatum = OutputDatum (Datum d)}] ->
              case fromBuiltinData d of
                Just newDatum -> SR.totalReward newDatum <= SR.totalReward oldDatum
                _ -> False
            _ -> False
    SR.FundPlastikToEscrow _ ->
      admin1 `elem` txInfoSignatories (scriptContextTxInfo ctx)
    SR.FundUSDM _ ->
      admin1 `elem` txInfoSignatories (scriptContextTxInfo ctx)
  where
    extractDatumFromCtx :: ScriptContext -> SR.LenderDatum
    extractDatumFromCtx sc =
      let outs = txInfoOutputs (scriptContextTxInfo sc)
       in case outs of
            [TxOut {txOutDatum = OutputDatum (Datum d)}] ->
              case fromBuiltinData d of
                Just dt -> dt
                _ -> oldDatum
            _ -> oldDatum

    getLenderPT :: PubKeyHash -> SR.LenderDatum -> Integer
    getLenderPT pkh datum =
      case lookup pkh (SR.lenders datum) of
        Just (pt, _) -> pt
        Nothing -> 0

-- Arbitrary property tests
prop_arbitrary_deposit_valid :: SR.LenderDatum -> Property
prop_arbitrary_deposit_valid oldDatum =
  forAll (pure oldDatum) $ \_ ->
    let ctx = mkCtx True [mkContinuingOutput oldDatum]
     in mockValidator oldDatum SR.Deposit ctx === True

prop_arbitrary_withdraw_valid :: SR.LenderDatum -> Property
prop_arbitrary_withdraw_valid datum@(SR.LenderDatum _ _ _ lenders) =
  case lookup lender1 lenders of
    Just (pt, _) ->
      pt > 0
        ==> forAll (choose (1, pt))
        $ \amt ->
          let ctx = mkCtx True [mkContinuingOutput datum]
           in mockValidator datum (SR.Withdraw amt) ctx === True
    Nothing -> property True

prop_arbitrary_redeem_valid :: SR.LenderDatum -> Property
prop_arbitrary_redeem_valid oldDatum =
  forAll (choose (0, SR.totalReward oldDatum)) $ \newReward ->
    let newDatum = oldDatum {SR.totalReward = newReward}
        ctx = mkCtx True [mkContinuingOutput newDatum]
     in mockValidator oldDatum SR.Redeem ctx === True

prop_arbitrary_fundPlastik_valid :: SR.LenderDatum -> Property
prop_arbitrary_fundPlastik_valid datum =
  forAll arbitraryPositiveInt $ \amt ->
    let ctx = mkCtx True [mkContinuingOutput datum]
     in mockValidator datum (SR.FundPlastikToEscrow amt) ctx === True

prop_arbitrary_fundUSDM_valid :: SR.LenderDatum -> Property
prop_arbitrary_fundUSDM_valid datum =
  forAll arbitraryPositiveInt $ \amt ->
    let ctx = mkCtx True [mkContinuingOutput datum]
     in mockValidator datum (SR.FundUSDM amt) ctx === True

-- Test group
main :: IO ()
main =
  defaultMain $
    testGroup
      "StakeReward Arbitrary Property Tests"
      [ testProperty "Valid arbitrary Deposit" $ QC.withMaxSuccess 200 prop_arbitrary_deposit_valid,
        testProperty "Valid arbitrary Withdraw" $ QC.withMaxSuccess 200 prop_arbitrary_withdraw_valid,
        testProperty "Valid arbitrary Redeem" $ QC.withMaxSuccess 200 prop_arbitrary_redeem_valid,
        testProperty "Valid arbitrary FundPlastikToEscrow" $ QC.withMaxSuccess 200 prop_arbitrary_fundPlastik_valid,
        testProperty "Valid arbitrary FundUSDM" $ QC.withMaxSuccess 200 prop_arbitrary_fundUSDM_valid
      ]
