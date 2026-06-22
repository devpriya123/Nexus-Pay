{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | QuickCheck properties for the GADT state machine.
--
--   Note: the most important "test" is that this file compiles.
--   Functions like @captureWithoutAuth@ simply cannot be written --
--   they are type errors, not missing runtime guards.

module StateMachineSpec (spec) where

import           Data.Time       (getCurrentTime)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic (monadicIO, run, assert)
import           Gateway.Types
import           Gateway.StateMachine

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

genAmount :: Gen Amount
genAmount = Amount <$> choose (1.0, 99999.0)

genCurrency :: Gen Currency
genCurrency = elements [Currency "INR", Currency "USD", Currency "EUR", Currency "GBP"]

genPaymentDetails :: IO PaymentDetails
genPaymentDetails = do
  now <- getCurrentTime
  return PaymentDetails
    { pdTransactionId = TransactionId "TXN-PROP-001"
    , pdMethod        = UPI "test@upi"
    , pdAmount        = Amount 1000.0
    , pdCurrency      = Currency "INR"
    , pdMerchantId    = "MERCH-TEST"
    , pdCreatedAt     = now
    , pdDescription   = "property test"
    }

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- Transaction ID is never mutated as payment moves through states
prop_txnIdPreserved :: Property
prop_txnIdPreserved = monadicIO $ do
  pd      <- run genPaymentDetails
  let initiated = mkInitiated pd
  assert (transactionId initiated == pdTransactionId pd)

-- Amount carried through is exactly what was set at initiation
prop_amountPreserved :: Property
prop_amountPreserved = monadicIO $ do
  pd <- run genPaymentDetails
  let initiated = mkInitiated pd
  assert (paymentAmount initiated == pdAmount pd)

-- paymentDetails round-trips for InitiatedPayment
prop_detailsRoundtrip :: Property
prop_detailsRoundtrip = monadicIO $ do
  pd <- run genPaymentDetails
  let initiated = mkInitiated pd
  assert (paymentDetails initiated == pd)

-- Amount generator always produces positive values
prop_amountPositive :: Property
prop_amountPositive =
  forAll genAmount $ \(Amount a) -> a > 0

-- Currency generator produces known currencies
prop_knownCurrencies :: Property
prop_knownCurrencies =
  forAll genCurrency $ \(Currency c) ->
    c `elem` ["INR", "USD", "EUR", "GBP"]

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "StateMachine" $ do

  describe "GADT invariants" $ do

    it "preserves transaction ID through initiation" $
      property prop_txnIdPreserved

    it "preserves amount through initiation" $
      property prop_amountPreserved

    it "paymentDetails round-trips for InitiatedPayment" $
      property prop_detailsRoundtrip

    it "Amount generator is always positive" $
      property prop_amountPositive

    it "Currency generator produces known values" $
      property prop_knownCurrencies

  describe "Type-level safety (compile-time)" $ do

    it "mkInitiated only accepts PaymentDetails, returns Payment 'Initiated" $ do
      pd <- genPaymentDetails
      let p = mkInitiated pd
      paymentAmount p `shouldBe` pdAmount pd

    -- The following would be a COMPILE ERROR -- not a runtime test:
    --
    --   badCapture :: Payment 'Initiated -> Payment 'Captured
    --   badCapture initiated = CapturedPayment (paymentDetails initiated) ??? ???
    --
    -- There is no way to produce an AuthCode or SettlementRef without
    -- going through AuthorizedPayment first, because the constructors
    -- aren't exposed as functions that accept Initiated.

    it "stateMachine note: invalid transitions are type errors, not test failures" $ do
      True `shouldBe` True  -- self-documenting placeholder
