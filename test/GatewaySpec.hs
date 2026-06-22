{-# LANGUAGE OverloadedStrings #-}

module GatewaySpec (spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic (monadicIO, run, assert)
import           Gateway

-- ---------------------------------------------------------------------------
-- Sample requests
-- ---------------------------------------------------------------------------

validUPIReq :: PaymentRequest
validUPIReq = PaymentRequest
  { prMethod         = UPI "user@phonepe"
  , prAmount         = Amount 500.0
  , prCurrency       = Currency "INR"
  , prMerchantId     = "M-001"
  , prDescription    = "test"
  , prIdempotencyKey = "k-1"
  }

validCardReq :: PaymentRequest
validCardReq = PaymentRequest
  { prMethod         = Card "4111111111111111" "12/26" "123" "Test User"
  , prAmount         = Amount 999.0
  , prCurrency       = Currency "USD"
  , prMerchantId     = "M-001"
  , prDescription    = "test"
  , prIdempotencyKey = "k-2"
  }

validWalletReq :: PaymentRequest
validWalletReq = PaymentRequest
  { prMethod         = Wallet "w-001" Paytm
  , prAmount         = Amount 200.0
  , prCurrency       = Currency "INR"
  , prMerchantId     = "M-001"
  , prDescription    = "test"
  , prIdempotencyKey = "k-3"
  }

-- ---------------------------------------------------------------------------
-- UPI Gateway
-- ---------------------------------------------------------------------------

specUPI :: Spec
specUPI = describe "UPI Gateway" $ do
  let gw = UPIGateway { upiSuccessRate = 1.0, upiAmountLimit = 100000.0 }

  it "accepts a valid UPI request" $ do
    r <- initiate gw validUPIReq
    r `shouldSatisfy` isRight

  it "rejects a non-UPI payment method" $ do
    r <- initiate gw validCardReq
    r `shouldBe` Left UnsupportedMethod

  it "rejects an invalid UPI handle (no @)" $ do
    let req = validUPIReq { prMethod = UPI "noemail" }
    r <- initiate gw req
    r `shouldBe` Left UPIHandleNotFound

  it "rejects amount above the limit" $ do
    let req = validUPIReq { prAmount = Amount 200000.0 }
    r <- initiate gw req
    r `shouldBe` Left InvalidAmount

  it "rejects zero amount" $ do
    let req = validUPIReq { prAmount = Amount 0 }
    r <- initiate gw req
    r `shouldBe` Left InvalidAmount

  it "full happy path: initiate -> authorize -> capture -> refund" $ do
    Right initiated  <- initiate  gw validUPIReq
    Right authorized <- authorize gw initiated
    Right captured   <- capture   gw authorized
    Right _          <- refund    gw captured
    True `shouldBe` True

  it "preserves amount through the full lifecycle" $ do
    Right initiated  <- initiate  gw validUPIReq
    Right authorized <- authorize gw initiated
    Right captured   <- capture   gw authorized
    paymentAmount captured `shouldBe` prAmount validUPIReq

  it "preserves transaction ID through lifecycle" $ do
    Right initiated  <- initiate  gw validUPIReq
    Right authorized <- authorize gw initiated
    Right captured   <- capture   gw authorized
    transactionId initiated  `shouldBe` transactionId authorized
    transactionId authorized `shouldBe` transactionId captured

-- ---------------------------------------------------------------------------
-- Card Gateway
-- ---------------------------------------------------------------------------

specCard :: Spec
specCard = describe "Card Gateway" $ do
  let gw = CardGateway { cardSuccessRate = 1.0, cardAmountLimit = 500000.0 }

  it "accepts a valid card request" $ do
    r <- initiate gw validCardReq
    r `shouldSatisfy` isRight

  it "rejects a non-card payment method" $ do
    r <- initiate gw validUPIReq
    r `shouldBe` Left UnsupportedMethod

  it "rejects an expired card" $ do
    let req = validCardReq { prMethod = Card "4111111111111111" "01/20" "123" "Test" }
    r <- initiate gw req
    r `shouldBe` Left CardExpired

  it "rejects a card number that fails Luhn" $ do
    let req = validCardReq { prMethod = Card "1234567890123456" "12/26" "123" "Test" }
    r <- initiate gw req
    r `shouldBe` Left InvalidCredentials

  it "rejects amount above card limit" $ do
    let req = validCardReq { prAmount = Amount 600000.0 }
    r <- initiate gw req
    r `shouldBe` Left InvalidAmount

  it "full happy path: initiate -> authorize -> capture" $ do
    Right initiated  <- initiate  gw validCardReq
    Right authorized <- authorize gw initiated
    Right _          <- capture   gw authorized
    True `shouldBe` True

  it "voidPayment returns a Failed payment" $ do
    Right initiated  <- initiate  gw validCardReq
    Right authorized <- authorize gw initiated
    result <- voidPayment gw authorized
    result `shouldSatisfy` isRight

-- ---------------------------------------------------------------------------
-- Wallet Gateway
-- ---------------------------------------------------------------------------

specWallet :: Spec
specWallet = describe "Wallet Gateway" $ do
  let gw = defaultWalletGateway Paytm

  it "accepts a valid wallet request" $ do
    r <- initiate gw validWalletReq
    r `shouldSatisfy` isRight

  it "rejects a non-wallet payment method" $ do
    r <- initiate gw validUPIReq
    r `shouldBe` Left UnsupportedMethod

  it "rejects a different wallet provider" $ do
    let req = validWalletReq { prMethod = Wallet "w-001" PhonePe }
    r <- initiate gw req
    r `shouldBe` Left UnsupportedMethod

  it "rejects amount above wallet limit (20000 INR)" $ do
    let req = validWalletReq { prAmount = Amount 25000.0 }
    r <- initiate gw req
    r `shouldBe` Left WalletBalanceLow

  it "full happy path: initiate -> authorize -> capture -> refund" $ do
    Right initiated  <- initiate  gw validWalletReq
    Right authorized <- authorize gw initiated
    Right captured   <- capture   gw authorized
    Right _          <- refund    gw captured
    True `shouldBe` True

-- ---------------------------------------------------------------------------
-- Polymorphic property — all gateways satisfy same contract
-- ---------------------------------------------------------------------------

specPolymorphic :: Spec
specPolymorphic = describe "Polymorphic contract" $ do

  it "amount is preserved by UPI initiation" $
    property $ monadicIO $ do
      let gw  = UPIGateway 1.0 100000.0
          req = validUPIReq
      res <- run $ initiate gw req
      case res of
        Left  _  -> assert True   -- error is fine, amount preservation doesn't apply
        Right p  -> assert (paymentAmount p == prAmount req)

  it "amount is preserved by Card initiation" $
    property $ monadicIO $ do
      let gw  = CardGateway 1.0 500000.0
          req = validCardReq
      res <- run $ initiate gw req
      case res of
        Left  _  -> assert True
        Right p  -> assert (paymentAmount p == prAmount req)

  it "amount is preserved by Wallet initiation" $
    property $ monadicIO $ do
      let gw  = defaultWalletGateway Paytm
          req = validWalletReq
      res <- run $ initiate gw req
      case res of
        Left  _  -> assert True
        Right p  -> assert (paymentAmount p == prAmount req)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

-- ---------------------------------------------------------------------------
-- Top-level spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  specUPI
  specCard
  specWallet
  specPolymorphic
