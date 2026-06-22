{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           Gateway

-- ---------------------------------------------------------------------------
-- Generic flow -- works for ANY PaymentGateway g without a single if/else
-- on the provider type.  This is the core point of the abstraction.
-- ---------------------------------------------------------------------------

runPaymentFlow :: PaymentGateway g => g -> PaymentRequest -> Bool -> IO ()
runPaymentFlow gw req doRefund = do
  putStrLn $ "\n-- " ++ gatewayName gw ++ " " ++ replicate (42 - length (gatewayName gw)) '-'
  TIO.putStrLn $ "   " <> fmtMethod (prMethod req)
              <> "  |  " <> fmtAmount (prAmount req) (prCurrency req)

  initRes <- initiate gw req
  case initRes of
    Left err -> putStrLn $ "   FAIL [1/4] Initiation: " ++ show err
    Right initiated -> do
      putStrLn $ "   OK   [1/4] Initiated   -> " ++ T.unpack (txnId (transactionId initiated))

      authRes <- authorize gw initiated
      case authRes of
        Left err -> putStrLn $ "   FAIL [2/4] Authorization: " ++ show err
        Right authorized -> do
          let (AuthorizedPayment _ (AuthCode ac)) = authorized
          putStrLn $ "   OK   [2/4] Authorized  -> " ++ T.unpack ac

          capRes <- capture gw authorized
          case capRes of
            Left err -> putStrLn $ "   FAIL [3/4] Capture: " ++ show err
            Right captured -> do
              let (CapturedPayment _ _ (SettlementRef sr)) = captured
              putStrLn $ "   OK   [3/4] Captured    -> " ++ T.unpack sr

              if doRefund
                then do
                  refRes <- refund gw captured
                  case refRes of
                    Left err -> putStrLn $ "   FAIL [4/4] Refund: " ++ show err
                    Right (RefundedPayment _ (RefundRef rr)) ->
                      putStrLn $ "   OK   [4/4] Refunded    -> " ++ T.unpack rr
                else putStrLn "   --   [4/4] Refund skipped"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

fmtMethod :: PaymentMethod -> T.Text
fmtMethod (UPI uid)             = uid
fmtMethod (Card num _ _ holder) = holder <> " **** " <> T.takeEnd 4 (T.filter (/= ' ') num)
fmtMethod (Wallet _ provider)   = T.pack (show provider)

fmtAmount :: Amount -> Currency -> T.Text
fmtAmount (Amount a) (Currency c) = c <> " " <> T.pack (show a)

-- ---------------------------------------------------------------------------
-- Sample requests
-- ---------------------------------------------------------------------------

upiRequest :: PaymentRequest
upiRequest = PaymentRequest
  { prMethod         = UPI "priya@phonepe"
  , prAmount         = Amount 4500.00
  , prCurrency       = Currency "INR"
  , prMerchantId     = "MERCH-001"
  , prDescription    = "Order #ORD-9921"
  , prIdempotencyKey = "idem-upi-001"
  }

cardRequest :: PaymentRequest
cardRequest = PaymentRequest
  { prMethod         = Card "4111111111111111" "12/26" "123" "Priya Dev"
  , prAmount         = Amount 1200.00
  , prCurrency       = Currency "USD"
  , prMerchantId     = "MERCH-001"
  , prDescription    = "SaaS subscription"
  , prIdempotencyKey = "idem-card-001"
  }

walletRequest :: PaymentRequest
walletRequest = PaymentRequest
  { prMethod         = Wallet "wallet-PAY-38472" PhonePe
  , prAmount         = Amount 850.00
  , prCurrency       = Currency "INR"
  , prMerchantId     = "MERCH-001"
  , prDescription    = "Recharge"
  , prIdempotencyKey = "idem-wallet-001"
  }

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "====================================================="
  putStrLn "  NexusPay -- Type-Safe Payment Gateway Demo"
  putStrLn "====================================================="
  putStrLn ""
  putStrLn "All three gateways run through the same runPaymentFlow."
  putStrLn "Zero branching on provider type -- type class handles dispatch."

  runPaymentFlow defaultUPIGateway              upiRequest    True
  runPaymentFlow defaultCardGateway             cardRequest   False
  runPaymentFlow (defaultWalletGateway PhonePe) walletRequest True

  putStrLn "\n-- Error scenarios -----------------------------------"

  runPaymentFlow defaultUPIGateway
    (upiRequest { prMethod = UPI "invalid-no-at" }) False

  runPaymentFlow (defaultWalletGateway Paytm)
    (walletRequest { prAmount = Amount 50000.0, prMethod = Wallet "w-1" Paytm }) False

  runPaymentFlow defaultUPIGateway cardRequest False

  putStrLn "\nDone."
