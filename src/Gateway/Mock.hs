{-# LANGUAGE OverloadedStrings #-}

module Gateway.Mock
  ( simulateCall
  , randomFailure
  , genTransactionId
  , genAuthCode
  , genSettlementRef
  , genRefundRef
  , mkDetails
  ) where

import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Time       (getCurrentTime)
import           System.Random   (randomRIO)
import           Gateway.Types

-- | Run an IO action and wrap in Right with probability @p@;
--   otherwise produce a random GatewayError as Left.
simulateCall :: Double -> IO a -> IO (Either GatewayError a)
simulateCall p action = do
  r <- randomRIO (0.0 :: Double, 1.0)
  if r < p
    then Right <$> action
    else Left  <$> randomFailure

randomFailure :: IO GatewayError
randomFailure = do
  i <- randomRIO (0 :: Int, 4)
  return $ errors !! i
  where
    errors =
      [ NetworkTimeout
      , GatewayDown
      , VelocityLimitExceeded
      , InsufficientFunds
      , DuplicateTransaction
      ]

-- ---------------------------------------------------------------------------
-- Reference generators
-- ---------------------------------------------------------------------------

genTransactionId :: Text -> IO TransactionId
genTransactionId prefix = do
  n <- randomRIO (100000 :: Int, 999999)
  return $ TransactionId $ prefix <> "-TXN-" <> T.pack (show n)

genAuthCode :: IO AuthCode
genAuthCode = do
  n <- randomRIO (10000000 :: Int, 99999999)
  return $ AuthCode $ "AUTH-" <> T.pack (show n)

genSettlementRef :: IO SettlementRef
genSettlementRef = do
  n <- randomRIO (1000000 :: Int, 9999999)
  return $ SettlementRef $ "SETTL-" <> T.pack (show n)

genRefundRef :: IO RefundRef
genRefundRef = do
  n <- randomRIO (1000000 :: Int, 9999999)
  return $ RefundRef $ "REFND-" <> T.pack (show n)

-- ---------------------------------------------------------------------------
-- Build a PaymentDetails record from a request + fresh txn ID
-- ---------------------------------------------------------------------------

mkDetails :: Text -> PaymentRequest -> IO PaymentDetails
mkDetails prefix req = do
  now  <- getCurrentTime
  txn  <- genTransactionId prefix
  return PaymentDetails
    { pdTransactionId = txn
    , pdMethod        = prMethod req
    , pdAmount        = prAmount req
    , pdCurrency      = prCurrency req
    , pdMerchantId    = prMerchantId req
    , pdCreatedAt     = now
    , pdDescription   = prDescription req
    }
