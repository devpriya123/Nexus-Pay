{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Gateway.Types
  ( -- * Payment methods
    PaymentMethod (..)
  , WalletProvider (..)
    -- * Monetary
  , Amount (..)
  , Currency (..)
    -- * Identifiers
  , TransactionId (..)
  , AuthCode (..)
  , SettlementRef (..)
  , RefundRef (..)
    -- * Core records
  , PaymentDetails (..)
  , PaymentRequest (..)
    -- * Errors & status
  , GatewayError (..)
  , PaymentStatus (..)
  ) where

import           Data.Text    (Text)
import           Data.Time    (UTCTime)
import           GHC.Generics (Generic)

-- ---------------------------------------------------------------------------
-- Payment method ADT — three distinct providers, no stringly-typed dispatch
-- ---------------------------------------------------------------------------

data PaymentMethod
  = UPI
      { upiId :: Text
      }
  | Card
      { cardNumber     :: Text
      , cardExpiry     :: Text   -- "MM/YY"
      , cardCvv        :: Text
      , cardholderName :: Text
      }
  | Wallet
      { walletId       :: Text
      , walletProvider :: WalletProvider
      }
  deriving (Show, Eq, Generic)

data WalletProvider = Paytm | PhonePe | AmazonPay
  deriving (Show, Eq, Ord, Generic)

-- ---------------------------------------------------------------------------
-- Monetary newtypes — prevent mixing amounts with other Doubles/Texts
-- ---------------------------------------------------------------------------

newtype Amount      = Amount      { unAmount   :: Double } deriving (Show, Eq, Ord, Generic)
newtype Currency    = Currency    { unCurrency :: Text   } deriving (Show, Eq,      Generic)

-- ---------------------------------------------------------------------------
-- Reference newtypes — distinct types for each lifecycle artifact
-- ---------------------------------------------------------------------------

newtype TransactionId = TransactionId { txnId     :: Text } deriving (Show, Eq, Generic)
newtype AuthCode      = AuthCode      { authCode  :: Text } deriving (Show, Eq, Generic)
newtype SettlementRef = SettlementRef { settlRef  :: Text } deriving (Show, Eq, Generic)
newtype RefundRef     = RefundRef     { refundRef :: Text } deriving (Show, Eq, Generic)

-- ---------------------------------------------------------------------------
-- Immutable snapshot created at initiation; carried through all states
-- ---------------------------------------------------------------------------

data PaymentDetails = PaymentDetails
  { pdTransactionId  :: TransactionId
  , pdMethod         :: PaymentMethod
  , pdAmount         :: Amount
  , pdCurrency       :: Currency
  , pdMerchantId     :: Text
  , pdCreatedAt      :: UTCTime
  , pdDescription    :: Text
  } deriving (Show, Eq, Generic)

-- ---------------------------------------------------------------------------
-- What the caller supplies to kick off a payment
-- ---------------------------------------------------------------------------

data PaymentRequest = PaymentRequest
  { prMethod         :: PaymentMethod
  , prAmount         :: Amount
  , prCurrency       :: Currency
  , prMerchantId     :: Text
  , prDescription    :: Text
  , prIdempotencyKey :: Text
  } deriving (Show, Eq, Generic)

-- ---------------------------------------------------------------------------
-- All reasons a gateway can reject or fail
-- ---------------------------------------------------------------------------

data GatewayError
  = InsufficientFunds
  | InvalidCredentials
  | NetworkTimeout
  | GatewayDown
  | InvalidAmount
  | DuplicateTransaction
  | VelocityLimitExceeded
  | CardExpired
  | UPIHandleNotFound
  | WalletBalanceLow
  | UnsupportedMethod
  deriving (Show, Eq, Generic)

-- ---------------------------------------------------------------------------
-- Summary type for status queries
-- ---------------------------------------------------------------------------

data PaymentStatus
  = StatusInitiated
  | StatusAuthorized
  | StatusCaptured
  | StatusFailed   GatewayError
  | StatusRefunded
  deriving (Show, Eq, Generic)
