{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gateway.UPI
  ( UPIGateway (..)
  , defaultUPIGateway
  ) where

import qualified Data.Text    as T
import           Gateway.Class
import           Gateway.Mock
import           Gateway.StateMachine
import           Gateway.Types

-- ---------------------------------------------------------------------------
-- Gateway data type
-- ---------------------------------------------------------------------------

data UPIGateway = UPIGateway
  { upiSuccessRate :: Double  -- authorization success probability
  , upiAmountLimit :: Double  -- per-transaction cap (INR)
  }

defaultUPIGateway :: UPIGateway
defaultUPIGateway = UPIGateway
  { upiSuccessRate = 0.92
  , upiAmountLimit = 100000.0
  }

-- ---------------------------------------------------------------------------
-- PaymentGateway instance
-- ---------------------------------------------------------------------------

instance PaymentGateway UPIGateway where

  gatewayName _ = "NPCI UPI"

  initiate UPIGateway{..} req =
    case prMethod req of
      UPI uid
        | not (isValidUpiId uid)              -> return (Left UPIHandleNotFound)
        | unAmount (prAmount req) <= 0        -> return (Left InvalidAmount)
        | unAmount (prAmount req) > upiAmountLimit
                                              -> return (Left InvalidAmount)
        | otherwise -> do
            pd <- mkDetails "UPI" req
            return $ Right (mkInitiated pd)
      _ -> return (Left UnsupportedMethod)

  authorize UPIGateway{..} payment =
    simulateCall upiSuccessRate $ do
      auth <- genAuthCode
      let pd = paymentDetails payment
      return (AuthorizedPayment pd auth)

  capture _ payment =
    simulateCall 0.99 $ do
      settl <- genSettlementRef
      let (AuthorizedPayment pd auth) = payment
      return (CapturedPayment pd auth settl)

  refund _ payment =
    simulateCall 0.97 $ do
      ref <- genRefundRef
      let pd = paymentDetails payment
      return (RefundedPayment pd ref)

  voidPayment _ (AuthorizedPayment pd _) =
    return $ Right (FailedPayment pd NetworkTimeout)

  queryStatus _ _ = return (Right StatusCaptured)

-- ---------------------------------------------------------------------------
-- Validation
-- ---------------------------------------------------------------------------

isValidUpiId :: T.Text -> Bool
isValidUpiId uid = "@" `T.isInfixOf` uid && T.length uid >= 5
