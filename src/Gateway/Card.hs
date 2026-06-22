{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gateway.Card
  ( CardGateway (..)
  , defaultCardGateway
  ) where

import qualified Data.Char    as C
import qualified Data.Text    as T
import           Gateway.Class
import           Gateway.Mock
import           Gateway.StateMachine
import           Gateway.Types

-- ---------------------------------------------------------------------------
-- Gateway data type
-- ---------------------------------------------------------------------------

data CardGateway = CardGateway
  { cardSuccessRate :: Double  -- auth success probability (fraud checks etc.)
  , cardAmountLimit :: Double  -- per-transaction cap
  }

defaultCardGateway :: CardGateway
defaultCardGateway = CardGateway
  { cardSuccessRate = 0.88
  , cardAmountLimit = 500000.0
  }

-- ---------------------------------------------------------------------------
-- PaymentGateway instance
-- ---------------------------------------------------------------------------

instance PaymentGateway CardGateway where

  gatewayName _ = "Card (Stripe-mock)"

  initiate CardGateway{..} req =
    case prMethod req of
      Card num expiry _ _ -> do
        case validateCard num expiry (unAmount (prAmount req)) cardAmountLimit of
          Just err -> return (Left err)
          Nothing  -> do
            pd <- mkDetails "CARD" req
            return $ Right (mkInitiated pd)
      _ -> return (Left UnsupportedMethod)

  authorize CardGateway{..} payment =
    simulateCall cardSuccessRate $ do
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

validateCard :: T.Text -> T.Text -> Double -> Double -> Maybe GatewayError
validateCard num expiry amount limit
  | amount <= 0          = Just InvalidAmount
  | amount > limit       = Just InvalidAmount
  | not (isValidPan num) = Just InvalidCredentials
  | isExpired expiry     = Just CardExpired
  | otherwise            = Nothing

isValidPan :: T.Text -> Bool
isValidPan t =
  let digits = T.filter C.isDigit t
  in T.length digits >= 15 && T.length digits <= 16 && luhn digits

-- | Luhn algorithm — catches most transcription errors in card numbers.
luhn :: T.Text -> Bool
luhn t =
  let ds       = map (\c -> fromEnum c - fromEnum '0') (T.unpack t)
      doubled  = zipWith step (reverse ds) [0 :: Int ..]
      step d i = if even i then d
                 else let x = d * 2 in if x > 9 then x - 9 else x
  in sum doubled `mod` 10 == 0

isExpired :: T.Text -> Bool
isExpired t =
  case T.splitOn "/" t of
    [_, yy]  -> (read (T.unpack yy) :: Int) < 24
    _        -> True
