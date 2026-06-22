{-# LANGUAGE DataKinds #-}

-- | The single @PaymentGateway@ type class that all providers implement.
--
--   Key design point: __the method signatures ARE the state machine__.
--   @capture@ accepts @Payment \'Authorized@, not @Payment \'Initiated@.
--   Calling @capture@ on a freshly-initiated payment is a __type error__,
--   eliminating an entire class of runtime bugs without any guard clauses.

module Gateway.Class
  ( PaymentGateway (..)
  ) where

import Gateway.Types
import Gateway.StateMachine

class PaymentGateway g where

  -- | Human-readable gateway name for logging / display.
  gatewayName :: g -> String

  -- | Validate the request and register it with the gateway.
  --   Returns @Payment \'Initiated@ — the only legal starting state.
  initiate
    :: g
    -> PaymentRequest
    -> IO (Either GatewayError (Payment 'Initiated))

  -- | Run 3DS / OTP / PIN check.  Only valid on an initiated payment.
  authorize
    :: g
    -> Payment 'Initiated
    -> IO (Either GatewayError (Payment 'Authorized))

  -- | Move funds.  Only valid after authorization.
  --   Passing @Payment \'Initiated@ here is a compile error.
  capture
    :: g
    -> Payment 'Authorized
    -> IO (Either GatewayError (Payment 'Captured))

  -- | Return funds.  Only valid after capture.
  --   You cannot accidentally refund an authorized-but-not-captured payment.
  refund
    :: g
    -> Payment 'Captured
    -> IO (Either GatewayError (Payment 'Refunded))

  -- | Cancel an authorized-but-not-yet-captured payment.
  voidPayment
    :: g
    -> Payment 'Authorized
    -> IO (Either GatewayError (Payment 'Failed))

  -- | Status probe — returns current lifecycle position.
  queryStatus
    :: g
    -> TransactionId
    -> IO (Either GatewayError PaymentStatus)
