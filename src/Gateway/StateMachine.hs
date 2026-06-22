{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE StandaloneDeriving   #-}

-- | GADT-encoded payment lifecycle state machine.
--
--   The phantom type parameter @(s :: PaymentState)@ means:
--
--   * @Payment \'Initiated@  — created, not yet authorised
--   * @Payment \'Authorized@ — gateway issued an auth code
--   * @Payment \'Captured@   — funds have been moved
--   * @Payment \'Failed@     — terminal failure
--   * @Payment \'Refunded@   — funds returned
--
--   Invalid transitions are __compile errors__, not runtime checks.
--   You cannot pass a @Payment \'Initiated@ to a function that expects
--   @Payment \'Authorized@; the types simply don't unify.

module Gateway.StateMachine
  ( PaymentState (..)
  , Payment (..)
  , mkInitiated
  , paymentDetails
  , transactionId
  , paymentAmount
  ) where

import Gateway.Types

-- ---------------------------------------------------------------------------
-- Promoted kind
-- ---------------------------------------------------------------------------

data PaymentState
  = Initiated
  | Authorized
  | Captured
  | Failed
  | Refunded

-- ---------------------------------------------------------------------------
-- The GADT
--
--   Each constructor captures exactly the evidence accumulated so far.
--   CapturedPayment requires an AuthCode — so it can only come from
--   AuthorizedPayment. There is no runtime check needed; the compiler
--   enforces it.
-- ---------------------------------------------------------------------------

data Payment (s :: PaymentState) where

  InitiatedPayment
    :: PaymentDetails
    -> Payment 'Initiated

  AuthorizedPayment
    :: PaymentDetails
    -> AuthCode          -- ^ issued by the gateway after 3DS / PIN / OTP
    -> Payment 'Authorized

  CapturedPayment
    :: PaymentDetails
    -> AuthCode
    -> SettlementRef     -- ^ acquirer settlement reference
    -> Payment 'Captured

  FailedPayment
    :: PaymentDetails
    -> GatewayError
    -> Payment 'Failed

  RefundedPayment
    :: PaymentDetails
    -> RefundRef
    -> Payment 'Refunded

deriving instance Show (Payment s)
deriving instance Eq   (Payment s)

-- ---------------------------------------------------------------------------
-- Constructor
-- ---------------------------------------------------------------------------

mkInitiated :: PaymentDetails -> Payment 'Initiated
mkInitiated = InitiatedPayment

-- ---------------------------------------------------------------------------
-- Extractors — polymorphic over all states
-- ---------------------------------------------------------------------------

paymentDetails :: Payment s -> PaymentDetails
paymentDetails (InitiatedPayment  pd)       = pd
paymentDetails (AuthorizedPayment pd _)     = pd
paymentDetails (CapturedPayment   pd _ _)   = pd
paymentDetails (FailedPayment     pd _)     = pd
paymentDetails (RefundedPayment   pd _)     = pd

transactionId :: Payment s -> TransactionId
transactionId = pdTransactionId . paymentDetails

paymentAmount :: Payment s -> Amount
paymentAmount = pdAmount . paymentDetails
