-- | Re-export facade — callers import only this module.
module Gateway
  ( -- * Type class
    module Gateway.Class
    -- * GADT state machine
  , module Gateway.StateMachine
    -- * Core types
  , module Gateway.Types
    -- * Concrete gateways
  , module Gateway.UPI
  , module Gateway.Card
  , module Gateway.Wallet
  ) where

import Gateway.Class
import Gateway.StateMachine
import Gateway.Types
import Gateway.UPI
import Gateway.Card
import Gateway.Wallet
