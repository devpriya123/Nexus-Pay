{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gateway.Wallet
  ( WalletGateway (..)
  , defaultWalletGateway
  ) where

import           Gateway.Class
import           Gateway.Mock
import           Gateway.StateMachine
import           Gateway.Types

-- ---------------------------------------------------------------------------
-- Gateway data type
-- ---------------------------------------------------------------------------

data WalletGateway = WalletGateway
  { walletGatewayProvider :: WalletProvider
  , walletSuccessRate     :: Double
  , walletAmountLimit     :: Double  -- wallets typically have lower caps
  }

defaultWalletGateway :: WalletProvider -> WalletGateway
defaultWalletGateway provider = WalletGateway
  { walletGatewayProvider = provider
  , walletSuccessRate     = 0.95
  , walletAmountLimit     = 20000.0
  }

-- ---------------------------------------------------------------------------
-- PaymentGateway instance
-- ---------------------------------------------------------------------------

instance PaymentGateway WalletGateway where

  gatewayName gw = "Wallet (" ++ show (walletGatewayProvider gw) ++ ")"

  initiate WalletGateway{..} req =
    case prMethod req of
      Wallet _ provider
        | provider /= walletGatewayProvider -> return (Left UnsupportedMethod)
        | unAmount (prAmount req) <= 0      -> return (Left InvalidAmount)
        | unAmount (prAmount req) > walletAmountLimit
                                            -> return (Left WalletBalanceLow)
        | otherwise -> do
            pd <- mkDetails "WLLT" req
            return $ Right (mkInitiated pd)
      _ -> return (Left UnsupportedMethod)

  authorize WalletGateway{..} payment =
    simulateCall walletSuccessRate $ do
      auth <- genAuthCode
      let pd = paymentDetails payment
      return (AuthorizedPayment pd auth)

  capture _ payment =
    simulateCall 0.99 $ do
      settl <- genSettlementRef
      let (AuthorizedPayment pd auth) = payment
      return (CapturedPayment pd auth settl)

  refund _ payment =
    simulateCall 0.99 $ do
      ref <- genRefundRef
      let pd = paymentDetails payment
      return (RefundedPayment pd ref)

  voidPayment _ (AuthorizedPayment pd _) =
    return $ Right (FailedPayment pd NetworkTimeout)

  queryStatus _ _ = return (Right StatusCaptured)
