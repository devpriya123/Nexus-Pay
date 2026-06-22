module Main (main) where

import           Test.Hspec
import qualified GatewaySpec
import qualified StateMachineSpec

main :: IO ()
main = hspec $ do
  GatewaySpec.spec
  StateMachineSpec.spec
