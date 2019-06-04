module Spec.Crowdfunding(tests) where

import           Data.Foldable                                 (fold)
import           Test.Tasty

import qualified Ledger.Ada                                    as Ada
import qualified Wallet.Emulator                               as EM

import           Examples.Crowdfunding
import           Language.Plutus.Contract.Contract             as Con

import           Spec.HUnit

tests :: TestTree
tests = testGroup "crowdfunding" [
    checkPredicate "Expose 'contribute' and 'scheduleCollection' endpoints"
        (endpointAvailable "contribute" <> endpointAvailable "schedule collection")
        $ pure (fst (Con.drain crowdfunding))

    , checkPredicate "'contribute' endpoint submits a transaction" anyTx $
        let key = EM.walletPubKey w1
            contribution = Ada.adaValueOf 10
        in fold . fst <$> callEndpoint w1 "contribute" (key, contribution) crowdfunding
    ]


w1, w2 :: EM.Wallet
w1 = EM.Wallet 1
w2 = EM.Wallet 2
    