{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
module Ledger.Typed.Constraints where

import           Data.Kind

import           Ledger.Constraints
import           Ledger.Specificity     (Specificity(..))
import           Ledger.Tx              (TxOut)
import           Ledger.Typed.Scripts   (ScriptType(..), ScriptInstance)
import           Ledger.Typed.Tx
import           Ledger.Typed.TypeUtils

import           Language.PlutusTx


data TypedOrPubKeyTxIns (ins :: [Type]) =
  TypedOrPubKeyTxIns
    { topTypedTxIns  :: HListF TypedScriptTxIn ins
    , topPubKeyTxIns :: [PubKeyTxIn] -- FIXME: Do we even need them
    }

data TypedOrUntypedTxOuts (outs :: [Type]) =
  TypedOrUntypedTxOuts
    { totTypedTxOuts :: HListF TypedScriptTxOut outs
    , totOtherTxOuts :: [TxOut] -- pubkey outputs, and outputs whose types we don't care about
    }

newtype TypedTxConstraints (ins :: [Type]) (outs :: [Type]) =
  TypedTxConstraints {
    unTypedTxConstraints :: TxConstraints (TypedOrPubKeyTxIns ins) (TypedOrUntypedTxOuts outs)
  }

data TypedTxSomeOutsConstraints (ins :: [Type]) =
  forall outs. TypedTxSomeOutsConstraints (TypedTxConstraints ins outs)

-- | Turn an on-chain 'PendingTxConstraints' value into an (off-chain)
--   'TypedTxSomeOutsConstraints []' value.
toTypedTxConstraints
  :: forall inn 
  .  (IsData (DataType inn))
  => ScriptInstance inn
  -> PendingTxConstraints (DataType inn)
  -> Maybe (TypedTxSomeOutsConstraints '[])
toTypedTxConstraints inst txc  = 
  let ValueAllocation{vaOtherPayments, vaOwnAddress} = tcOutputs txc
      txIns = TypedOrPubKeyTxIns { topTypedTxIns = HNilF, topPubKeyTxIns = [] }
      otherTxOuts = TypedOrUntypedTxOuts { totTypedTxOuts = HNilF, totOtherTxOuts = vaOtherPayments}
  in case vaOwnAddress of
    Unspecified ->
      Just $ TypedTxSomeOutsConstraints $ TypedTxConstraints $ txc { tcInputs = txIns, tcOutputs = otherTxOuts }
    Exactly PayToSelf{ptsData, ptsValue} ->
      let out = makeTypedScriptTxOut inst ptsData ptsValue
      in Just $ TypedTxSomeOutsConstraints $ TypedTxConstraints $ txc { tcInputs = txIns, tcOutputs = otherTxOuts { totTypedTxOuts = HConsF out HNilF }}
    Overspecified -> Nothing

-- mirror TypedTxSomeOuts

--
