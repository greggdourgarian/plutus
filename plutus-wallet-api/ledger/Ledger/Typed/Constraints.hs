{-# LANGUAGE TypeOperators             #-}
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

addTypedTxIn'
  :: forall (ins :: [Type]) inn
  .  TypedScriptTxIn inn
  -> TypedOrPubKeyTxIns ins
  -> TypedOrPubKeyTxIns (inn ': ins)
addTypedTxIn' txi t =
  t { topTypedTxIns = HConsF txi (topTypedTxIns t) }

data TypedOrUntypedTxOuts (outs :: [Type]) =
  TypedOrUntypedTxOuts
    { totTypedTxOuts :: HListF TypedScriptTxOut outs
    , totOtherTxOuts :: [TxOut] -- pubkey outputs, and outputs whose types we don't care about
    }

type TypedTxConstraints (ins :: [Type]) (outs :: [Type]) =
  TxConstraints (TypedOrPubKeyTxIns ins) (TypedOrUntypedTxOuts outs)

-- | Turn an on-chain 'PendingTxConstraints' value into an (off-chain)
--   'TypedTxSomeOutsConstraints []' value with either zero or one
--   continuing outputs. Returns 'Nothing' if the self-payment is overspecified
--   (constraints can't be solved)
toTypedTxConstraints
  :: forall inn 
  .  (IsData (DataType inn))
  => ScriptInstance inn
  -> PendingTxConstraints (DataType inn)
  -> Maybe (Either (TypedTxConstraints '[] '[]) (TypedTxConstraints '[] '[inn]))
toTypedTxConstraints inst txc  = 
  let ValueAllocation{vaOtherPayments, vaOwnAddress} = tcOutputs txc
      txIns = TypedOrPubKeyTxIns { topTypedTxIns = HNilF, topPubKeyTxIns = [] }
      otherTxOuts = TypedOrUntypedTxOuts { totTypedTxOuts = HNilF, totOtherTxOuts = vaOtherPayments}
  in case vaOwnAddress of
    Unspecified ->
      Just $ Left $ txc { tcInputs = txIns, tcOutputs = otherTxOuts }
    Exactly PayToSelf{ptsData, ptsValue} ->
      let out = makeTypedScriptTxOut inst ptsData ptsValue
      in Just $ Right $ txc { tcInputs = txIns, tcOutputs = otherTxOuts { totTypedTxOuts = HConsF out HNilF }}
    Overspecified -> Nothing

addTypedTxIn
  :: forall (ins :: [Type]) (outs :: [Type]) inn
  .  TypedScriptTxIn inn
  -> TypedTxConstraints ins outs
  -> TypedTxConstraints (inn ': ins) outs
addTypedTxIn inn tc = 
  let oldIns = tcInputs tc
      newIns :: TypedOrPubKeyTxIns (inn ': ins)
      newIns = addTypedTxIn' inn oldIns
  in tc { tcInputs = newIns }

toUntypedLedgerConstraints
  :: forall inn out
  .  TypedTxConstraints inn out
  -> LedgerTxConstraints
toUntypedLedgerConstraints = undefined
