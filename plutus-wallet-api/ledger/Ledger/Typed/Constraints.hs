{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
module Ledger.Typed.Constraints where

import           Data.Kind

import           Ledger.Constraints     (LedgerTxConstraints, TxConstraints (..))
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

toTypedTxConstraints
  :: forall inn 
  .  (IsData (RedeemerType inn), IsData (DataType inn))
  => ScriptInstance inn
  -> RedeemerType inn
  -> TypedScriptTxIn inn
  -> LedgerTxConstraints
  -> TypedTxSomeOutsConstraints '[inn]
toTypedTxConstraints = undefined

-- mirror TypedTxSomeOuts

--
