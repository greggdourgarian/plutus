{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
-- | Functions for working with the contract interface using typed transactions.
module Language.Plutus.Contract.Typed.Tx where

import qualified Language.Plutus.Contract.Tx as Contract
import qualified Language.PlutusTx           as PlutusTx
import qualified Ledger.Constraints as Constraints

import           Ledger                      (TxOutRef, TxOutTx)
import qualified Ledger                      as L
import           Ledger.AddressMap           (AddressMap)
import qualified Ledger.Typed.Scripts        as Scripts
import qualified Ledger.Typed.Tx             as Typed
import           Ledger.Value                (Value)

import qualified Wallet.Typed.API            as Typed

-- | Given the pay to script address of the 'Validator', collect from it
--   all the outputs that match a predicate, using the 'RedeemerValue'.
collectFromScriptFilter ::
    forall a
    . (PlutusTx.IsData (Scripts.DataType a), PlutusTx.IsData (Scripts.RedeemerType a))
    => (TxOutRef -> TxOutTx -> Bool)
    -> AddressMap
    -> Scripts.ScriptInstance a
    -> Scripts.RedeemerType a
    -> Constraints.LedgerTxConstraints
collectFromScriptFilter flt am si red =
    let typed = Typed.collectFromScriptFilter flt am si red
        untypedTx :: L.Tx
        -- Need to match to get the existential type out
        untypedTx = case typed of
            (Typed.TypedTxSomeIns tx) -> Typed.toUntypedTx tx
    in Constraints.fromLedgerTx untypedTx

-- | A version of 'collectFromScript' that selects all outputs
--   at the address
collectFromScript ::
    forall a
    . (PlutusTx.IsData (Scripts.DataType a), PlutusTx.IsData (Scripts.RedeemerType a))
    => AddressMap
    -> Scripts.ScriptInstance a
    -> Scripts.RedeemerType a
    -> Constraints.LedgerTxConstraints
collectFromScript = collectFromScriptFilter (\_ _ -> True)

-- | Given a 'ScriptInstance', lock a value with it using the 'DataValue'.
makeScriptPayment ::
    forall a
    . (PlutusTx.IsData (Scripts.DataType a))
    => Scripts.ScriptInstance a
    -> Value
    -> Scripts.DataType a
    -> Constraints.LedgerTxConstraints
makeScriptPayment si vl ds =
    let out    = Typed.makeTypedScriptTxOut @a si ds vl
        tyTx   = Typed.addTypedTxOut @'[] @'[] @a out Typed.baseTx
    in Constraints.fromLedgerTx (Typed.toUntypedTx tyTx)
