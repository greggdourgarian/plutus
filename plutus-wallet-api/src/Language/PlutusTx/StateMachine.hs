{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DataKinds        #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
-- | On-chain code fragments for creating a state machine. First
--   define a @StateMachine s i@ with input type @i@ and state type @s@. Then
--   use 'mkValidator' in on-chain code to check the required hashes and
--   validate the transition, and 'mkRedeemer' to make redeemer scripts.
module Language.PlutusTx.StateMachine(
      StateMachine(..)
    , StateMachineInstance (..)
    , machineAddress
    , mkValidator
    ) where

import           Language.PlutusTx.Prelude hiding (check)
import           Ledger.Constraints
import qualified Language.PlutusTx as PlutusTx

import           Ledger                    (Address, Value)
import           Ledger.Typed.Scripts
import           Ledger.Validation         (PendingTx, pendingTxInValue, pendingTxIn)

-- | Specification of a state machine, consisting of a transition function that determines the
-- next state from the current state and an input, and a checking function that checks the validity
-- of the transition in the context of the current transaction.
data StateMachine s i = StateMachine {
      -- | The transition function of the state machine. 'Nothing' indicates an invalid transition from the current state.
      smTransition :: s -> i -> Value -> Maybe (PendingTxConstraints s),
      -- | The condition checking function. Checks whether a given state transition is allowed given the 'PendingTx'.
      smCheck :: s -> i -> PendingTx -> Bool
    }

instance ScriptType (StateMachine s i) where
    type instance RedeemerType (StateMachine s i) = i
    type instance DataType (StateMachine s i) = s

data StateMachineInstance s i = StateMachineInstance {
    -- | The state machine specification.
    stateMachine :: StateMachine s i,
    -- | The validator code for this state machine.
    validatorInstance :: ScriptInstance (StateMachine s i)
    }

machineAddress :: StateMachineInstance s i -> Address
machineAddress = scriptAddress . validatorInstance

{-# INLINABLE mkValidator #-}
-- | Turn a transition function @s -> i -> Value -> Maybe (TxConstraints s)@ into a validator script.
mkValidator :: (Eq s, PlutusTx.IsData s) => StateMachine s i -> ValidatorType (StateMachine s i)
mkValidator (StateMachine step check) currentState input ptx =
    let vl = pendingTxInValue (pendingTxIn ptx)
        checkOk = traceIfFalseH "State transition invalid - checks failed" (check currentState input ptx)
        stateAndOutputsOk = case step currentState input vl of
            Just constraints ->
                traceIfFalseH "State transition invalid - constraints not satisfied by PendingTx" (checkPendingTx constraints ptx)
            Nothing -> traceH "State transition invalid - input is not a valid transition at the current state" False
    in checkOk && stateAndOutputsOk
