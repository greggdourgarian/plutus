{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
-- needed for makeClassyPrisms
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Wallet.Typed.StateMachine where

import           Control.Lens
import           Data.Either                    (rights)
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromMaybe)

import qualified Language.PlutusTx              as PlutusTx
import qualified Language.PlutusTx.StateMachine as SM
import           Ledger.AddressMap              (AddressMap)
import qualified Ledger.Typed.Scripts           as Scripts
import qualified Ledger.Typed.Tx                as Typed

data SMError s i = InvalidTransition s i
    deriving Show
makeClassyPrisms ''SMError

type OnChainState s i = (Typed.TypedScriptTxOut (SM.StateMachine s i), Typed.TypedScriptTxOutRef (SM.StateMachine s i))
type SteppingTx s i = Typed.TypedTx '[SM.StateMachine s i] '[SM.StateMachine s i]
type HaltingTx s i = Typed.TypedTx '[SM.StateMachine s i] '[]

getStates
    :: forall s i
    . (PlutusTx.IsData s)
    => SM.StateMachineInstance s i
    -> AddressMap
    -> [OnChainState s i]
getStates (SM.StateMachineInstance _ si) am =
    let refMap = fromMaybe Map.empty $ am ^. at (Scripts.scriptAddress si)
        lkp (ref, out) = do
            tref <- Typed.typeScriptTxOutRef (\r -> Map.lookup r refMap) si ref
            tout <- Typed.typeScriptTxOut si out
            pure (tout, tref)
    in rights $ fmap lkp $ Map.toList refMap
