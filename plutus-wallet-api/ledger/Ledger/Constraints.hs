{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
-- | Constraints for transactions
module Ledger.Constraints(
    TxConstraints(..)
    , PendingTxConstraints
    , LedgerTxConstraints
    -- * Defining constraints
    , payToScript
    , payToPubKey
    , forgeValue
    , moveValue
    , mustBeValidIn
    , mustBeSignedBy
    , mustSpendInput
    , mustProduceOutput
    , mustPayToOwnAddress
    , mustIncludeDataValue
    -- * Queries
    , valueMoved
    , requiredSignatures
    , validityRange
    , checkPendingTx
    , modifiesUtxoSet
    , hasValidTx
    -- * Ledger transactions
    , toLedgerTx
    , fromLedgerTx
    , toLedgerConstraints
    ) where

import qualified Data.Aeson                as Aeson
import qualified Data.Map                  as Map
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text.Prettyprint.Doc hiding ((<>))
import           GHC.Generics              (Generic)
import           IOTS                      (IotsType)

import qualified Language.PlutusTx         as PlutusTx
import           Language.PlutusTx.Prelude

import           Ledger.Address            (Address)
import           Ledger.Crypto             (PubKey)
import           Ledger.Interval           (contains, isEmpty)
import           Ledger.Scripts            (DataValue (..), dataValueHash)
import           Ledger.Slot               (SlotRange)
import           Ledger.Tx                 (Tx (..))
import qualified Ledger.Tx                 as LTx
import           Ledger.Typed.Scripts
import           Ledger.Validation         (PendingTx, PendingTx' (..), TxOut (..))
import qualified Ledger.Validation         as V
import           Ledger.Value              (Value, isZero, leq)

import qualified Prelude                   as Haskell

type PendingTxConstraints = TxConstraints () ValueAllocation
type LedgerTxConstraints  = TxConstraints (Set LTx.TxIn) [TxOut]

data ValueAllocation =
    ValueAllocation
        { vaOwnAddress    :: Value
        , vaOtherPayments :: [TxOut]
        } deriving stock (Haskell.Eq, Generic)
          deriving anyclass (Aeson.FromJSON, Aeson.ToJSON, IotsType)

instance Semigroup ValueAllocation where
    l <> r =
        ValueAllocation
            { vaOwnAddress = vaOwnAddress l <> vaOwnAddress r
            , vaOtherPayments = vaOtherPayments l <> vaOtherPayments r
            }

instance Haskell.Semigroup ValueAllocation where
    (<>) = (<>)

instance Monoid ValueAllocation where
    mempty = ValueAllocation mempty mempty

instance Haskell.Monoid ValueAllocation where
    mempty = mempty

instance Pretty ValueAllocation where
    pretty ValueAllocation{vaOwnAddress, vaOtherPayments} =
        let renderOutput LTx.TxOut{LTx.txOutType, LTx.txOutValue} =
                hang 2 $ vsep ["-" <+> pretty txOutValue <+> "locked by", pretty txOutType]
        in
        vsep
            [ hang 2 (vsep ["paid to own address:",  pretty vaOwnAddress])
            , hang 2 (vsep ("other payments:" : fmap renderOutput vaOtherPayments))
            ]

-- | Restrictions placed on the allocation of funds to outputs of the state
--   machine transition transaction.
data TxConstraints i o =
    TxConstraints
        { tcOutputs            :: o
        , tcInputs             :: i
        , tcDataValues         :: [DataValue] -- TODO: Should be [DataValueHash] for Eq comparison??
        -- ^ Data values to be included in the spending transaction
        , tcForge              :: Value
        -- ^ How much value is to be forged
        , tcInterval           :: SlotRange
        -- ^ Validity interval of this 'TxConstraints'
        , tcRequiredSignatures :: [PubKey]
        -- ^ Signatories of the transaction
        , tcValueMoved         :: Value
        -- ^ The minimum size of the transaction's left and right side. The
        --   purpose of this field is to enable proof of ownership for tokens
        --   (a transaction proves ownership of a token if the value consumed
        --   and spent by it includes the token. The value in the '_valueMoved'
        --   field will be paid from the wallet's own funds back to an address
        --   owned by the wallet)
        } deriving stock (Haskell.Functor, Haskell.Eq, Haskell.Show, Generic)
          deriving anyclass (Aeson.FromJSON, Aeson.ToJSON, IotsType)

instance (Semigroup o, Semigroup i) => Semigroup (TxConstraints i o) where
    l <> r =
        TxConstraints
            { tcOutputs = tcOutputs l <> tcOutputs r
            , tcDataValues = tcDataValues l <> tcDataValues r
            , tcForge = tcForge l <> tcForge r
            , tcInterval = tcInterval l /\ tcInterval r
            , tcRequiredSignatures = tcRequiredSignatures l <> tcRequiredSignatures r
            , tcValueMoved = tcValueMoved l <> tcValueMoved r
            , tcInputs = tcInputs l <> tcInputs r
            }

instance (Haskell.Semigroup o, Haskell.Semigroup i) => Haskell.Semigroup (TxConstraints i o) where
    l <> r =
        TxConstraints
            { tcOutputs = tcOutputs l Haskell.<> tcOutputs r
            , tcDataValues = tcDataValues l Haskell.<> tcDataValues r
            , tcForge = tcForge l Haskell.<> tcForge r
            , tcInterval = tcInterval l /\ tcInterval r
            , tcRequiredSignatures = tcRequiredSignatures l Haskell.<> tcRequiredSignatures r
            , tcValueMoved = tcValueMoved l Haskell.<> tcValueMoved r
            , tcInputs = tcInputs l Haskell.<> tcInputs r
            }

instance (Monoid i, Monoid o) => Monoid (TxConstraints i o) where
    mempty  = TxConstraints mempty mempty mempty mempty top mempty mempty

instance (Haskell.Monoid o, Haskell.Monoid i) => Haskell.Monoid (TxConstraints i o) where
    mappend = (Haskell.<>)
    mempty  = TxConstraints Haskell.mempty Haskell.mempty Haskell.mempty Haskell.mempty top Haskell.mempty Haskell.mempty

instance (Eq o, Eq i) => Eq (TxConstraints i o) where
    l == r =
        tcOutputs l == tcOutputs r
        && tcInputs l == tcInputs r
        && tcDataValues l == tcDataValues r
        && tcForge l == tcForge r
        && tcInterval l == tcInterval r
        && tcRequiredSignatures l == tcRequiredSignatures r
        && tcValueMoved l == tcValueMoved r


instance (Pretty i, Pretty o) => Pretty (TxConstraints i o) where
    pretty TxConstraints{tcOutputs, tcDataValues, tcForge, tcInterval, tcRequiredSignatures, tcValueMoved, tcInputs} =
        vsep
            [ hang 2 (vsep ["inputs:", pretty tcInputs])
            , hang 2 (vsep ["outputs:", pretty tcOutputs])
            , hang 2 (vsep ("data values:" : fmap pretty tcDataValues))
            , hang 2 (vsep ["value forged:", pretty tcForge])
            , hang 2 (vsep ["validity range:", viaShow tcInterval])
            , hang 2 (vsep ("required signatures:" : fmap pretty tcRequiredSignatures))
            , hang 2 (vsep ["value moved:", pretty tcValueMoved])
            ]

valueMoved :: TxConstraints i o -> Value
valueMoved = tcValueMoved

requiredSignatures :: TxConstraints i o -> [PubKey]
requiredSignatures = tcRequiredSignatures

validityRange :: TxConstraints i o -> SlotRange
validityRange = tcInterval

{-# INLINABLE mustBeValidIn #-}
-- | @mustBeValidIn r@ requires the transaction's slot range to be contained
--   in @r@.
mustBeValidIn :: (Monoid i, Monoid o) => SlotRange -> TxConstraints i o
mustBeValidIn range = mempty { tcInterval = range }

{-# INLINABLE mustBeSignedBy #-}
-- | Require the transaction to be signed by the public key.
mustBeSignedBy :: (Monoid i, Monoid o) => PubKey -> TxConstraints i o
mustBeSignedBy pk = mempty { tcRequiredSignatures = [pk] }

{-# INLINABLE mustSpendInput #-}
-- | Require the transaction to spend the input
mustSpendInput :: LTx.TxIn -> LedgerTxConstraints
mustSpendInput i = (Haskell.mempty @LedgerTxConstraints) { tcInputs = Set.singleton i }

{-# INLINABLE mustProduceOutput #-}
-- | Require the transaction to produce the ouptut
mustProduceOutput :: TxOut -> PendingTxConstraints
mustProduceOutput o = (mempty @PendingTxConstraints) { tcOutputs = mempty { vaOtherPayments = [o] }  }

{-# INLINABLE mustPayToOwnAddress #-}
-- | Require the transaction to pay the value to the contract's own
--   address
mustPayToOwnAddress :: Value -> PendingTxConstraints
mustPayToOwnAddress vl = (mempty @PendingTxConstraints) { tcOutputs = mempty { vaOwnAddress = vl }}

{-# INLINABLE mustIncludeDataValue #-}
-- | Require the transaction to include a data value
mustIncludeDataValue :: (Monoid i, Monoid o) => DataValue -> TxConstraints i o
mustIncludeDataValue dv = mempty { tcDataValues = [dv] }

{-# INLINABLE payToScript #-}
-- | Lock the value with a script
payToScript :: Value -> Address -> DataValue -> PendingTxConstraints
payToScript v a ds = (mempty @PendingTxConstraints) { tcOutputs = mempty { vaOtherPayments = [outp] },  tcDataValues = [ds] } where
    outp = LTx.scriptTxOut' v a ds

{-# INLINABLE payToPubKey #-}
-- | Lock the value with a public key
payToPubKey :: Value -> PubKey -> PendingTxConstraints
payToPubKey v pk = (mempty @PendingTxConstraints) { tcOutputs = mempty { vaOtherPayments = [outp] } } where
    outp = LTx.pubKeyTxOut v pk

-- This just sets the 'tcForge' field, but it's exported for convenient
-- use with '<>'.
-- | Create the given value
forgeValue :: (Monoid i, Monoid o) => Value -> TxConstraints i o
forgeValue vl = mempty { tcForge = vl }

-- | Requirement to move (produce and/or spend) the given value
moveValue :: (Monoid i, Monoid o) =>Value -> TxConstraints i o
moveValue vl = mempty { tcValueMoved = vl }

{-# INLINABLE checkPendingTx #-}
-- | Does the 'PendingTx' satisfy the constraints?
checkPendingTx :: PendingTxConstraints -> PendingTx -> Bool
checkPendingTx TxConstraints{tcOutputs, tcDataValues, tcForge, tcInterval, tcRequiredSignatures, tcValueMoved} ptx =
    let outputsOK =
            let ValueAllocation{vaOwnAddress, vaOtherPayments} = tcOutputs
            in
                foldMap V.txOutValue (V.getContinuingOutputs ptx) == vaOwnAddress
                && all (`elem` pendingTxOutputs ptx) vaOtherPayments
        dataValuesOK =
            all (`elem` fmap snd (pendingTxData ptx)) tcDataValues
        forgeOK =
            tcForge == pendingTxForge ptx
        valueMovedOK = tcValueMoved `leq` V.valueSpent ptx
        intervalOK =
            -- the pending transaction's validation interval is at least
            -- as specific (as small) as 'tcInterval'
            tcInterval `contains` pendingTxValidRange ptx
        signaturesOK =
            all (V.txSignedBy ptx) tcRequiredSignatures
    in traceIfFalseH "checkPendingTx failed - outputs not OK" outputsOK
        && traceIfFalseH "checkPendingTx failed - data values not OK" dataValuesOK
        && traceIfFalseH "checkPendingTx failed - forge not OK" forgeOK
        && traceIfFalseH "checkPendingTx failed - value moved not OK" valueMovedOK
        && traceIfFalseH "checkPendingTx failed - interval not OK" intervalOK
        && traceIfFalseH "checkPendingTx failed - signatures missing" signaturesOK

{-# INLINABLE hasValidTx #-}
-- | Is there a valid transaction that satisfies the constraints? (ignoring
--   the inputs)
hasValidTx :: TxConstraints i o -> Bool
hasValidTx TxConstraints{tcInterval} = not (isEmpty tcInterval)

-- | A ledger transaction that satisfies the constraints (unbalanced and
--   unsigned)
toLedgerTx :: LedgerTxConstraints -> Tx
toLedgerTx TxConstraints{tcInputs, tcOutputs, tcForge, tcInterval, tcDataValues} =
    Tx
        { txInputs = tcInputs
        , txOutputs = tcOutputs
        , txForge = tcForge
        , txFee = mempty
        , txValidRange = tcInterval
        , txSignatures = Map.empty
        , txData = Map.fromList $ fmap (\ds -> (dataValueHash ds, ds)) tcDataValues
        }

-- | Constraints that are satisfied by the given ledger transaction
fromLedgerTx :: Tx -> LedgerTxConstraints
fromLedgerTx Tx{txInputs, txOutputs, txForge, txValidRange, txSignatures, txData} =
    TxConstraints
        { tcInputs = txInputs
        , tcOutputs = txOutputs
        , tcForge = txForge
        , tcInterval = txValidRange
        , tcRequiredSignatures = Set.toList (Map.keysSet txSignatures)
        , tcDataValues = snd <$> Map.toList txData
        , tcValueMoved = mempty
        }

-- | Can the constraints be satisfied by a transaction with no
--   inputs and outputs?
modifiesUtxoSet :: LedgerTxConstraints -> Bool
modifiesUtxoSet TxConstraints{tcForge, tcOutputs, tcValueMoved} =
    not (isZero tcForge)
    || not (isZero (foldMap txOutValue tcOutputs))
    || not (isZero tcValueMoved)

-- | Then an on-chain 'PendingTxConstraints' value into an (off-chain)
--   'LedgerTxConstraints' value, using the validator and the data value
--   for the output at the 'PendingTxConstraints'' "own address"
toLedgerConstraints :: PendingTxConstraints -> Validator -> DataValue -> LedgerTxConstraints
toLedgerConstraints txc val dv =
    let ValueAllocation{vaOtherPayments, vaOwnAddress} = tcOutputs txc
    in txc
        { tcInputs = Set.empty
        , tcOutputs =
            if isZero vaOwnAddress
                then vaOtherPayments
                else LTx.scriptTxOut vaOwnAddress val dv : vaOtherPayments
        }

PlutusTx.makeIsData ''TxConstraints
PlutusTx.makeLift ''TxConstraints
