{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass     #-}
-- | A type for talking about values that may be under- or overspecified.
module Ledger.Specificity(Specificity(..)) where

import qualified Data.Aeson                as Aeson
import           Data.Text.Prettyprint.Doc
import           GHC.Generics              (Generic)
import           IOTS                      (IotsType)

import           Language.PlutusTx.Prelude

import qualified Prelude                   as Haskell

-- | Information about a value of type a.
data Specificity a =
    Unspecified
    -- ^ No information about the value
    | Exactly a
    -- ^ The value is known exactly
    | Overspecified
    -- ^ Too much (ie. conflicting) information about a
    deriving stock (Haskell.Eq, Generic, Haskell.Functor)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON, IotsType)

instance Eq a => Eq (Specificity a) where
    Unspecified == Unspecified     = True
    Exactly l == Exactly r         = l == r
    Overspecified == Overspecified = True
    _ == _ = False

instance Pretty a => Pretty (Specificity a) where
    pretty Unspecified   = "(unspecified)"
    pretty Overspecified = "(overspecified)"
    pretty (Exactly a)   = pretty a

instance Eq a => JoinSemiLattice (Specificity a) where
    Unspecified \/ _ = Unspecified
    _ \/ Unspecified = Unspecified
    Exactly a \/ Exactly a'
      | a == a' = Exactly a
      | True    = Unspecified
    Overspecified \/ r = r
    l \/ Overspecified = l
  
instance Eq a => BoundedJoinSemiLattice (Specificity a) where
  bottom = Unspecified

instance Eq a => MeetSemiLattice (Specificity a) where
    Unspecified /\ a = a
    a /\ Unspecified = a
    Exactly a /\ Exactly a'
        | a == a'   = Exactly a
        | True       = Overspecified
    _ /\ _           = Overspecified

instance Eq a => BoundedMeetSemiLattice (Specificity a) where
  top = Overspecified

instance Eq a => Semigroup (Specificity a) where
    (<>) = (/\)

instance Eq a => Monoid (Specificity a) where
    mempty = Unspecified
